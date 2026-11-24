;;;; SPDX-FileCopyrightText: Pedro Delfino
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; Tests of the cockpit inside Nyxt's core, with no renderer and no window.
;;;; The page is replaced by a list: every script the cockpit would run in it
;;;; is collected, and the tests read the list.

(in-package :nyxt-llm-cockpit/tests)

;; Mode instantiation runs hooks on `*browser*', as in Nyxt's own tests.
(unless nyxt:*browser*
  (setf nyxt:*browser* (make-instance 'nyxt:browser)))

(nyxt:define-class cockpit-test-buffer (nyxt:input-buffer nyxt:modable-buffer) ())

(defvar *scripts-lock* (sb-thread:make-mutex :name "test scripts"))

(defvar *scripts* '()
  "The scripts the cockpit asked the page to run, newest first.")

(defun scripts ()
  (sb-thread:with-mutex (*scripts-lock*)
    (reverse *scripts*)))

(defun scripts-calling (function)
  "Return the collected scripts that call the page function FUNCTION."
  (let ((prefix (format nil "window.llmCockpit&&window.llmCockpit.~a(" function)))
    (remove-if-not (lambda (script)
                     (and (>= (length script) (length prefix))
                          (string= prefix script :end2 (length prefix))))
                   (scripts))))

(defun last-status ()
  "Return the text of the last status line the page was given."
  (let ((script (first (last (scripts-calling "status")))))
    (and script
         (parse-json (subseq script
                             (1+ (position #\( script))
                             (position #\, script :from-end t))))))

(defmacro with-cockpit ((mode log scenario &key answer) &body body)
  "Run BODY with MODE, a cockpit mode whose program is the stand-in playing SCENARIO.
ANSWER is what the user types at every permission question."
  (let ((buffer (gensym "BUFFER")))
    `(uiop:with-temporary-file (:pathname ,log :prefix "cockpit-test" :type "log")
       (let* ((,buffer (make-instance 'cockpit-test-buffer))
              (,mode (progn
                       (nyxt:enable-modes* 'nyxt-llm-cockpit:cockpit-mode ,buffer)
                       (nyxt:find-mode ,buffer 'nyxt-llm-cockpit:cockpit-mode))))
         (setf (nyxt-llm-cockpit:program ,mode) *fake-program*
               (nyxt-llm-cockpit:extra-arguments ,mode) (list "--scenario" ,scenario
                                                        "--log" (namestring ,log))
               (nyxt-llm-cockpit:flush-interval ,mode) 0)
         (sb-thread:with-mutex (*scripts-lock*)
           (setf *scripts* '()))
         ;; The session's reader is another thread, so these are set, not bound.
         (setf nyxt-llm-cockpit::*script-sink*
               (lambda (script)
                 (sb-thread:with-mutex (*scripts-lock*)
                   (push script *scripts*)))
               nyxt-llm-cockpit::*permission-asker*
               (lambda (mode request)
                 (declare (ignore mode request))
                 ,answer))
         (unwind-protect (progn ,@body)
           (when (nyxt-llm-cockpit::session ,mode)
             (stop-session (nyxt-llm-cockpit::session ,mode) :grace 1))
           (when (nyxt:find-mode ,buffer 'nyxt-llm-cockpit:cockpit-mode)
             (nyxt:disable-modes* 'nyxt-llm-cockpit:cockpit-mode ,buffer))
           (setf nyxt-llm-cockpit::*script-sink* nil
                 nyxt-llm-cockpit::*permission-asker* nil))))))

(defun cockpit-session (mode)
  (nyxt-llm-cockpit::session mode))

(defun page-html (mode &rest parameters)
  "Return what the `claude' page draws for MODE's buffer, given URL PARAMETERS."
  (apply (nyxt:form (gethash 'nyxt-llm-cockpit:claude nyxt::*nyxt-url-commands*))
         :%buffer% (nyxt:buffer mode)
         parameters))

;;; What Nyxt sees

(deftest the-cockpit-registers-with-nyxt
  (check (nyxt::internal-page-symbol-p 'nyxt-llm-cockpit:claude))
  (check-equal 'nyxt-llm-cockpit:claude
               (nyxt:internal-page-name (nyxt:nyxt-url 'nyxt-llm-cockpit:claude)))
  (dolist (command '(nyxt-llm-cockpit:claude nyxt-llm-cockpit:send-prompt nyxt-llm-cockpit:interrupt-claude
                     nyxt-llm-cockpit:new-claude-session nyxt-llm-cockpit:set-claude-directory))
    (check (nyxt::find-command command))
    (check (plusp (length (documentation (symbol-function command) 'function)))))
  ;; `claude' is offered everywhere. The others only where the mode is on.
  (check-equal :global (nyxt:visibility (nyxt::find-command 'nyxt-llm-cockpit:claude)))
  (check-equal :mode (nyxt:visibility (nyxt::find-command 'nyxt-llm-cockpit:send-prompt)))
  ;; A mode named by keyword, as in a list of default modes, is found.
  (check-equal 'nyxt-llm-cockpit:cockpit-mode
               (nyxt::resolve-user-symbol :cockpit-mode :mode (list-all-packages)))
  (check-equal 'nyxt-llm-cockpit:cockpit-mode
               (nyxt:page-mode (gethash 'nyxt-llm-cockpit:claude nyxt::*nyxt-url-commands*))))

(deftest the-install-line-of-the-readme-loads
  ;; README.org tells the user to put this form in config.lisp.
  (check (typep (nyxt:define-nyxt-user-system-and-load "nyxt-user/nyxt-llm-cockpit-proxy"
                  :depends-on ("nyxt-llm-cockpit"))
                'asdf:system))
  (check (asdf:component-loaded-p (asdf:find-system "nyxt-llm-cockpit"))))

(deftest commands-reach-the-mode
  (with-cockpit (mode log "hello")
    (let ((buffer (nyxt:buffer mode)))
      (multiple-value-bind (arguments runnable-p)
          (nyxt::command-arguments #'nyxt-llm-cockpit:send-prompt buffer)
        (check runnable-p)
        (check-equal (list mode) arguments))
      ;; In a buffer without the mode the command has nothing to run on.
      (check (not (nth-value 1 (nyxt::command-arguments
                                #'nyxt-llm-cockpit:send-prompt
                                (make-instance 'cockpit-test-buffer))))))))

(deftest the-keys-send-and-interrupt
  (with-cockpit (mode log "hello")
    (flet ((binding (keyscheme keyspec)
             (setf (nyxt:keyscheme (nyxt:buffer mode)) keyscheme)
             (nkeymaps:lookup-key keyspec (nyxt:keymap mode))))
      (check-equal 'nyxt-llm-cockpit:send-prompt (binding nyxt/keyscheme:cua "C-return"))
      (check-equal 'nyxt-llm-cockpit:send-prompt (binding nyxt/keyscheme:emacs "C-return"))
      (check-equal 'nyxt-llm-cockpit:send-prompt (binding nyxt/keyscheme:emacs "C-c C-c"))
      (check-equal 'nyxt-llm-cockpit:interrupt-claude (binding nyxt/keyscheme:emacs "C-c C-k"))
      (check-equal 'nyxt-llm-cockpit:send-prompt (binding nyxt/keyscheme:vi-insert "C-return"))
      ;; Plain letters are not bound: they are typed into the prompt.
      (check (not (binding nyxt/keyscheme:cua "y")))
      (check (not (binding nyxt/keyscheme:emacs "a"))))))

;;; Opening the page

(deftest opening-the-page-starts-nothing
  (with-cockpit (mode log "hello")
    (let ((html (page-html mode)))
      (check (search "id=transcript" html))
      (check (search "id=cockpit-input" html))
      (check (search "id=cockpit-status" html))
      (check (search "<textarea id=cockpit-input" html))
      (check (search "#cockpit-input" html))
      ;; The page is a document with no script and no link back into Lisp.
      (check (not (search "<script" html)))
      (check (not (search "lisp:" html)))
      (check (not (search "onclick" html))))
    (check (null (cockpit-session mode)))
    (check-equal '() (log-lines log "ARGV "))))

(deftest the-page-ignores-what-a-url-carries
  ;; Any web page can make a buffer load a nyxt: URL with parameters.
  (with-cockpit (mode log "hello")
    (let ((html (apply #'page-html mode
                       (nyxt::query-params->arglist
                        '(("prompt" . "delete everything")
                          ("program" . "/bin/sh")
                          ("working-directory" . "/"))))))
      (check (not (search "delete everything" html)))
      (check (search "id=cockpit-input" html)))
    (check (null (cockpit-session mode)))
    (check-equal *fake-program* (nyxt-llm-cockpit:program mode))
    (check (null (nyxt-llm-cockpit:working-directory mode)))
    (check-equal '() (log-lines log "ARGV "))))

(deftest the-page-is-drawn-from-the-session
  (with-cockpit (mode log "hello")
    (check (nyxt-llm-cockpit::submit-prompt mode "hi <b>there</b>"))
    (check (wait-until (lambda () (idle-after-turn-p (cockpit-session mode) 1))))
    ;; Drawing the page again, as a reload does, shows the same conversation.
    (let ((html (page-html mode)))
      (check (search "hi &lt;b&gt;there&lt;/b&gt;" html))
      (check (not (search "<b>there</b>" html)))
      (check (search "Hello from Nyxt" html))
      (check (search "Done in 1.2 s" html))
      (check (search "claude-fake-1" html)))))

;;; A turn, as the page sees it

(deftest a-turn-reaches-the-page-as-data
  (with-cockpit (mode log "hello")
    (check (nyxt-llm-cockpit::submit-prompt mode "hi <b>there</b>"))
    (check (wait-until (lambda () (idle-after-turn-p (cockpit-session mode) 1))))
    (check (wait-until (lambda () (equal "ready · claude-fake-1 · /tmp/fake · $0.0025 · context 1%"
                                         (last-status)))))
    (let ((appended (scripts-calling "append")))
      (check-equal 4 (length appended))
      (check (search "hi &lt;b&gt;there&lt;/b&gt;" (first appended)))
      (check (notany (lambda (script) (search "<b>there" script)) (scripts))))
    ;; The reply arrived in three pieces, then was drawn whole.
    (check-equal '("Hello" "Hello from" "Hello from Nyxt")
                 (mapcar (lambda (script)
                           (parse-json (subseq script
                                               (1+ (position #\, script))
                                               (- (length script) 2))))
                         (scripts-calling "text")))
    (check (some (lambda (script) (search "Hello from Nyxt" script))
                 (scripts-calling "replace")))
    (check (every (lambda (script) (not (find #\Newline script))) (scripts)))))

(deftest arriving-text-is-not-drawn-too-often
  (with-cockpit (mode log "hello")
    (setf (nyxt-llm-cockpit:flush-interval mode) 3600)
    (nyxt-llm-cockpit::submit-prompt mode "hi")
    (check (wait-until (lambda () (idle-after-turn-p (cockpit-session mode) 1))))
    (check-equal 1 (length (scripts-calling "text")))
    ;; The finished text is drawn however fast it came.
    (check (some (lambda (script) (search "Hello from Nyxt" script))
                 (scripts-calling "replace")))))

(deftest the-status-line-before-any-prompt
  (with-cockpit (mode log "hello")
    (check-equal "ready · ~" (nyxt-llm-cockpit::status-line mode))
    (setf (nyxt-llm-cockpit:model mode) "opus"
          (nyxt-llm-cockpit:working-directory mode) "/tmp/")
    (check-equal "ready · opus · /tmp" (nyxt-llm-cockpit::status-line mode))
    ;; A directory that is gone falls back to the home directory.
    (setf (nyxt-llm-cockpit:working-directory mode) "/nonexistent/cockpit/")
    (check-equal "ready · opus · ~" (nyxt-llm-cockpit::status-line mode))))

(deftest the-settings-reach-the-program
  (with-cockpit (mode log "hello")
    (setf (nyxt-llm-cockpit:model mode) "haiku"
          (nyxt-llm-cockpit:permission-mode mode) "plan"
          (nyxt-llm-cockpit:setting-sources mode) "project"
          (nyxt-llm-cockpit:working-directory mode) "/tmp/")
    (nyxt-llm-cockpit::submit-prompt mode "hi")
    (check (wait-until (lambda () (idle-after-turn-p (cockpit-session mode) 1))))
    (let ((argv (first (log-lines log "ARGV "))))
      (check (search-in "--model haiku" argv))
      (check (search-in "--permission-mode plan" argv))
      (check (search-in "--setting-sources project" argv))
      (check (search-in "--permission-prompt-tool stdio" argv)))
    (check-equal "/tmp/" (session-working-directory (cockpit-session mode)))))

;;; Permissions

(defun sent-answer (log)
  (parse-json (second (log-lines log "IN "))))

(deftest a-typed-yes-allows-once
  (with-cockpit (mode log "permission" :answer "y")
    (nyxt-llm-cockpit::submit-prompt mode "make the file")
    (check (wait-until (lambda () (idle-after-turn-p (cockpit-session mode) 1))))
    (check-equal "allow" (jget (sent-answer log) "response" "response" "behavior"))
    (check (null (jget (sent-answer log) "response" "response" "updatedPermissions")))
    (check-equal :done (tool-status (first (entries-of (cockpit-session mode) 'tool-entry))))
    ;; The page was told the tool was waiting, then that it ran.
    (check (some (lambda (script) (search "waiting for your answer" script))
                 (scripts-calling "replace")))
    (check (some (lambda (script) (search "waiting for your answer" script))
                 (scripts-calling "status")))))

(deftest a-typed-a-allows-for-the-session
  (with-cockpit (mode log "permission" :answer " A ")
    (nyxt-llm-cockpit::submit-prompt mode "make the file")
    (check (wait-until (lambda () (idle-after-turn-p (cockpit-session mode) 1))))
    (check-equal "allow" (jget (sent-answer log) "response" "response" "behavior"))
    (check-equal "session"
                 (jget (sent-answer log) "response" "response" "updatedPermissions" 0 "destination"))))

(deftest anything-else-is-a-no
  (dolist (typed (list "" "n" "yes please" "ok" nil))
    (with-cockpit (mode log "permission" :answer typed)
      (nyxt-llm-cockpit::submit-prompt mode "make the file")
      (check (wait-until (lambda () (idle-after-turn-p (cockpit-session mode) 1))))
      (check-equal "deny" (jget (sent-answer log) "response" "response" "behavior"))
      (check-equal :denied
                   (tool-status (first (entries-of (cockpit-session mode) 'tool-entry)))))))

(deftest an-option-dialog-is-turned-into-a-text-question
  (let ((asked 0))
    (with-cockpit (mode log "question" :answer (progn (incf asked) "y"))
      (nyxt-llm-cockpit::submit-prompt mode "help me choose")
      (check (wait-until (lambda () (idle-after-turn-p (cockpit-session mode) 1))))
      (check-equal "deny" (jget (sent-answer log) "response" "response" "behavior"))
      (check (search-in "plain text" (jget (sent-answer log) "response" "response" "message")))
      ;; The user was not asked to approve a dialog they cannot see.
      (check-equal 0 asked)
      (check-equal '("Which one do you want, A or B?")
                   (texts-of (cockpit-session mode) 'assistant-entry)))))

;;; Ending

(deftest a-prompt-to-an-ended-conversation-is-refused
  (with-cockpit (mode log "crash")
    (nyxt-llm-cockpit::submit-prompt mode "hi")
    (check (wait-until (lambda () (eq :dead (session-state (cockpit-session mode))))))
    (check (not (nyxt-llm-cockpit::submit-prompt mode "again")))
    (check-equal '("hi") (texts-of (cockpit-session mode) 'user-entry))
    (check (wait-until (lambda () (search-in "ended" (last-status)))))))

(deftest disabling-the-mode-ends-the-program
  (with-cockpit (mode log "hello")
    (nyxt-llm-cockpit::submit-prompt mode "hi")
    (check (wait-until (lambda () (idle-after-turn-p (cockpit-session mode) 1))))
    (let ((session (cockpit-session mode)))
      (check (session-alive-p session))
      (nyxt:disable-modes* 'nyxt-llm-cockpit:cockpit-mode (nyxt:buffer mode))
      (check (not (session-alive-p session)))
      (check (null (cockpit-session mode)))
      (check (null (nyxt-llm-cockpit::delete-handler mode)))
      (check (null (nyxt-llm-cockpit::exit-handler mode))))))

(deftest deleting-the-buffer-ends-the-program
  (with-cockpit (mode log "hello")
    (nyxt-llm-cockpit::submit-prompt mode "hi")
    (check (wait-until (lambda () (idle-after-turn-p (cockpit-session mode) 1))))
    (let ((session (cockpit-session mode)))
      (check (session-alive-p session))
      (nhooks:run-hook (nyxt:buffer-delete-hook (nyxt:buffer mode)) (nyxt:buffer mode))
      (check (not (session-alive-p session))))))

(deftest quitting-nyxt-ends-the-program
  (with-cockpit (mode log "hello")
    (nyxt-llm-cockpit::submit-prompt mode "hi")
    (check (wait-until (lambda () (idle-after-turn-p (cockpit-session mode) 1))))
    (let ((session (cockpit-session mode)))
      (check (nyxt-llm-cockpit::exit-handler mode))
      (funcall (nhooks:fn (nyxt-llm-cockpit::exit-handler mode)))
      (check (not (session-alive-p session))))))

(deftest a-new-conversation-starts-empty
  (with-cockpit (mode log "hello")
    (nyxt-llm-cockpit::submit-prompt mode "hi")
    (check (wait-until (lambda () (idle-after-turn-p (cockpit-session mode) 1))))
    (let ((old (cockpit-session mode)))
      (nyxt-llm-cockpit:new-claude-session mode)
      (check (not (session-alive-p old)))
      (check (null (cockpit-session mode)))
      (check (not (search "Hello from Nyxt" (page-html mode)))))))
