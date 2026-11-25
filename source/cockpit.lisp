;;;; SPDX-FileCopyrightText: Pedro Delfino
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; The cockpit: a Nyxt buffer where you talk to a coding agent.
;;;;
;;;; The conversation is an internal page with a page mode, the way
;;;; `list-messages' and `terminal' are. The session lives in the mode, so the
;;;; page can be drawn again at any time without losing anything.
;;;;
;;;; Two rules shape this file.
;;;;
;;;; 1. Lisp talks to the page, the page never talks to Lisp. The page shows
;;;;    text nobody here wrote: what a model said, what a tool returned. So it
;;;;    is given nothing to call: no callbacks, no click handlers, no links
;;;;    into Lisp. The prompt is read by asking the page for it, and commands
;;;;    run from keys.
;;;;
;;;; 2. A tool runs only after an answer the user typed. The question is asked
;;;;    in the prompt buffer as free text. A typed answer comes from the
;;;;    keyboard and from nowhere else.

(nyxt:define-package :nyxt-llm-cockpit
  (:documentation "An LLM cockpit for Nyxt: talk to a coding agent from a buffer.

The first agent it drives is Claude Code. Run the `claude' command to open the
cockpit. Type a prompt and send it with `send-prompt'. Claude Code runs as its
own program, with your login and your settings, and this package draws what it
says and asks you before it runs a tool.

The commands:
- `claude' opens the cockpit.
- `send-prompt' sends what is typed.
- `interrupt-claude' stops the turn that is running.
- `new-claude-session' ends the conversation and starts an empty one.
- `set-claude-directory' chooses the directory Claude Code works in.

See the `cockpit-mode' slots for what can be configured."))
(in-package :nyxt-llm-cockpit)

(defvar *script-sink* nil
  "When a function, it is called with each script instead of running it in the page.
Tests bind it to see what the page would be told.")

(defvar *permission-asker* nil
  "When a function, it is called with a mode and a request instead of the prompt buffer.
It returns what the user would have typed. Tests bind it.")

(defvar *prompt-taker* nil
  "When a function, it is called with a mode instead of asking the page for the prompt.
It returns what would be typed in the box. Tests bind it.")

(define-mode cockpit-mode ()
  "Talk to Claude Code in this buffer.
The conversation is drawn as a document: what you sent, what Claude said, the
tools it asked for and what they returned. Claude Code runs as a separate
program with your own login and settings.

Return sends what is typed. Shift-Return starts a new line.

Before a tool runs, the prompt buffer asks. Type y to allow it once, or a to
allow the same thing for the rest of the session. Anything else is no."
  ((visible-in-status-p t)
   (program
    "claude"
    :type string
    :documentation "The Claude Code program: a name or a path.
A name is looked up on PATH, then in ~/.local/bin and the other usual places,
because an application started from the Dock has a short PATH.")
   (model
    nil
    :type (maybe string)
    :documentation "The model to use, such as \"opus\" or \"sonnet\".
NIL leaves the choice to Claude Code's own settings.")
   (permission-mode
    nil
    :type (maybe string)
    :documentation "The permission mode, such as \"plan\" or \"acceptEdits\".
NIL leaves the choice to Claude Code's own settings.")
   (setting-sources
    nil
    :type (maybe string)
    :documentation "Which settings files Claude Code loads, such as \"user,project\".
NIL loads what it loads in a terminal: your hooks and allow rules apply.")
   (extra-arguments
    '()
    :type list
    :documentation "More command line arguments for the program, as strings.")
   (working-directory
    nil
    :type (maybe string)
    :documentation "The directory Claude Code works in.
NIL is the home directory. `set-claude-directory' changes it.")
   (flush-interval
    0.05
    :type real
    :documentation "The least time in seconds between two redraws of text that is arriving.")
   (session
    nil
    :type t
    :export nil
    :documentation "The conversation, or NIL before the first prompt.")
   (listener
    nil
    :type t
    :export nil
    :documentation "The function the session calls when it changes.")
   (last-flush
    nil
    :type (maybe integer)
    :export nil
    :documentation "When arriving text was last drawn, in internal time units.
NIL before the first draw.")
   (question-lock
    (bt:make-lock "cockpit question")
    :type t
    :export nil
    :documentation "Held while the user is asked, so questions come one at a time.")
   (delete-handler
    nil
    :type t
    :export nil
    :documentation "Hook that ends the program when the buffer is deleted.")
   (exit-handler
    nil
    :type t
    :export nil
    :documentation "Hook that ends the program when Nyxt quits.")
   (keyscheme-map
    (define-keyscheme-map "cockpit-mode" ()
      ;; Return sends, as it does in Claude Code and in every chat box. Nyxt
      ;; takes a bound key before the page sees it, and it does not fall back
      ;; from an unbound key with a modifier to the plain one. So Shift-Return
      ;; and Option-Return reach the box and start a new line there.
      ;; In Nyxt's notation "S-" is super, the Command key on a Mac, and "s-"
      ;; is shift. So "S-return" is Command-Return, and "s-return" stays unbound.
      keyscheme:default
      (list
       "return" 'send-prompt
       "C-return" 'send-prompt
       "S-return" 'send-prompt)
      keyscheme:emacs
      (list
       "C-c C-c" 'send-prompt
       "C-c C-k" 'interrupt-claude)
      ;; Nothing is bound for vi's insert state: there Nyxt hands every key to
      ;; the page, whatever a mode binds. With vi keys a prompt is sent from
      ;; the normal state, which takes the keys of the default scheme.
      keyscheme:vi-normal
      (list
       "C-c" 'interrupt-claude))))
  (:toggler-command-p nil))

;;; Talking to the page

(-> page-call (cockpit-mode string &rest t) t)
(defun page-call (mode function &rest arguments)
  "Call the page function FUNCTION in MODE's buffer with ARGUMENTS.
The arguments reach the page as data. Nothing is returned."
  (let ((script (apply #'nyxt-llm-cockpit/view:script-call function arguments))
        (buffer (buffer mode)))
    (cond
      (*script-sink*
       (funcall *script-sink* script))
      ((and buffer (not (dead-buffer-p buffer)))
       (handler-case (ffi-buffer-evaluate-javascript-async buffer script)
         (error (condition)
           (log:debug "Cockpit page call dropped: ~a" condition)))))
    nil))

(-> effective-directory (cockpit-mode) string)
(defun effective-directory (mode)
  "Return the directory MODE's program runs in."
  (let ((directory (working-directory mode)))
    (namestring
     (uiop:ensure-directory-pathname
      (if (and directory (uiop:directory-exists-p directory))
          directory
          (user-homedir-pathname))))))

(-> status-line (cockpit-mode) string)
(defun status-line (mode)
  "Return the line under the transcript: state, model, directory, cost, context."
  (nyxt-llm-cockpit/view:status-text (session mode)
                               :directory (effective-directory mode)
                               :model (model mode)))

(-> session-state-name (cockpit-mode) string)
(defun session-state-name (mode)
  "Return the word for MODE's session state, used as a style class."
  (let ((session (session mode)))
    (cond
      ((null session) "ready")
      ((nyxt-llm-cockpit/agent:session-pending-permissions session) "waiting")
      (t (nyxt-llm-cockpit/view:state-word (nyxt-llm-cockpit/agent:session-state session))))))

(-> refresh-status (cockpit-mode) t)
(defun refresh-status (mode)
  "Draw MODE's status line again."
  (page-call mode "status" (status-line mode) (session-state-name mode)))

;;; Following the session

(-> flush-due-p (cockpit-mode) boolean)
(defun flush-due-p (mode)
  "Return whether enough time has passed to draw arriving text again."
  (let ((now (get-internal-real-time))
        (last (last-flush mode)))
    (when (or (null last)
              (>= (- now last)
                  (* (flush-interval mode) internal-time-units-per-second)))
      (setf (last-flush mode) now)
      t)))

(-> draw-entry (cockpit-mode nyxt-llm-cockpit/agent:entry) t)
(defun draw-entry (mode entry)
  "Draw ENTRY again after it changed.
Text that is still arriving replaces only its own text, and not more often
than `flush-interval'. A finished entry, and any tool entry, is drawn whole."
  (if (and (typep entry '(or nyxt-llm-cockpit/agent:assistant-entry
                          nyxt-llm-cockpit/agent:thinking-entry))
           (not (nyxt-llm-cockpit/agent:entry-final-p entry)))
      (when (flush-due-p mode)
        (page-call mode "text"
                   (nyxt-llm-cockpit/view:entry-body-id entry)
                   (nyxt-llm-cockpit/agent:entry-text entry)))
      (page-call mode "replace"
                 (nyxt-llm-cockpit/view:entry-dom-id entry)
                 (nyxt-llm-cockpit/view:entry-html entry))))

(-> ask-in-prompt-buffer (cockpit-mode nyxt-llm-cockpit/agent:permission-request) t)
(defun ask-in-prompt-buffer (mode request)
  "Ask the user about REQUEST in the prompt buffer and return what was typed.
A cancelled prompt returns NIL."
  (declare (ignore mode))
  (handler-case
      (prompt1 :prompt (nyxt-llm-cockpit/view:permission-question request)
               :sources 'prompter:raw-source)
    (prompt-buffer-canceled () nil)))

(defparameter *questions-as-text*
  "The Nyxt cockpit cannot show option dialogs yet. Ask the question as plain text and wait for the reply."
  "What the model is told when it asks to show a dialog of options.")

(-> answer-permission (cockpit-mode nyxt-llm-cockpit/agent:permission-request) t)
(defun answer-permission (mode request)
  "Ask the user about REQUEST and send the answer to the program.
This waits for the user, so it runs on its own thread."
  (bt:with-lock-held ((question-lock mode))
    ;; The program may have withdrawn the request while another was asked.
    (when (and (session mode)
               (eq :pending (nyxt-llm-cockpit/agent:request-state request)))
      (if (equal "AskUserQuestion" (nyxt-llm-cockpit/agent:request-tool-name request))
          ;; A dialog of options cannot be drawn yet, and a yes to one the
          ;; user cannot see means nothing. The model is told to ask in text.
          (nyxt-llm-cockpit/agent:respond-permission (session mode) request :deny
                                               :message *questions-as-text*)
          (let* ((typed (funcall (or *permission-asker* #'ask-in-prompt-buffer)
                                 mode request))
                 (decision (nyxt-llm-cockpit/view:parse-permission-answer typed)))
            (log:debug "Cockpit: the answer about ~a was ~(~a~)."
                       (nyxt-llm-cockpit/agent:request-tool-name request) decision)
            (nyxt-llm-cockpit/agent:respond-permission (session mode) request decision))))))

(-> permission-requested (cockpit-mode nyxt-llm-cockpit/agent:permission-request) t)
(defun permission-requested (mode request)
  "Handle a new permission REQUEST on a thread of its own.
The caller is the thread that reads the program. It must neither wait for the
user nor write to the program."
  (log:debug "Cockpit: the program asks to run ~a."
             (nyxt-llm-cockpit/agent:request-tool-name request))
  (refresh-status mode)
  (let ((asker *permission-asker*)
        (sink *script-sink*))
    (run-thread "cockpit permission"
      (let ((*permission-asker* asker)
            (*script-sink* sink))
        (answer-permission mode request)))))

(-> session-changed (cockpit-mode keyword list) t)
(defun session-changed (mode event arguments)
  "Draw what EVENT changed. This is the session's listener."
  (case event
    (:entry-added
     (log:debug "Cockpit: a new entry, ~(~a~)." (type-of (first arguments)))
     (page-call mode "append" (nyxt-llm-cockpit/view:entry-html (first arguments))))
    (:entry-changed
     (draw-entry mode (first arguments)))
    (:permission-requested
     (permission-requested mode (first arguments)))
    ((:state :usage :turn-ended :permission-settled)
     (unless (eq event :usage)
       (log:debug "Cockpit: ~(~a~)~@[ ~(~a~)~]." event
                  (and (eq event :state) (first arguments))))
     (refresh-status mode)))
  nil)

;;; The session's life

(-> make-session (cockpit-mode) nyxt-llm-cockpit/agent:session)
(defun make-session (mode)
  "Return a new session with MODE's settings, followed by MODE's page."
  (log:debug "Cockpit: a new session. Program ~s, directory ~s, model ~s."
             (program mode) (effective-directory mode) (model mode))
  (let ((session (make-instance 'nyxt-llm-cockpit/agent:session
                                :program (program mode)
                                :working-directory (effective-directory mode)
                                :model (model mode)
                                :permission-mode (permission-mode mode)
                                :setting-sources (setting-sources mode)
                                :extra-arguments (extra-arguments mode))))
    (setf (listener mode)
          (nyxt-llm-cockpit/agent:add-listener
           session
           (lambda (event &rest arguments)
             (session-changed mode event arguments))))
    session))

(-> end-session (cockpit-mode) t)
(defun end-session (mode)
  "End MODE's program, if any. The transcript is kept."
  (let ((session (session mode)))
    (when session
      (when (listener mode)
        (nyxt-llm-cockpit/agent:remove-listener session (listener mode))
        (setf (listener mode) nil))
      (nyxt-llm-cockpit/agent:stop-session session)))
  nil)

(defun detach-handlers (mode)
  "Remove MODE's buffer-delete and browser-exit hook handlers."
  (when (delete-handler mode)
    (hooks:remove-hook (buffer-delete-hook (buffer mode)) (delete-handler mode))
    (setf (delete-handler mode) nil))
  (when (exit-handler mode)
    (when *browser*
      (hooks:remove-hook (before-exit-hook *browser*) (exit-handler mode)))
    (setf (exit-handler mode) nil)))

(defun attach-handlers (mode)
  "End MODE's program when its buffer is deleted or Nyxt quits.
The program is its own process, so it outlives the buffer unless it is ended."
  (unless (delete-handler mode)
    (setf (delete-handler mode)
          (make-instance 'hooks:handler
                         :name (gensym "COCKPIT-DELETE")
                         :fn (lambda (buffer)
                               (declare (ignore buffer))
                               (end-session mode))))
    (hooks:add-hook (buffer-delete-hook (buffer mode)) (delete-handler mode)))
  (when (and *browser* (not (exit-handler mode)))
    (setf (exit-handler mode)
          (make-instance 'hooks:handler
                         :name (gensym "COCKPIT-EXIT")
                         :fn (lambda ()
                               (end-session mode))))
    (hooks:add-hook (before-exit-hook *browser*) (exit-handler mode))))

(defmethod enable ((mode cockpit-mode) &key)
  ;; Opening the page starts nothing: a web page can make a buffer load a
  ;; nyxt: URL, so the program starts with the first prompt the user sends.
  (attach-handlers mode))

(defmethod disable ((mode cockpit-mode) &key)
  (detach-handlers mode)
  (end-session mode)
  (setf (session mode) nil))

(defmethod on-signal-load-finished ((mode cockpit-mode) url title)
  (declare (ignore title))
  (let ((buffer (buffer mode)))
    (when (and buffer (not (dead-buffer-p buffer)))
      (ffi-buffer-evaluate-javascript-async buffer nyxt-llm-cockpit/view:*runtime-script*))
    (refresh-status mode)
    (page-call mode "focus"))
  url)

;;; The page

(-> cockpit-css (theme:theme) string)
(defun cockpit-css (theme)
  "Return the stylesheet that draws the cockpit in THEME's colors."
  (theme:themed-css theme
   '(body
     :padding "0"
     :margin "0")
   '("#cockpit"
     :max-width "88ch"
     :margin "0 auto"
     :padding "24px 24px 190px 24px")
   '(".entry"
     :margin "0 0 17px 0")
   `(".entry .who"
     :font-size "13px"
     :font-weight "600"
     :line-height "17px"
     :color ,theme:primary-color)
   '(".entry .body"
     :white-space "pre-wrap"
     :overflow-wrap "anywhere")
   `(".entry.user .body"
     :padding "7px 12px"
     :border-radius "6px"
     :background-color ,theme:background-color-)
   `(".entry.live .body::after"
     :content "\" ▍\""
     :color ,theme:action-color)
   `(".entry.notice .body"
     :font-size "13px"
     :line-height "17px"
     :color ,theme:primary-color)
   `(".entry.notice.error .body"
     :color ,theme:warning-color)
   `(".entry.thinking"
     :font-size "15px"
     :color ,theme:primary-color)
   '(".entry.child"
     :margin-left "24px"
     :opacity "0.85")
   `(".entry.tool"
     :padding "7px 12px"
     :border-radius "6px"
     :border "1px solid"
     :border-color ,theme:background-color-)
   `(".entry.tool .state"
     :font-weight "400"
     :margin-left "7px"
     :color ,theme:primary-color)
   `(".entry.tool.awaiting-permission"
     :border-color ,theme:action-color)
   `(".entry.tool.awaiting-permission .state"
     :color ,theme:action-color
     :font-weight "600")
   `(".entry.tool.failed .state, .entry.tool.denied .state"
     :color ,theme:warning-color)
   `(".entry.tool .summary, .entry.tool pre"
     :font-family ,theme:monospace-font-family
     :font-size "15px"
     :line-height "22px"
     :white-space "pre-wrap"
     :overflow-wrap "anywhere"
     :margin "3px 0 0 0")
   `(".entry.tool pre"
     :max-height "320px"
     :overflow "auto"
     :padding "0"
     :background-color "transparent")
   `(".entry.tool .cut"
     :font-size "13px"
     :color ,theme:primary-color)
   '("details > summary"
     :cursor "pointer"
     :font-size "13px")
   `("#composer"
     :position "fixed"
     :left "0"
     :right "0"
     :bottom "0"
     :padding "12px 24px 17px 24px"
     :background-color ,theme:background-color)
   '("#composer > div"
     :max-width "88ch"
     :margin "0 auto")
   `("#cockpit-status"
     :font-size "13px"
     :line-height "17px"
     :margin-bottom "7px"
     :color ,theme:primary-color)
   `("#cockpit-status.state-working"
     :color ,theme:action-color)
   `("#cockpit-status.state-waiting"
     :color ,theme:action-color
     :font-weight "600")
   `("#cockpit-status.state-ended"
     :color ,theme:warning-color)
   `("#cockpit-input"
     :display "block"
     :box-sizing "border-box"
     :width "100%"
     :height "96px"
     :padding "7px 12px"
     :border "1px solid"
     :border-color ,theme:background-color-
     :border-radius "6px"
     :resize "vertical"
     :font-family ,theme:font-family
     :font-size "17px"
     :line-height "24px"
     :color ,theme:on-background-color
     :background-color ,theme:background-color)
   `("#cockpit-input:focus"
     :outline "2px solid"
     :outline-color ,theme:action-color
     :outline-offset "-1px")))

(-> cockpit-page (cockpit-mode) string)
(defun cockpit-page (mode)
  "Return the body of the cockpit page for MODE, drawn from its session."
  (let ((entries (if (session mode)
                     (nyxt-llm-cockpit/agent:session-entries (session mode))
                     #())))
    (spinneret:with-html-string
      (:style (:raw (cockpit-css (theme *browser*))))
      (:div :id "cockpit"
            (:div :id "transcript"
                  (:raw (nyxt-llm-cockpit/view:transcript-html entries))))
      (:div :id "composer"
            (:div
             (:div :id "cockpit-status"
                   :class (uiop:strcat "state-" (session-state-name mode))
                   (status-line mode))
             (:textarea :id "cockpit-input"
                        :spellcheck "false"
                        :placeholder "Ask Claude. Return sends, Shift-Return starts a new line."))))))

(define-internal-page-command-global claude ()
    (buffer "*Claude*" 'nyxt-llm-cockpit:cockpit-mode)
  "Open the Claude Code cockpit."
  (cockpit-page (find-mode buffer 'cockpit-mode)))

;;; Commands

(-> typed-prompt (cockpit-mode) (maybe string))
(defun typed-prompt (mode)
  "Take what is typed in MODE's page and return it, or NIL when it is blank."
  (let* ((buffer (buffer mode))
         (value (cond
                  (*prompt-taker*
                   (funcall *prompt-taker* mode))
                  ((and buffer (not (dead-buffer-p buffer)))
                   (ffi-buffer-evaluate-javascript
                    buffer
                    "window.llmCockpit ? window.llmCockpit.take() : \"\""))))
         (text (and (stringp value)
                    (string-trim '(#\Space #\Tab #\Newline #\Return) value))))
    (log:debug "Cockpit: the page gave ~:[no text~;~:*~d character~:p~]."
               (and (stringp value) (length value)))
    (and text (plusp (length text)) text)))

(-> submit-prompt (cockpit-mode string) boolean)
(defun submit-prompt (mode text)
  "Send TEXT to MODE's session, starting one first. Return whether it was sent."
  (let ((session (or (session mode)
                     (setf (session mode) (make-session mode)))))
    (cond
      ((eq :dead (nyxt-llm-cockpit/agent:session-state session))
       (echo-warning "This conversation has ended. Run new-claude-session to start another.")
       nil)
      ((nyxt-llm-cockpit/agent:send-prompt session text)
       t)
      (t
       (refresh-status mode)
       (echo-warning "Claude Code is not running. The transcript says why.")
       nil))))

(define-command send-prompt ((mode cockpit-mode))
  "Send what is typed in the cockpit to Claude."
  (log:debug "Cockpit: send-prompt runs.")
  (let ((text (typed-prompt mode)))
    (if text
        (let ((sent (submit-prompt mode text)))
          (log:debug "Cockpit: the prompt was ~:[not sent~;sent~]." sent)
          (unless sent
            ;; Nothing typed is lost when it could not be sent.
            (page-call mode "restore" text)))
        (echo "Type a prompt first."))
    ;; The next prompt can be typed at once, wherever the focus had gone.
    (page-call mode "focus")))

(define-command interrupt-claude ((mode cockpit-mode))
  "Stop the turn Claude is running."
  (if (and (session mode)
           (nyxt-llm-cockpit/agent:interrupt-turn (session mode)))
      (echo "Asked Claude to stop.")
      (echo "Claude is not working on anything.")))

(define-command new-claude-session ((mode cockpit-mode))
  "End this conversation and start an empty one."
  (end-session mode)
  (setf (session mode) nil)
  (let ((buffer (buffer mode)))
    (when (and buffer (not (dead-buffer-p buffer)))
      (ffi-buffer-reload buffer)))
  (echo "A new conversation starts with your next prompt."))

(define-command set-claude-directory ((mode cockpit-mode))
  "Choose the directory Claude Code works in, from the next conversation on."
  (let* ((typed (prompt1 :prompt "Directory for Claude Code"
                         :input (effective-directory mode)
                         :sources 'prompter:raw-source))
         (directory (and (stringp typed)
                         (uiop:directory-exists-p
                          (uiop:ensure-directory-pathname
                           (uiop:native-namestring typed))))))
    (if directory
        (progn
          (setf (working-directory mode) (namestring directory))
          (refresh-status mode)
          (echo "Claude Code will work in ~a from the next conversation." (namestring directory)))
        (echo-warning "~a is not a directory." typed))))
