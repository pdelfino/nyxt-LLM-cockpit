;;;; SPDX-FileCopyrightText: Pedro Delfino
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; Drive the real Claude Code program once, on Haiku, in an empty folder.
;;;; This spends a fraction of a cent of the logged-in account and needs the
;;;; network, so it is never part of `make test'.
;;;;
;;;; Usage, from the repository root: sbcl --script tests/real.lisp
;;;;
;;;; The run loads no user settings (so no personal hooks or allow rules
;;;; apply) and saves no session.

(require :asdf)

(let ((root (uiop:pathname-parent-directory-pathname
             (uiop:pathname-directory-pathname *load-truename*))))
  (asdf:load-asd (merge-pathnames "nyxt-llm-cockpit.asd" root)))

(let ((*standard-output* (make-broadcast-stream))
      (*error-output* (make-broadcast-stream)))
  (handler-bind ((warning #'muffle-warning))
    (asdf:load-system :nyxt-llm-cockpit/agent)))

(defpackage :nyxt-llm-cockpit/real
  (:use :cl :nyxt-llm-cockpit/agent))
(in-package :nyxt-llm-cockpit/real)

(defvar *failures* 0)

(defun expect (description value)
  (format t "~&  ~:[FAIL~;ok  ~] ~a~%" value description)
  (unless value
    (incf *failures*))
  value)

(defun entries-of (session class)
  (remove-if-not (lambda (entry) (typep entry class))
                 (coerce (session-entries session) 'list)))

(defun last-text (session class)
  (let ((entry (first (last (entries-of session class)))))
    (and entry (entry-text entry))))

(defun show (session)
  (loop for entry across (session-entries session)
        do (format t "~&    ~3d ~(~a~)~@[ ~a~]: ~a~%"
                   (entry-id entry)
                   (type-of entry)
                   (and (typep entry 'tool-entry) (tool-status entry))
                   (let ((text (if (typep entry 'tool-entry)
                                   (tool-summary (tool-name entry) (tool-input entry))
                                   (entry-text entry))))
                     (substitute #\Space #\Newline
                                 (subseq text 0 (min 100 (length text))))))))

(defun turn-done-p (session turns)
  (and (>= (session-turns session) turns)
       (member (session-state session) '(:idle :dead))))

(defun run ()
  (unless (find-program "claude")
    (format t "~&Claude Code is not installed: nothing to run.~%")
    (return-from run 2))
  (let* ((directory (uiop:ensure-directory-pathname
                     (uiop:run-program '("mktemp" "-d") :output '(:string :stripped t))))
         (marker (merge-pathnames "cockpit-marker.txt" directory))
         (session (make-instance 'session
                                 :working-directory directory
                                 :model "haiku"
                                 :setting-sources "project"
                                 :extra-arguments '("--no-session-persistence")))
         (changes 0))
    (add-listener session (lambda (event &rest arguments)
                            (declare (ignore arguments))
                            (when (eq event :entry-changed)
                              (incf changes))))
    (unwind-protect
         (progn
           (format t "~&Turn 1: a tool the user denies~%")
           (send-prompt session "Use the Bash tool to run exactly: touch cockpit-marker.txt  Then answer with the single word done, or the single word refused if you were not allowed.")
           (let ((request (wait-until (lambda ()
                                        (first (session-pending-permissions session)))
                                      :timeout 90)))
             (expect "the program asked before running the tool" request)
             (when request
               (expect "the request names the Bash tool"
                       (equal "Bash" (request-tool-name request)))
               (expect "the request carries the command"
                       (search "touch cockpit-marker.txt"
                               (or (jget (request-input request) "command") "")))
               (respond-permission session request :deny
                                   :message "Denied by the cockpit's real test.")))
           (expect "the turn ended" (wait-until (lambda () (turn-done-p session 1))
                                                :timeout 90))
           (expect "the file was not created" (not (probe-file marker)))
           (expect "the tool is marked denied"
                   (let ((tool (first (entries-of session 'tool-entry))))
                     (and tool (eq :denied (tool-status tool)))))
           (expect "the model said it was refused"
                   (search "refused" (string-downcase (or (last-text session 'assistant-entry) ""))))
           (expect "text arrived in pieces" (plusp changes))

           (format t "~&Turn 2: the same tool, allowed~%")
           (send-prompt session "Try the same command once more and answer the same way.")
           (let ((request (wait-until (lambda ()
                                        (first (session-pending-permissions session)))
                                      :timeout 90)))
             (expect "the program asked again" request)
             (when request
               (respond-permission session request :allow)))
           (expect "the turn ended" (wait-until (lambda () (turn-done-p session 2))
                                                :timeout 90))
           (expect "the file was created" (probe-file marker))
           (expect "the model said done"
                   (search "done" (string-downcase (or (last-text session 'assistant-entry) ""))))
           (expect "the session has an id" (stringp (session-id session)))
           (expect "a cost was reported" (plusp (session-total-cost session)))
           (expect "the context size is known"
                   (and (integerp (session-context-tokens session))
                        (integerp (session-context-window session))))

           (format t "~&Turn 3: interrupted~%")
           (send-prompt session "Count from 1 to 400, one number per line, with no other text.")
           (expect "the reply started"
                   (wait-until (lambda ()
                                 (let ((text (last-text session 'assistant-entry)))
                                   (and text (search "5" text))))
                               :timeout 90))
           (expect "the interrupt was sent" (interrupt-turn session))
           (expect "the turn ended" (wait-until (lambda () (turn-done-p session 3))
                                                :timeout 90))
           (expect "the count stopped early"
                   (not (search "400" (or (last-text session 'assistant-entry) ""))))
           (expect "the session is still usable" (eq :idle (session-state session)))
           (expect "nothing went wrong inside" (null (session-problems session)))
           (format t "~&Transcript:~%")
           (show session)
           (format t "~&  cost $~,4f · context ~a of ~a tokens · ~d entry changes~%"
                   (session-total-cost session)
                   (session-context-tokens session)
                   (session-context-window session)
                   changes)
           (dolist (problem (session-problems session))
             (format t "~&  problem: ~a~%" problem)))
      (stop-session session)
      (uiop:delete-directory-tree directory :validate t :if-does-not-exist :ignore))
    (expect "the program has exited" (not (session-alive-p session)))
    (format t "~&~d failure~:p~%" *failures*)
    (if (zerop *failures*) 0 1)))

(uiop:quit (run))
