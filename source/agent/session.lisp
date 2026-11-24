;;;; SPDX-FileCopyrightText: Pedro Delfino
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; A session: one running program, the transcript it has produced so far,
;;;; and the questions it is waiting on.
;;;;
;;;; This is the only file that knows the program's frames. Everything above
;;;; it reads entries, so a change in the protocol is a change here.

(in-package :nyxt-llm-cockpit/agent)

(defparameter *denied-by-user*
  "The user denied this tool in the Nyxt cockpit."
  "What the model is told when the user says no without a reason.")

;;; Entries

(defclass entry ()
  ((id
    :initarg :id
    :reader entry-id
    :documentation "A number unique in the session. Entries are made in this order.")
   (parent
    :initarg :parent
    :initform nil
    :reader entry-parent
    :documentation "The tool call that started the subagent this happened in.
NIL for what the main thread did."))
  (:documentation "One item of a transcript."))

(defclass text-entry (entry)
  ((text
    :initarg :text
    :initform ""
    :accessor entry-text)
   (final-p
    :initarg :final-p
    :initform t
    :accessor entry-final-p
    :documentation "NIL while the text is still arriving.")
   (message-id
    :initarg :message-id
    :initform nil
    :reader entry-message-id
    :documentation "The model message the text belongs to."))
  (:documentation "An entry that is text."))

(defclass user-entry (text-entry) ()
  (:documentation "What the user sent."))

(defclass assistant-entry (text-entry) ()
  (:documentation "What the model said."))

(defclass thinking-entry (text-entry) ()
  (:documentation "What the model thought before answering."))

(defclass notice-entry (text-entry)
  ((kind
    :initarg :kind
    :reader notice-kind
    :documentation "One of :started, :turn-end, :error, :stopped or :compact."))
  (:documentation "A line from the cockpit itself, not from the model."))

(defclass tool-entry (entry)
  ((tool-use-id
    :initarg :tool-use-id
    :reader tool-use-id)
   (name
    :initarg :name
    :initform ""
    :accessor tool-name)
   (input
    :initarg :input
    :initform nil
    :accessor tool-input
    :documentation "The arguments, as a JSON object. They are data, never instructions.")
   (status
    :initarg :status
    :initform :proposed
    :accessor tool-status
    :documentation "One of :proposed (still arriving), :requested, :awaiting-permission,
:running, :done, :failed or :denied.")
   (result
    :initform nil
    :accessor tool-result
    :documentation "What the tool returned, as text.")
   (error-p
    :initform nil
    :accessor tool-error-p))
  (:documentation "A tool the model asked for, and what came back."))

(defmethod entry-final-p ((entry tool-entry))
  (member (tool-status entry) '(:done :failed :denied)))

(defmethod entry-text ((entry tool-entry))
  (or (tool-result entry) ""))

;;; Permission requests

(defclass permission-request ()
  ((id
    :initarg :id
    :reader request-id)
   (tool-name
    :initarg :tool-name
    :reader request-tool-name)
   (input
    :initarg :input
    :initform nil
    :reader request-input)
   (description
    :initarg :description
    :initform nil
    :reader request-description)
   (suggestions
    :initarg :suggestions
    :initform nil
    :reader request-suggestions
    :documentation "The rules the program offers so the user is not asked again.")
   (blocked-path
    :initarg :blocked-path
    :initform nil
    :reader request-blocked-path)
   (tool-use-id
    :initarg :tool-use-id
    :initform nil
    :reader request-tool-use-id)
   (state
    :initform :pending
    :accessor request-state
    :documentation "One of :pending, :allowed, :denied or :cancelled."))
  (:documentation "The program asking whether it may run a tool."))

;;; Sessions

(defclass session ()
  ((program
    :initarg :program
    :initform "claude"
    :accessor session-program
    :documentation "The Claude Code program: a name looked up by `find-program', or a path.")
   (working-directory
    :initarg :working-directory
    :initform nil
    :accessor session-working-directory
    :documentation "Where the program runs. NIL is the current directory.")
   (model
    :initarg :model
    :initform nil
    :accessor session-model)
   (permission-mode
    :initarg :permission-mode
    :initform nil
    :accessor session-permission-mode)
   (resume
    :initarg :resume
    :initform nil
    :accessor session-resume
    :documentation "The id of an earlier session to continue, or NIL.")
   (setting-sources
    :initarg :setting-sources
    :initform nil
    :accessor session-setting-sources
    :documentation "Which settings files the program loads, such as \"user,project\".
NIL lets the program load what it loads in a terminal.")
   (extra-arguments
    :initarg :extra-arguments
    :initform '()
    :accessor session-extra-arguments)
   (state
    :initform :new
    :reader session-state
    :documentation "One of :new, :idle, :running or :dead.")
   (id
    :initform nil
    :reader session-id
    :documentation "The program's own id for the conversation, once it has said it.")
   (info
    :initform nil
    :reader session-info
    :documentation "The program's first frame: model, tools, version, directory.")
   (entries
    :initform (make-array 16 :adjustable t :fill-pointer 0))
   (permissions
    :initform (make-hash-table :test 'equal))
   (listeners
    :initform '())
   (total-cost
    :initform 0
    :reader session-total-cost
    :documentation "The program's estimate for the whole session, in US dollars.")
   (turns
    :initform 0
    :reader session-turns)
   (context-tokens
    :initform nil
    :reader session-context-tokens
    :documentation "How many tokens the last model call read and wrote.")
   (context-window
    :initform nil
    :reader session-context-window)
   (problems
    :initform '()
    :documentation "Newest first.")
   (child :initform nil)
   (reader :initform nil)
   (error-reader :initform nil)
   (error-lines :initform '())
   (announced-p :initform nil)
   (stopping-p :initform nil)
   (live-messages :initform (make-hash-table :test 'equal))
   (live-blocks :initform (make-hash-table :test 'equal))
   (next-entry-id :initform 0)
   (next-request-id :initform 0)
   (lock :initform (sb-thread:make-mutex :name "cockpit session")))
  (:documentation "A conversation with the program."))

(defmacro with-session ((session) &body body)
  "Run BODY holding SESSION's lock. The same thread may take it again."
  `(sb-thread:with-recursive-lock ((slot-value ,session 'lock))
     ,@body))

(defvar *event-box* nil
  "While a thread changes a session, a one-element list holding the events
the change produced, newest first. NIL otherwise.")

(defun deliver (session events)
  "Call SESSION's listeners with each of EVENTS, in order."
  (dolist (event events)
    (dolist (listener (with-session (session)
                        (copy-list (slot-value session 'listeners))))
      (handler-case (apply listener event)
        (error (condition)
          (note-problem session "A listener failed on ~(~a~): ~a"
                        (first event) condition))))))

(defun call-changing-session (session function)
  "Call FUNCTION holding SESSION's lock, then tell the listeners what changed.
The listeners run after the lock is released, so one that waits on the
browser, or that calls back into the session, holds nothing up."
  (if *event-box*
      (with-session (session)
        (funcall function))
      (let ((box (list '())))
        (unwind-protect
             (let ((*event-box* box))
               (with-session (session)
                 (funcall function)))
          (deliver session (reverse (first box)))))))

(defmacro changing-session ((session) &body body)
  "Run BODY holding SESSION's lock. Its events reach the listeners afterwards."
  `(call-changing-session ,session (lambda () ,@body)))

(defun session-entries (session)
  "Return SESSION's entries, oldest first, as a fresh vector."
  (with-session (session)
    (copy-seq (slot-value session 'entries))))

(defun session-pending-permissions (session)
  "Return the permission requests SESSION is waiting on, oldest first."
  (with-session (session)
    (sort (loop for request being the hash-values of (slot-value session 'permissions)
                collect request)
          #'string< :key #'request-id)))

(defun session-problems (session)
  "Return what went wrong inside SESSION, oldest first, as strings."
  (with-session (session)
    (reverse (slot-value session 'problems))))

(defun session-alive-p (session)
  "Return whether SESSION's program is running."
  (child-alive-p (slot-value session 'child)))

(defun session-busy-p (session)
  "Return whether SESSION is in the middle of a turn."
  (eq (session-state session) :running))

(defun add-listener (session listener)
  "Call LISTENER with an event and its arguments whenever SESSION changes.
The events are (:entry-added ENTRY), (:entry-changed ENTRY),
(:permission-requested REQUEST), (:permission-settled REQUEST), (:state STATE),
(:turn-ended) and (:usage). LISTENER runs on the thread that made the change, after the change is complete
and with no lock held. It must not wait for the user: that thread is often the
one reading the program."
  (with-session (session)
    (pushnew listener (slot-value session 'listeners)))
  listener)

(defun remove-listener (session listener)
  "Stop calling LISTENER."
  (with-session (session)
    (setf (slot-value session 'listeners)
          (remove listener (slot-value session 'listeners))))
  nil)

(defun note-problem (session control &rest arguments)
  "Record that something went wrong inside SESSION."
  (with-session (session)
    (push (apply #'format nil control arguments) (slot-value session 'problems)))
  nil)

(defun notify (session event &rest arguments)
  "Tell SESSION's listeners about EVENT, once the change in progress is done."
  (if *event-box*
      (push (cons event arguments) (first *event-box*))
      (deliver session (list (cons event arguments)))))

(defun set-state (session state)
  (unless (eq state (slot-value session 'state))
    (setf (slot-value session 'state) state)
    (notify session :state state)))

;;; Building the transcript

(defun add-entry (session class &rest initargs)
  "Make an entry of CLASS, append it to SESSION and tell the listeners."
  (let ((entry (apply #'make-instance class
                      :id (incf (slot-value session 'next-entry-id))
                      initargs)))
    (vector-push-extend entry (slot-value session 'entries))
    (notify session :entry-added entry)
    entry))

(defun add-notice (session kind control &rest arguments)
  (add-entry session 'notice-entry
             :kind kind
             :text (apply #'format nil control arguments)))

(defun find-tool-entry (session tool-use-id)
  "Return SESSION's entry for the tool call TOOL-USE-ID, newest first, or NIL."
  (when tool-use-id
    (find-if (lambda (entry)
               (and (typep entry 'tool-entry)
                    (equal (tool-use-id entry) tool-use-id)))
             (slot-value session 'entries)
             :from-end t)))

(defun find-open-text-entry (session class message-id parent)
  "Return the oldest entry of CLASS in MESSAGE-ID whose text is still arriving."
  (find-if (lambda (entry)
             (and (typep entry class)
                  (not (entry-final-p entry))
                  (equal (entry-message-id entry) message-id)
                  (equal (entry-parent entry) parent)))
           (slot-value session 'entries)))

(defun content-text (content)
  "Return the text of a tool result's CONTENT, which is a string or a list of blocks."
  (cond
    ((stringp content) content)
    ((vectorp content)
     (with-output-to-string (out)
       (loop for block across content
             for first-p = t then nil
             do (unless first-p
                  (terpri out))
                (let ((type (jget block "type")))
                  (cond
                    ((equal type "text") (write-string (or (jget block "text") "") out))
                    ((stringp type) (format out "[~a]" type)))))))
    (t "")))

(defun tool-summary (name input)
  "Return one line saying what the tool NAME would do with INPUT."
  (let ((detail (or (jget input "command")
                    (jget input "file_path")
                    (jget input "path")
                    (jget input "pattern")
                    (jget input "url")
                    (jget input "query")
                    (jget input "description")
                    (jget input "prompt"))))
    (if (stringp detail)
        (let ((line (substitute #\Space #\Newline detail)))
          (format nil "~a: ~a" name
                  (if (> (length line) 160)
                      (concatenate 'string (subseq line 0 157) "...")
                      line)))
        name)))

;;; Reading frames

(defun block-key (parent index)
  (format nil "~a/~a" (or parent "") index))

(defun handle-system (session frame)
  (let ((subtype (jget frame "subtype")))
    (cond
      ((equal subtype "init")
       (setf (slot-value session 'info) frame
             (slot-value session 'id) (jget frame "session_id"))
       (unless (slot-value session 'announced-p)
         (setf (slot-value session 'announced-p) t)
         (add-notice session :started "Claude Code ~a · ~a · ~a"
                     (or (jget frame "claude_code_version") "?")
                     (or (jget frame "model") "?")
                     (or (jget frame "cwd") "?")))
       (notify session :usage))
      ((equal subtype "compact_boundary")
       (add-notice session :compact "The conversation was compacted.")))))

(defun handle-stream-event (session frame)
  (let* ((event (jget frame "event"))
         (type (jget event "type"))
         (parent (jget frame "parent_tool_use_id"))
         (live-blocks (slot-value session 'live-blocks))
         (key (block-key parent (jget event "index"))))
    (cond
      ((equal type "message_start")
       (setf (gethash (or parent "") (slot-value session 'live-messages))
             (jget event "message" "id")))
      ((equal type "content_block_start")
       (let* ((block (jget event "content_block"))
              (kind (jget block "type"))
              (message-id (gethash (or parent "") (slot-value session 'live-messages))))
         (cond
           ((equal kind "text")
            (setf (gethash key live-blocks)
                  (add-entry session 'assistant-entry
                             :text (or (jget block "text") "")
                             :final-p nil
                             :message-id message-id
                             :parent parent)))
           ;; A thinking block may carry no text at all. The entry is made
           ;; when the first text arrives.
           ((equal kind "thinking")
            (setf (gethash key live-blocks) (list :thinking message-id)))
           ((equal kind "tool_use")
            (setf (gethash key live-blocks)
                  (add-entry session 'tool-entry
                             :tool-use-id (jget block "id")
                             :name (or (jget block "name") "")
                             :parent parent))))))
      ((equal type "content_block_delta")
       (let* ((delta (jget event "delta"))
              (delta-type (jget delta "type"))
              (piece (cond
                       ((equal delta-type "text_delta") (jget delta "text"))
                       ((equal delta-type "thinking_delta") (jget delta "thinking"))))
              (live (gethash key live-blocks)))
         (when (and (stringp piece) (plusp (length piece)))
           (cond
             ((typep live 'text-entry)
              (setf (entry-text live) (concatenate 'string (entry-text live) piece))
              (notify session :entry-changed live))
             ((and (consp live) (eq (first live) :thinking))
              (setf (gethash key live-blocks)
                    (add-entry session 'thinking-entry
                               :text piece
                               :final-p nil
                               :message-id (second live)
                               :parent parent)))))))
      ((equal type "content_block_stop")
       (remhash key live-blocks)))))

(defun settle-text (session class message-id parent text)
  "Give the entry of CLASS in MESSAGE-ID its finished TEXT, or make it."
  (let ((entry (find-open-text-entry session class message-id parent)))
    (cond
      (entry
       (when (plusp (length text))
         (setf (entry-text entry) text))
       (setf (entry-final-p entry) t)
       (notify session :entry-changed entry))
      ((plusp (length text))
       (add-entry session class
                  :text text
                  :message-id message-id
                  :parent parent)))))

(defun settle-tool (session block parent)
  "Give the entry for the tool call BLOCK its finished arguments, or make it."
  (let ((entry (find-tool-entry session (jget block "id"))))
    (if entry
        (progn
          (setf (tool-name entry) (or (jget block "name") (tool-name entry))
                (tool-input entry) (jget block "input"))
          (when (eq (tool-status entry) :proposed)
            (setf (tool-status entry) :requested))
          (notify session :entry-changed entry))
        (add-entry session 'tool-entry
                   :tool-use-id (jget block "id")
                   :name (or (jget block "name") "")
                   :input (jget block "input")
                   :status :requested
                   :parent parent))))

(defun note-context (session usage)
  "Remember how full the context was at the model call that reported USAGE."
  (let ((total (loop for key in '("input_tokens" "cache_read_input_tokens"
                                  "cache_creation_input_tokens" "output_tokens")
                     for count = (jget usage key)
                     when (integerp count)
                       sum count)))
    (when (plusp total)
      (setf (slot-value session 'context-tokens) total))))

(defun handle-assistant (session frame)
  (let* ((message (jget frame "message"))
         (message-id (jget message "id"))
         (parent (jget frame "parent_tool_use_id"))
         (content (jget message "content")))
    (when (and (vectorp content) (not (stringp content)))
      (loop for block across content
            for kind = (jget block "type")
            do (cond
                 ((equal kind "text")
                  (settle-text session 'assistant-entry message-id parent
                               (or (jget block "text") "")))
                 ((equal kind "thinking")
                  (settle-text session 'thinking-entry message-id parent
                               (or (jget block "thinking") "")))
                 ((equal kind "tool_use")
                  (settle-tool session block parent)))))
    (unless parent
      (note-context session (jget message "usage")))
    (let ((failure (jget frame "error")))
      (when (stringp failure)
        (add-notice session :error "The model call failed: ~a" failure)))))

(defun handle-user (session frame)
  (let ((content (jget frame "message" "content")))
    (when (and (vectorp content) (not (stringp content)))
      (loop for block across content
            do (when (equal (jget block "type") "tool_result")
                 (let ((entry (find-tool-entry session (jget block "tool_use_id"))))
                   (when entry
                     (setf (tool-result entry) (content-text (jget block "content"))
                           (tool-error-p entry) (jtrue-p (jget block "is_error")))
                     (setf (tool-status entry)
                           (cond
                             ((eq (tool-status entry) :denied) :denied)
                             ((tool-error-p entry) :failed)
                             (t :done)))
                     (notify session :entry-changed entry))))))))

(defun settle-open-entries (session)
  "Mark every entry whose text was still arriving as finished."
  (loop for entry across (slot-value session 'entries)
        do (when (and (typep entry 'text-entry) (not (entry-final-p entry)))
             (setf (entry-final-p entry) t)
             (notify session :entry-changed entry)))
  (clrhash (slot-value session 'live-blocks)))

(defun handle-result (session frame)
  (let* ((failed-p (jtrue-p (jget frame "is_error")))
         (subtype (or (jget frame "subtype") ""))
         (reason (jget frame "terminal_reason"))
         (cost (jget frame "total_cost_usd"))
         (before (slot-value session 'total-cost))
         (duration (jget frame "duration_ms")))
    (settle-open-entries session)
    (when (realp cost)
      (setf (slot-value session 'total-cost) cost))
    (incf (slot-value session 'turns))
    (let ((usage (jget frame "modelUsage")))
      (when (hash-table-p usage)
        (loop for model being the hash-values of usage
              for window = (jget model "contextWindow")
              when (integerp window)
                do (setf (slot-value session 'context-window)
                         (max window (or (slot-value session 'context-window) 0))))))
    (let ((seconds (if (realp duration) (/ duration 1000.0) nil))
          (turn-cost (if (realp cost) (max 0 (- cost before)) nil)))
      (if (or failed-p (not (equal subtype "success")))
          (add-notice session :error "The turn stopped (~a)~@[: ~a~]"
                      (or reason subtype)
                      (let ((text (jget frame "result")))
                        (and (stringp text) (plusp (length text)) text)))
          (add-notice session :turn-end "Done~@[ in ~,1f s~]~@[ · $~,4f~]"
                      seconds turn-cost)))
    (when (child-alive-p (slot-value session 'child))
      (set-state session :idle))
    (notify session :usage)
    (notify session :turn-ended)))

(defun success-frame (request-id answer)
  (jobject "type" "control_response"
           "response" (jobject "subtype" "success"
                               "request_id" request-id
                               "response" answer)))

(defun refusal-frame (request-id message)
  (jobject "type" "control_response"
           "response" (jobject "subtype" "error"
                               "request_id" request-id
                               "error" message)))

(defun handle-control-request (session frame)
  "Return the frames to send back at once, if any."
  (let* ((request (jget frame "request"))
         (subtype (jget request "subtype"))
         (request-id (jget frame "request_id")))
    (cond
      ((equal subtype "can_use_tool")
       (let ((permission (make-instance
                          'permission-request
                          :id request-id
                          :tool-name (or (jget request "tool_name") "")
                          :input (jget request "input")
                          :description (jget request "description")
                          :suggestions (jget request "permission_suggestions")
                          :blocked-path (jget request "blocked_path")
                          :tool-use-id (jget request "tool_use_id")))
             (entry (find-tool-entry session (jget request "tool_use_id"))))
         (setf (gethash request-id (slot-value session 'permissions)) permission)
         (when entry
           (setf (tool-status entry) :awaiting-permission)
           (notify session :entry-changed entry))
         (notify session :permission-requested permission)
         nil))
      ;; The program waits for an answer to every request. One this client
      ;; does not know is refused, so the turn goes on.
      (t
       (list (refusal-frame request-id
                            (format nil "The Nyxt cockpit does not handle ~a requests."
                                    (or subtype "these"))))))))

(defun handle-control-cancel (session frame)
  (let* ((request-id (jget frame "request_id"))
         (permission (gethash request-id (slot-value session 'permissions))))
    (when permission
      (remhash request-id (slot-value session 'permissions))
      (setf (request-state permission) :cancelled)
      (let ((entry (find-tool-entry session (request-tool-use-id permission))))
        (when (and entry (eq (tool-status entry) :awaiting-permission))
          (setf (tool-status entry) :requested)
          (notify session :entry-changed entry)))
      (notify session :permission-settled permission))))

(defun handle-frame (session frame)
  "Apply one FRAME from the program. Return the frames to send back, if any."
  (let ((type (jget frame "type")))
    (changing-session (session)
      (cond
        ((equal type "stream_event") (handle-stream-event session frame) nil)
        ((equal type "assistant") (handle-assistant session frame) nil)
        ((equal type "user") (handle-user session frame) nil)
        ((equal type "system") (handle-system session frame) nil)
        ((equal type "result") (handle-result session frame) nil)
        ((equal type "control_request") (handle-control-request session frame))
        ((equal type "control_cancel_request") (handle-control-cancel session frame) nil)
        ;; Answers to our own requests, rate limit notes, keep-alives and
        ;; frames from a newer program are read and dropped.
        (t nil)))))

;;; Talking to the program

(defun send-frame (session frame)
  "Write FRAME to SESSION's program. Return whether it was written."
  (let ((child (slot-value session 'child)))
    (and child
         (child-send-line child (to-json frame)))))

(defun read-frames (session)
  "Read the program's output until it ends. This is the reader thread."
  (let ((stream (child-output (slot-value session 'child))))
    (unwind-protect
         (loop for line = (handler-case (read-line stream nil :eof)
                            (error (condition)
                              (note-problem session "Reading the program failed: ~a"
                                            condition)
                              :eof))
               until (eq line :eof)
               do (let ((text (string-trim '(#\Space #\Tab #\Return) line)))
                    (when (plusp (length text))
                      (handler-case
                          (dolist (reply (handle-frame session (parse-json text)))
                            (send-frame session reply))
                        (json-error (condition)
                          (note-problem session "A line was not JSON (~a): ~a"
                                        condition
                                        (subseq text 0 (min 120 (length text)))))
                        (error (condition)
                          (note-problem session "A frame could not be applied: ~a"
                                        condition))))))
      (session-ended session))))

(defun read-error-lines (session)
  "Keep the last lines the program printed as errors."
  (let ((stream (child-error-output (slot-value session 'child))))
    (ignore-errors
     (loop for line = (read-line stream nil :eof)
           until (eq line :eof)
           do (with-session (session)
                (push line (slot-value session 'error-lines))
                (when (> (length (slot-value session 'error-lines)) 20)
                  (setf (slot-value session 'error-lines)
                        (subseq (slot-value session 'error-lines) 0 20))))))))

(defun session-ended (session)
  "Record that SESSION's program has exited."
  (let* ((child (slot-value session 'child))
         ;; A program that closed its output without exiting is not waited on forever.
         (code (and (wait-for-exit child 5)
                    (ignore-errors (uiop:wait-process (child-info child))))))
    (changing-session (session)
      (settle-open-entries session)
      (loop for permission being the hash-values of (slot-value session 'permissions)
            do (setf (request-state permission) :cancelled)
               (notify session :permission-settled permission))
      (clrhash (slot-value session 'permissions))
      (cond
        ((slot-value session 'stopping-p)
         (add-notice session :stopped "The session was closed."))
        ((eql code 0)
         (add-notice session :stopped "Claude Code exited."))
        (t
         (add-notice session :error "Claude Code exited with code ~a.~@[~%~a~]"
                     code
                     (let ((lines (reverse (slot-value session 'error-lines))))
                       (and lines (format nil "~{~a~^~%~}" lines))))))
      (set-state session :dead))))

(defun session-command (session)
  "Return the command line that starts SESSION's program, as a list of strings."
  (append
   (list (or (find-program (session-program session)) (session-program session))
         "-p"
         "--output-format" "stream-json"
         "--input-format" "stream-json"
         "--verbose"
         "--include-partial-messages"
         ;; Permission questions come to this process on the same pipe.
         "--permission-prompt-tool" "stdio")
   (when (session-model session)
     (list "--model" (session-model session)))
   (when (session-permission-mode session)
     (list "--permission-mode" (session-permission-mode session)))
   (when (session-resume session)
     (list "--resume" (session-resume session)))
   (when (session-setting-sources session)
     (list "--setting-sources" (session-setting-sources session)))
   (session-extra-arguments session)))

(defun start-session (session)
  "Start SESSION's program. Return SESSION, or NIL when it could not start."
  (changing-session (session)
    (unless (eq (session-state session) :new)
      (return-from start-session session))
    (handler-case
        (setf (slot-value session 'child)
              (launch-child (session-command session)
                            :directory (session-working-directory session)))
      (error (condition)
        (note-problem session "Claude Code could not be started: ~a" condition)
        (add-notice session :error
                    "Claude Code could not be started (~a). Is it installed? ~a"
                    (session-program session) condition)
        (set-state session :dead)
        (return-from start-session nil)))
    (setf (slot-value session 'reader)
          (sb-thread:make-thread (lambda () (read-frames session))
                                 :name "cockpit reader")
          (slot-value session 'error-reader)
          (sb-thread:make-thread (lambda () (read-error-lines session))
                                 :name "cockpit error reader"))
    (set-state session :idle)
    session))

(defun send-prompt (session text)
  "Send TEXT to the model as the user. Return whether it was sent.
A new session is started first. A prompt sent while a turn is running is
queued by the program."
  (when (eq (session-state session) :new)
    (start-session session))
  (changing-session (session)
    (unless (session-alive-p session)
      (unless (eq (session-state session) :dead)
        (set-state session :dead))
      (return-from send-prompt nil))
    (add-entry session 'user-entry :text text)
    (set-state session :running))
  (send-frame session
              (jobject "type" "user"
                       "message" (jobject "role" "user"
                                          "content" (jarray (jobject "type" "text"
                                                                     "text" text)))
                       "parent_tool_use_id" :null)))

(defun interrupt-turn (session)
  "Ask the program to stop the turn it is running. Return whether it was asked."
  (when (session-busy-p session)
    (send-frame session
                (jobject "type" "control_request"
                         "request_id" (with-session (session)
                                        (format nil "nyxt-~d"
                                                (incf (slot-value session
                                                                  'next-request-id))))
                         "request" (jobject "subtype" "interrupt")))))

(defun session-rule (request)
  "Return the rule that allows REQUEST's tool for the rest of the session.
The program's first suggested rule is used with its destination changed, so
nothing is written to a settings file. Without one, the whole tool is allowed."
  (let ((suggested (find-if (lambda (suggestion)
                              (and (equal (jget suggestion "type") "addRules")
                                   (equal (jget suggestion "behavior") "allow")
                                   (vectorp (jget suggestion "rules"))))
                            (let ((suggestions (request-suggestions request)))
                              (if (vectorp suggestions) suggestions #())))))
    (jobject "type" "addRules"
             "rules" (if suggested
                         (jget suggested "rules")
                         (jarray (jobject "toolName" (request-tool-name request))))
             "behavior" "allow"
             "destination" "session")))

(defun respond-permission (session request decision &key message)
  "Answer REQUEST and return whether the answer was sent.
DECISION is :allow for this one call, :always to also allow the same thing for
the rest of the session, or :deny. MESSAGE tells the model why it was denied.
A request that was already answered or cancelled is left alone."
  (let ((frame nil))
    (changing-session (session)
      (unless (and (eq (request-state request) :pending)
                   (gethash (request-id request) (slot-value session 'permissions)))
        (return-from respond-permission nil))
      (remhash (request-id request) (slot-value session 'permissions))
      (setf (request-state request) (if (eq decision :deny) :denied :allowed))
      (let ((input (or (request-input request) (jobject))))
        (setf frame
              (success-frame
               (request-id request)
               (ecase decision
                 (:allow (jobject "behavior" "allow" "updatedInput" input))
                 (:always (jobject "behavior" "allow"
                                   "updatedInput" input
                                   "updatedPermissions" (jarray (session-rule request))))
                 (:deny (jobject "behavior" "deny"
                                 "message" (or message *denied-by-user*)))))))
      (let ((entry (find-tool-entry session (request-tool-use-id request))))
        (when entry
          (setf (tool-status entry) (if (eq decision :deny) :denied :running))
          (notify session :entry-changed entry)))
      (notify session :permission-settled request))
    (send-frame session frame)))

(defun stop-session (session &key (grace 2))
  "End SESSION's program and wait for its reader to finish."
  (let ((child (slot-value session 'child))
        (reader (slot-value session 'reader)))
    (when child
      (with-session (session)
        (setf (slot-value session 'stopping-p) t))
      (child-stop child :grace grace)
      (when (and reader
                 (not (eq reader sb-thread:*current-thread*)))
        (ignore-errors (sb-thread:join-thread reader :default nil :timeout 5))))
    (changing-session (session)
      (unless (eq (session-state session) :dead)
        (set-state session :dead))))
  nil)

(defun wait-until (predicate &key (timeout 10))
  "Call PREDICATE until it returns true or TIMEOUT seconds pass. Return its value."
  (loop with deadline = (+ (get-internal-real-time)
                           (* timeout internal-time-units-per-second))
        for value = (funcall predicate)
        do (cond
             (value (return value))
             ((> (get-internal-real-time) deadline) (return nil)))
           (sleep 0.01)))
