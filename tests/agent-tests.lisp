;;;; SPDX-FileCopyrightText: Pedro Delfino
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; Tests of the agent layer. They run tests/fake-claude, so they use no
;;;; network and cost nothing.

(in-package :nyxt-llm-cockpit/tests)

(defparameter *fake-program*
  (namestring (asdf:system-relative-pathname :nyxt-llm-cockpit/agent "tests/fake-claude")))

(defun read-log (path)
  "Return the lines the stand-in program recorded."
  (if (probe-file path)
      (uiop:read-file-lines path :external-format :utf-8)
      '()))

(defun log-lines (path prefix)
  "Return the recorded lines that start with PREFIX, without it."
  (loop for line in (read-log path)
        when (and (>= (length line) (length prefix))
                  (string= prefix line :end2 (length prefix)))
          collect (subseq line (length prefix))))

(defmacro with-fake-session ((session log scenario &rest initargs) &body body)
  "Run BODY with SESSION talking to the stand-in program playing SCENARIO.
LOG is bound to the file of what the stand-in read."
  `(uiop:with-temporary-file (:pathname ,log :prefix "cockpit-fake" :type "log")
     (let ((,session (make-instance 'session
                                    :program *fake-program*
                                    :extra-arguments (list "--scenario" ,scenario
                                                           "--log" (namestring ,log))
                                    ,@initargs)))
       (unwind-protect (progn ,@body)
         (stop-session ,session :grace 1)))))

(defun idle-after-turn-p (session turns)
  (and (>= (session-turns session) turns)
       (member (session-state session) '(:idle :dead))))

(defun entries-of (session class)
  (remove-if-not (lambda (entry) (typep entry class))
                 (coerce (session-entries session) 'list)))

(defun texts-of (session class)
  (mapcar #'entry-text (entries-of session class)))

(defun search-in (needle haystack)
  (and (stringp haystack) (search needle haystack) t))

;;; JSON

(deftest json-reads-values
  (check-equal 42 (parse-json "42"))
  (check-equal -7 (parse-json " -7 "))
  (check-equal 1.5d0 (parse-json "1.5"))
  (check-equal 0.0026915999999999997d0 (parse-json "0.0026915999999999997"))
  (check-equal 1200.0d0 (parse-json "1.2e3"))
  (check-equal 0.012d0 (parse-json "1.2E-2"))
  (check-equal :true (parse-json "true"))
  (check-equal :false (parse-json "false"))
  (check-equal :null (parse-json "null"))
  (check-equal "a\"b\\c/d" (parse-json "\"a\\\"b\\\\c\\/d\""))
  (check-equal (format nil "line~%tab~cend" #\Tab) (parse-json "\"line\\ntab\\tend\""))
  (check-equal "café" (parse-json "\"caf\\u00e9\""))
  (check-equal (string (code-char #x1F600)) (parse-json "\"\\ud83d\\ude00\""))
  (check-equal (string (code-char #xFFFD)) (parse-json "\"\\ud83d\""))
  (check (equalp #(1 "two" :null) (parse-json "[1, \"two\", null]")))
  (check (equalp #() (parse-json "[]")))
  (let ((object (parse-json "{\"a\": {\"b\": [10, 20, {\"c\": \"deep\"}]}, \"n\": null}")))
    (check-equal "deep" (jget object "a" "b" 2 "c"))
    (check-equal 20 (jget object "a" "b" 1))
    (check-equal nil (jget object "a" "b" 9))
    (check-equal nil (jget object "missing" "deeper"))
    (check-equal nil (jget object "n"))
    (check-equal nil (jget "a string" "key"))))

(deftest json-refuses-what-is-not-json
  (dolist (text '("" "{" "[1," "{\"a\" 1}" "tru" "\"open" "1 2" "{\"a\":1,}" "nope"
                  "\"bad \\q escape\"" "1e999" "-"))
    (check (handler-case (progn (parse-json text) nil)
             (json-error () t))))
  (check (handler-case (progn (parse-json (make-string 400 :initial-element #\[)) nil)
           (json-error () t))))

(deftest json-writes-values
  (check-equal "{\"a\":1,\"b\":[true,false,null],\"c\":\"x\"}"
               (to-json (jobject "a" 1 "b" (jarray :true :false :null) "c" "x")))
  (check-equal "null" (to-json nil))
  (check-equal "true" (to-json t))
  (check-equal "[]" (to-json #()))
  (check-equal "[1,2]" (to-json (list 1 2)))
  (check-equal "{}" (to-json (jobject)))
  (check-equal "1.5" (to-json 1.5d0))
  (check-equal "\"a\\\"b\\\\c\\nd\\te\\u0001\"" (to-json (format nil "a\"b\\c~%d~ce~c" #\Tab (code-char 1))))
  ;; One line, whatever the text holds: the protocol is one frame per line.
  (check (not (find #\Newline (to-json (jobject "text" (format nil "one~%two~c~%" #\Return))))))
  (check (not (find (code-char #x2028) (to-json (string (code-char #x2028))))))
  (check (handler-case (progn (to-json 'a-symbol) nil)
           (json-error () t))))

(deftest json-round-trips-without-loss
  ;; A tool's input goes back to the program unchanged when it is allowed.
  (dolist (text '("{\"command\":\"touch marker.txt\",\"flag\":false,\"none\":null,\"list\":[],\"n\":3}"
                  "{\"z\":1,\"a\":2,\"m\":{\"y\":[1,{\"x\":true}]}}"
                  "[\"café 😀\",\"quote \\\" slash \\\\\"]"))
    (check-equal text (to-json (parse-json text)))))

;;; The command line

(deftest command-line-carries-the-settings
  (let ((command (session-command
                  (make-instance 'session
                                 :program "/opt/claude/bin/claude"
                                 :model "haiku"
                                 :permission-mode "plan"
                                 :resume "abc"
                                 :setting-sources "project"
                                 :extra-arguments '("--no-session-persistence")))))
    (check-equal "/opt/claude/bin/claude" (first command))
    (check (search '("--output-format" "stream-json") command :test #'equal))
    (check (search '("--input-format" "stream-json") command :test #'equal))
    (check (search '("--permission-prompt-tool" "stdio") command :test #'equal))
    (check (search '("--model" "haiku") command :test #'equal))
    (check (search '("--permission-mode" "plan") command :test #'equal))
    (check (search '("--resume" "abc") command :test #'equal))
    (check (search '("--setting-sources" "project") command :test #'equal))
    (check (member "--include-partial-messages" command :test #'equal))
    (check-equal "--no-session-persistence" (first (last command))))
  (let ((command (session-command (make-instance 'session :program "/x/claude"))))
    (check (not (member "--model" command :test #'equal)))
    (check (not (member "--resume" command :test #'equal)))))

(deftest tool-summary-is-one-line
  (check-equal "Bash: touch marker.txt"
               (tool-summary "Bash" (jobject "command" "touch marker.txt")))
  (check-equal "Read: /tmp/a.txt" (tool-summary "Read" (jobject "file_path" "/tmp/a.txt")))
  (check-equal "Odd" (tool-summary "Odd" (jobject "count" 3)))
  (check-equal "Odd" (tool-summary "Odd" nil))
  (check (not (find #\Newline (tool-summary "Bash" (jobject "command" (format nil "a~%b"))))))
  (check (<= (length (tool-summary "Bash" (jobject "command" (make-string 500 :initial-element #\x))))
             170)))

;;; A whole turn

(deftest a-prompt-streams-a-reply
  (with-fake-session (session log "hello")
    (let ((seen '()))
      (add-listener session
                    (lambda (event &rest arguments)
                      (when (and (eq event :entry-changed)
                                 (typep (first arguments) 'assistant-entry))
                        (push (cons (entry-text (first arguments))
                                    (and (entry-final-p (first arguments)) t))
                              seen))))
      (check-equal :new (session-state session))
      (check (send-prompt session "hi"))
      (check (wait-until (lambda () (idle-after-turn-p session 1))))
      (check-equal :idle (session-state session))
      (check-equal '("hi") (texts-of session 'user-entry))
      (check-equal '("Hello from Nyxt") (texts-of session 'assistant-entry))
      (check (every #'entry-final-p (entries-of session 'assistant-entry)))
      ;; The text arrived in pieces before it was finished.
      (check-equal '(("Hello" . nil) ("Hello from" . nil) ("Hello from Nyxt" . nil)
                     ("Hello from Nyxt" . t))
                   (reverse seen))
      (check-equal "11111111-2222-3333-4444-555555555555" (session-id session))
      (check-equal "claude-fake-1" (jget (session-info session) "model"))
      (check-equal 0.0025d0 (session-total-cost session))
      (check-equal 1010 (session-context-tokens session))
      (check-equal 200000 (session-context-window session))
      (check-equal '(:started :turn-end)
                   (mapcar #'notice-kind (entries-of session 'notice-entry)))
      (check (search-in "0.0.0-fake" (first (texts-of session 'notice-entry))))
      (check-equal "Done in 1.2 s · $0.0025" (second (texts-of session 'notice-entry)))
      (check-equal '() (session-problems session))
      ;; What the program was given.
      (check-equal '("{\"type\":\"user\",\"message\":{\"role\":\"user\",\"content\":[{\"type\":\"text\",\"text\":\"hi\"}]},\"parent_tool_use_id\":null}")
                   (log-lines log "IN "))
      (let ((argv (first (log-lines log "ARGV "))))
        (check (search-in "--output-format stream-json" argv))
        (check (search-in "--input-format stream-json" argv))
        (check (search-in "--permission-prompt-tool stdio" argv))
        (check (search-in "-p " argv))))))

(deftest entries-keep-their-order
  (with-fake-session (session log "hello")
    (send-prompt session "hi")
    (check (wait-until (lambda () (idle-after-turn-p session 1))))
    (let ((ids (map 'list #'entry-id (session-entries session))))
      (check-equal (sort (copy-list ids) #'<) ids)
      (check-equal (length ids) (length (remove-duplicates ids))))
    (check-equal '(user-entry notice-entry assistant-entry notice-entry)
                 (map 'list #'type-of (session-entries session)))))

(deftest two-turns-share-a-session
  (with-fake-session (session log "two-turns")
    (send-prompt session "one")
    (check (wait-until (lambda () (idle-after-turn-p session 1))))
    (send-prompt session "two")
    (check (wait-until (lambda () (idle-after-turn-p session 2))))
    (check-equal '("First" "Second") (texts-of session 'assistant-entry))
    (check-equal 2 (session-turns session))
    (check-equal 0.006d0 (session-total-cost session))
    ;; The program announces itself on every turn. The transcript says it once.
    (check-equal '(:started :turn-end :turn-end)
                 (mapcar #'notice-kind (entries-of session 'notice-entry)))
    (check (search-in "$0.0025" (second (texts-of session 'notice-entry))))
    (check (search-in "$0.0035" (third (texts-of session 'notice-entry))))))

;;; Permissions

(defun wait-for-permission (session)
  (wait-until (lambda () (first (session-pending-permissions session)))))

(deftest a-tool-waits-for-a-yes
  (with-fake-session (session log "permission")
    (let ((asked '()))
      (add-listener session (lambda (event &rest arguments)
                              (when (eq event :permission-requested)
                                (push (first arguments) asked))))
      (send-prompt session "make the file")
      (let ((request (wait-for-permission session))
            (tool (first (entries-of session 'tool-entry))))
        (check request)
        (check-equal 1 (length asked))
        (check-equal "Bash" (request-tool-name request))
        (check-equal "touch marker.txt" (jget (request-input request) "command"))
        (check-equal "Create marker.txt" (request-description request))
        (check-equal "/tmp/fake" (request-blocked-path request))
        (check-equal 2 (length (request-suggestions request)))
        (check-equal :pending (request-state request))
        (check-equal :awaiting-permission (tool-status tool))
        (check-equal "touch marker.txt" (jget (tool-input tool) "command"))
        (check-equal :running (session-state session))
        (check (respond-permission session request :allow))
        (check-equal :allowed (request-state request))
        (check (wait-until (lambda () (idle-after-turn-p session 1))))
        (check-equal :done (tool-status tool))
        (check (not (tool-error-p tool)))
        (check-equal '("done") (texts-of session 'assistant-entry))
        (check-equal '() (session-pending-permissions session))
        ;; Answering twice sends nothing.
        (check (not (respond-permission session request :deny)))
        ;; The input goes back exactly as it came, false, null and empty list included.
        (check-equal "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req-1\",\"response\":{\"behavior\":\"allow\",\"updatedInput\":{\"command\":\"touch marker.txt\",\"description\":\"Create marker.txt\",\"flag\":false,\"none\":null,\"list\":[]}}}}"
                     (second (log-lines log "IN ")))
        (check-equal 2 (length (log-lines log "IN ")))))))

(deftest a-no-reaches-the-model
  (with-fake-session (session log "permission")
    (send-prompt session "make the file")
    (let ((request (wait-for-permission session))
          (tool (first (entries-of session 'tool-entry))))
      (check (respond-permission session request :deny :message "Not in this folder."))
      (check-equal :denied (request-state request))
      (check (wait-until (lambda () (idle-after-turn-p session 1))))
      (check-equal :denied (tool-status tool))
      (check (tool-error-p tool))
      (check-equal "The user said no." (tool-result tool))
      (check-equal '("refused") (texts-of session 'assistant-entry))
      (check-equal "{\"type\":\"control_response\",\"response\":{\"subtype\":\"success\",\"request_id\":\"req-1\",\"response\":{\"behavior\":\"deny\",\"message\":\"Not in this folder.\"}}}"
                   (second (log-lines log "IN "))))))

(deftest always-allows-for-the-session-only
  (with-fake-session (session log "permission")
    (send-prompt session "make the file")
    (let ((request (wait-for-permission session)))
      (check (respond-permission session request :always))
      (check (wait-until (lambda () (idle-after-turn-p session 1))))
      (let* ((sent (parse-json (second (log-lines log "IN "))))
             (updates (jget sent "response" "response" "updatedPermissions")))
        (check-equal "allow" (jget sent "response" "response" "behavior"))
        (check-equal 1 (length updates))
        (check-equal "addRules" (jget updates 0 "type"))
        (check-equal "allow" (jget updates 0 "behavior"))
        ;; Never a settings file: the rule lives as long as the session.
        (check-equal "session" (jget updates 0 "destination"))
        (check-equal "Bash" (jget updates 0 "rules" 0 "toolName"))
        (check-equal "touch:*" (jget updates 0 "rules" 0 "ruleContent"))))))

(deftest a-withdrawn-request-cannot-be-answered
  (with-fake-session (session log "cancelled")
    (let ((settled '()))
      (add-listener session (lambda (event &rest arguments)
                              (when (eq event :permission-settled)
                                (push (first arguments) settled))))
      (send-prompt session "make the file")
      (check (wait-until (lambda () (idle-after-turn-p session 1))))
      (check-equal 1 (length settled))
      (check-equal :cancelled (request-state (first settled)))
      (check-equal '() (session-pending-permissions session))
      (check (not (respond-permission session (first settled) :allow)))
      (check-equal :requested (tool-status (first (entries-of session 'tool-entry))))
      (check-equal 1 (length (log-lines log "IN "))))))

;;; Interrupts, odd input, failures

(deftest a-turn-can-be-interrupted
  (with-fake-session (session log "interrupt")
    (check (not (interrupt-turn session)))
    (send-prompt session "count to a hundred")
    (check (wait-until (lambda ()
                         (equal '("Counting: 1, 2") (texts-of session 'assistant-entry)))))
    (check (not (entry-final-p (first (entries-of session 'assistant-entry)))))
    (check (interrupt-turn session))
    (check (wait-until (lambda () (idle-after-turn-p session 1))))
    (check-equal :idle (session-state session))
    (check (entry-final-p (first (entries-of session 'assistant-entry))))
    (check-equal '(:started :error)
                 (mapcar #'notice-kind (entries-of session 'notice-entry)))
    (check (search-in "aborted_streaming" (second (texts-of session 'notice-entry))))
    (check-equal "{\"type\":\"control_request\",\"request_id\":\"nyxt-1\",\"request\":{\"subtype\":\"interrupt\"}}"
                 (second (log-lines log "IN ")))))

(deftest odd-frames-do-not-stop-the-reader
  (with-fake-session (session log "odd")
    (send-prompt session "hi")
    (check (wait-until (lambda () (idle-after-turn-p session 1))))
    (check-equal (list (format nil "Still here: café ~a" (code-char #x1F600)))
                 (texts-of session 'assistant-entry))
    ;; An empty thinking block makes no entry.
    (check-equal '() (entries-of session 'thinking-entry))
    (check-equal 1 (length (session-problems session)))
    (check (search-in "not JSON" (first (session-problems session))))
    ;; A request this client does not know is refused, not left hanging.
    (let ((answer (parse-json (second (log-lines log "IN ")))))
      (check-equal "control_response" (jget answer "type"))
      (check-equal "error" (jget answer "response" "subtype"))
      (check-equal "req-9" (jget answer "response" "request_id"))
      (check (search-in "something_new" (jget answer "response" "error"))))))

(deftest a-crash-is-reported
  (with-fake-session (session log "crash")
    (send-prompt session "hi")
    (check (wait-until (lambda () (eq (session-state session) :dead))))
    (check (not (session-alive-p session)))
    (let ((notice (first (last (entries-of session 'notice-entry)))))
      (check-equal :error (notice-kind notice))
      (check (search-in "code 3" (entry-text notice)))
      (check (search-in "something broke" (entry-text notice))))
    (check (not (send-prompt session "again")))
    (check-equal '("hi") (texts-of session 'user-entry))))

(deftest a-missing-program-is-reported
  (let ((session (make-instance 'session :program "/nonexistent/claude")))
    (check (not (send-prompt session "hi")))
    (check-equal :dead (session-state session))
    (check-equal '(:error) (mapcar #'notice-kind (entries-of session 'notice-entry)))
    (check (search-in "could not be started" (first (texts-of session 'notice-entry))))
    (stop-session session)))

(deftest stopping-ends-the-program
  (with-fake-session (session log "hello")
    (send-prompt session "hi")
    (check (wait-until (lambda () (idle-after-turn-p session 1))))
    (check (session-alive-p session))
    (stop-session session :grace 1)
    (check (not (session-alive-p session)))
    (check-equal :dead (session-state session))
    (check-equal :stopped (notice-kind (first (last (entries-of session 'notice-entry)))))
    (check-equal "The session was closed."
                 (entry-text (first (last (entries-of session 'notice-entry)))))
    (check (member "EOF" (read-log log) :test #'equal))))

(deftest a-failing-listener-does-not-stop-the-session
  (with-fake-session (session log "hello")
    (add-listener session (lambda (event &rest arguments)
                            (declare (ignore arguments))
                            (when (eq event :turn-ended)
                              (error "listener bug"))))
    (send-prompt session "hi")
    (check (wait-until (lambda () (idle-after-turn-p session 1))))
    (check-equal '("Hello from Nyxt") (texts-of session 'assistant-entry))
    (check (some (lambda (problem) (search-in "listener bug" problem))
                 (session-problems session)))))

(deftest listeners-run-with-the-lock-released
  ;; A listener draws in the browser and may call back into the session. If it
  ;; ran inside the lock, the thread that reads the program could wait on the
  ;; browser while the browser waits on the session.
  (with-fake-session (session log "permission")
    (let ((held 0) (calls 0) (inner '()))
      (add-listener session
                    (lambda (event &rest arguments)
                      (incf calls)
                      (when (sb-thread:holding-mutex-p
                             (slot-value session 'nyxt-llm-cockpit/agent::lock))
                        (incf held))
                      ;; Calling back into the session from a listener works.
                      (when (eq event :permission-requested)
                        (push (respond-permission session (first arguments) :allow)
                              inner))))
      (send-prompt session "make the file")
      (check (wait-until (lambda () (idle-after-turn-p session 1))))
      (check (> calls 5))
      (check-equal 0 held)
      (check-equal '(t) inner)
      (check-equal :done (tool-status (first (entries-of session 'tool-entry)))))))

(deftest find-program-looks-past-the-path
  (check-equal *fake-program* (find-program *fake-program*))
  (check (not (find-program "/nonexistent/dir/claude")))
  (check (not (find-program "no-such-program-for-the-cockpit")))
  (check (find-program "sh")))
