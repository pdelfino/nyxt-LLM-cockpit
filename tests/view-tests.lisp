;;;; SPDX-FileCopyrightText: Pedro Delfino
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; Tests of the view layer: what the page is given is text, never markup.

(in-package :nyxt-llm-cockpit/tests)

(defparameter *hostile*
  "<script>alert(1)</script><img src=x onerror=\"alert(2)\">&'"
  "Text a web page, a file or the model could hand the cockpit.")

(defun markup-free-p (html)
  "Return whether HTML holds none of the hostile text as markup."
  (and (not (search "<script" html))
       (not (search "<img" html))
       (not (search "onerror=\"" html))))

(defun entry-of (class &rest initargs)
  (apply #'make-instance class :id 7 initargs))

(deftest html-is-escaped
  (check-equal "&lt;b&gt; &amp; &quot;q&quot; &#39;s&#39;" (nyxt-llm-cockpit/view:escape-html "<b> & \"q\" 's'"))
  (check-equal "plain" (nyxt-llm-cockpit/view:escape-html "plain"))
  (check-equal "42" (nyxt-llm-cockpit/view:escape-html 42)))

(deftest every-kind-of-entry-escapes-its-text
  (dolist (entry (list (entry-of 'user-entry :text *hostile*)
                       (entry-of 'assistant-entry :text *hostile*)
                       (entry-of 'assistant-entry :text *hostile* :final-p nil)
                       (entry-of 'thinking-entry :text *hostile*)
                       (entry-of 'notice-entry :kind :error :text *hostile*)))
    (let ((html (nyxt-llm-cockpit/view:entry-html entry)))
      (check (markup-free-p html))
      (check (search "&lt;script&gt;alert(1)&lt;/script&gt;" html))
      (check (search "id=\"entry-7\"" html))
      (check (search "id=\"entry-7-body\"" html)))))

(deftest a-tool-entry-escapes-name-input-and-result
  (let ((entry (entry-of 'tool-entry
                         :tool-use-id "toolu_1"
                         :name *hostile*
                         :input (jobject "command" *hostile* *hostile* *hostile*)
                         :status :failed)))
    (setf (tool-result entry) *hostile*
          (tool-error-p entry) t)
    (let ((html (nyxt-llm-cockpit/view:entry-html entry)))
      (check (markup-free-p html))
      (check (search "class=\"entry tool failed\"" html))
      (check (search "<span class=\"state\">failed</span>" html))
      ;; A failure is shown without a click.
      (check (search "<details class=\"result\" open>" html)))))

(deftest a-tool-entry-says-where-it-stands
  (flet ((html-at (status)
           (nyxt-llm-cockpit/view:entry-html
            (entry-of 'tool-entry :tool-use-id "t" :name "Bash"
                                  :input (jobject "command" "ls -la")
                                  :status status))))
    (check (search "waiting for your answer" (html-at :awaiting-permission)))
    (check (search "class=\"entry tool awaiting-permission\"" (html-at :awaiting-permission)))
    (check (search "<span class=\"state\">denied</span>" (html-at :denied)))
    (check (search "<div class=\"summary\">ls -la</div>" (html-at :requested)))
    (check (search "&quot;command&quot;: &quot;ls -la&quot;" (html-at :done)))
    ;; No result yet, no result block.
    (check (not (search "class=\"result\"" (html-at :requested))))))

(deftest a-long-result-is-cut
  (let ((entry (entry-of 'tool-entry :tool-use-id "t" :name "Read" :status :done))
        (nyxt-llm-cockpit/view:*result-limit* 10))
    (setf (tool-result entry) "0123456789abcdef")
    (let ((html (nyxt-llm-cockpit/view:entry-html entry)))
      (check (search ">0123456789</pre>" html))
      (check (not (search "abcdef" html)))
      (check (search "6 more characters not shown" html)))
    (setf (tool-result entry) "short")
    (check (not (search "not shown" (nyxt-llm-cockpit/view:entry-html entry))))))

(deftest entries-say-who-and-whether-live
  (check (search "<div class=\"who\">You</div>"
                 (nyxt-llm-cockpit/view:entry-html (entry-of 'user-entry :text "hi"))))
  (check (search "<div class=\"who\">Claude</div>"
                 (nyxt-llm-cockpit/view:entry-html (entry-of 'assistant-entry :text "hi"))))
  (check (search "class=\"entry assistant live\""
                 (nyxt-llm-cockpit/view:entry-html (entry-of 'assistant-entry :text "h" :final-p nil))))
  (check (search "class=\"entry assistant\""
                 (nyxt-llm-cockpit/view:entry-html (entry-of 'assistant-entry :text "hi"))))
  (check (search "class=\"entry notice turn-end\""
                 (nyxt-llm-cockpit/view:entry-html (entry-of 'notice-entry :kind :turn-end :text "Done"))))
  (check (search "class=\"entry assistant child\""
                 (nyxt-llm-cockpit/view:entry-html (entry-of 'assistant-entry :text "sub" :parent "toolu_9"))))
  (check (search "<details class=\"entry thinking\""
                 (nyxt-llm-cockpit/view:entry-html (entry-of 'thinking-entry :text "hm")))))

(deftest a-transcript-is-its-entries-in-order
  (with-fake-session (session log "hello")
    (send-prompt session "hi <there>")
    (check (wait-until (lambda () (idle-after-turn-p session 1))))
    (let ((html (nyxt-llm-cockpit/view:transcript-html (session-entries session))))
      (check (search "hi &lt;there&gt;" html))
      (check (< (search "hi &lt;there&gt;" html)
                (search "Hello from Nyxt" html)
                (search "Done in 1.2 s" html)))
      (check-equal 4 (count-if (lambda (entry)
                                 (search (format nil "id=\"entry-~d\"" (entry-id entry)) html))
                               (session-entries session))))))

(deftest pretty-json-is-readable-and-still-json
  (let ((value (parse-json "{\"command\":\"ls\",\"list\":[1,{\"a\":true}],\"empty\":{},\"none\":[]}")))
    (check-equal (format nil "{~%  \"command\": \"ls\",~%  \"list\": [~%    1,~%    {~%      \"a\": true~%    }~%  ],~%  \"empty\": {},~%  \"none\": []~%}")
                 (nyxt-llm-cockpit/view:pretty-json value))
    (check-equal (to-json value) (to-json (parse-json (nyxt-llm-cockpit/view:pretty-json value))))))

;;; The status line

(deftest the-status-line-follows-the-session
  (check-equal "ready" (nyxt-llm-cockpit/view:status-text nil))
  (check-equal "ready · sonnet · /srv/project"
               (nyxt-llm-cockpit/view:status-text nil :model "sonnet" :directory "/srv/project"))
  (with-fake-session (session log "hello")
    (check-equal "ready" (nyxt-llm-cockpit/view:status-text session))
    (send-prompt session "hi")
    (check (wait-until (lambda () (idle-after-turn-p session 1))))
    ;; 1010 of 200000 tokens rounds to 1%.
    (check-equal "ready · claude-fake-1 · /tmp/fake · $0.0025 · context 1%"
                 (nyxt-llm-cockpit/view:status-text session))
    (stop-session session :grace 1)
    (check (search "ended" (nyxt-llm-cockpit/view:status-text session)))))

(deftest the-status-line-says-when-an-answer-is-needed
  (with-fake-session (session log "permission")
    (send-prompt session "make the file")
    (let ((request (wait-for-permission session)))
      (check (search "waiting for your answer" (nyxt-llm-cockpit/view:status-text session)))
      (respond-permission session request :deny)
      (check (wait-until (lambda () (idle-after-turn-p session 1))))
      (check (not (search "waiting" (nyxt-llm-cockpit/view:status-text session)))))))

(deftest the-home-directory-is-shortened
  (let ((home (string-right-trim "/" (namestring (user-homedir-pathname)))))
    (check-equal "ready · ~/projects/x"
                 (nyxt-llm-cockpit/view:status-text nil :directory (format nil "~a/projects/x/" home)))
    (check-equal "ready · ~" (nyxt-llm-cockpit/view:status-text nil :directory home))
    (check-equal (format nil "ready · ~a-other" home)
                 (nyxt-llm-cockpit/view:status-text nil :directory (format nil "~a-other" home)))))

;;; Permissions

(deftest only-an-explicit-yes-allows
  (dolist (answer '("y" "Y" "yes" " YES " "Yes"))
    (check-equal :allow (nyxt-llm-cockpit/view:parse-permission-answer answer)))
  (dolist (answer '("a" "A" "always" " Always "))
    (check-equal :always (nyxt-llm-cockpit/view:parse-permission-answer answer)))
  ;; Everything else is no: empty, cancelled, a typo, a longer sentence.
  (dolist (answer (list "" " " "n" "no" "ye" "yy" "yes please" "allow" "ok" "1" "true"
                        nil :yes 1 "y; rm -rf" *hostile*))
    (check-equal :deny (nyxt-llm-cockpit/view:parse-permission-answer answer))))

(deftest the-question-names-the-tool-on-one-line
  (with-fake-session (session log "permission")
    (send-prompt session "make the file")
    (let* ((request (wait-for-permission session))
           (question (nyxt-llm-cockpit/view:permission-question request)))
      (check (search "Allow Bash: touch marker.txt?" question))
      (check (search "y for once" question))
      (check (not (find #\Newline question)))
      ;; Nyxt strips a trailing colon from a prompt and warns about it.
      (check (not (char= #\: (char question (1- (length question))))))
      (respond-permission session request :deny))))

;;; The page script

(deftest a-script-call-carries-text-as-text
  (check-equal "window.llmCockpit&&window.llmCockpit.focus();"
               (nyxt-llm-cockpit/view:script-call "focus"))
  (check-equal "window.llmCockpit&&window.llmCockpit.text(\"entry-3-body\",\"a \\\"quoted\\\" line\\nnext\");"
               (nyxt-llm-cockpit/view:script-call "text" "entry-3-body" (format nil "a \"quoted\" line~%next")))
  ;; Whatever the text holds, the script stays one statement on one line.
  (let ((script (nyxt-llm-cockpit/view:script-call
                 "text" "id" (format nil "\"); alert(1); (\"~%~a~a" (code-char #x2028) *hostile*))))
    (check (not (find #\Newline script)))
    (check (not (find (code-char #x2028) script)))
    (check (search "\\\"); alert(1); (\\\"" script))
    (check (char= #\; (char script (1- (length script)))))))

(deftest the-runtime-script-only-receives
  (let ((script nyxt-llm-cockpit/view:*runtime-script*))
    (dolist (name '("append" "replace" "text" "status" "take" "restore" "focus"))
      (check (search (format nil "~a: function" name) script)))
    ;; The page never calls Lisp: no request leaves it.
    (dolist (forbidden '("fetch(" "XMLHttpRequest" "lisp:" "nyxt:" "WebSocket" "eval(" "innerHTML ="))
      (check (not (search forbidden script))))))
