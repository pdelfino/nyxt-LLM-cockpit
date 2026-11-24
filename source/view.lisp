;;;; SPDX-FileCopyrightText: Pedro Delfino
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; How a session looks: HTML for its entries, the status line, the question
;;;; a permission request asks, and the script the page runs.
;;;;
;;;; Everything here is a function from data to a string. No Nyxt, no
;;;; libraries. One rule holds the page together: text that came from the
;;;; model, from a tool or from the user reaches the page only through
;;;; `escape-html', or as an argument of `script-call', which the page sets
;;;; as text. None of it is ever markup.

(uiop:define-package :nyxt-llm-cockpit/view
  (:use :cl :nyxt-llm-cockpit/agent)
  (:export
   #:escape-html
   #:entry-dom-id
   #:entry-body-id
   #:entry-html
   #:transcript-html
   #:status-text
   #:state-word
   #:permission-question
   #:parse-permission-answer
   #:pretty-json
   #:script-call
   #:*runtime-script*
   #:*result-limit*)
  (:documentation "Turn a session into strings a page can show."))
(in-package :nyxt-llm-cockpit/view)

(defparameter *result-limit* 4000
  "How many characters of a tool's result the page shows.")

(defun escape-html (text)
  "Return TEXT with the five characters HTML reads as markup replaced."
  (with-output-to-string (out)
    (loop for character across (if (stringp text) text (princ-to-string text))
          do (case character
               (#\& (write-string "&amp;" out))
               (#\< (write-string "&lt;" out))
               (#\> (write-string "&gt;" out))
               (#\" (write-string "&quot;" out))
               (#\' (write-string "&#39;" out))
               (t (write-char character out))))))

(defun pretty-json (value &optional (indent 0))
  "Return VALUE as JSON over several lines, two spaces a level."
  (flet ((pad (level)
           (make-string (* 2 level) :initial-element #\Space)))
    (cond
      ((and (hash-table-p value) (plusp (hash-table-count value)))
       (with-output-to-string (out)
         (format out "{~%")
         (let ((first-p t))
           (maphash (lambda (key item)
                      (unless first-p
                        (format out ",~%"))
                      (setf first-p nil)
                      (format out "~a~a: ~a" (pad (1+ indent)) (to-json key)
                              (pretty-json item (1+ indent))))
                    value))
         (format out "~%~a}" (pad indent))))
      ((and (vectorp value) (not (stringp value)) (plusp (length value)))
       (with-output-to-string (out)
         (format out "[~%")
         (loop for item across value
               for first-p = t then nil
               do (unless first-p
                    (format out ",~%"))
                  (format out "~a~a" (pad (1+ indent)) (pretty-json item (1+ indent))))
         (format out "~%~a]" (pad indent))))
      (t (to-json value)))))

;;; Entries

(defun entry-dom-id (entry)
  "Return the id of ENTRY's element in the page."
  (format nil "entry-~d" (entry-id entry)))

(defun entry-body-id (entry)
  "Return the id of the element that holds ENTRY's text."
  (format nil "entry-~d-body" (entry-id entry)))

(defun tool-state-words (status)
  (ecase status
    (:proposed "preparing")
    (:requested "running")
    (:awaiting-permission "waiting for your answer")
    (:running "running")
    (:done "done")
    (:failed "failed")
    (:denied "denied")))

(defun shortened (text limit)
  "Return TEXT cut to LIMIT characters, and how many were cut."
  (if (> (length text) limit)
      (values (subseq text 0 limit) (- (length text) limit))
      (values text 0)))

(defun child-class (entry)
  (if (entry-parent entry) " child" ""))

(defgeneric entry-html (entry)
  (:documentation "Return the HTML of ENTRY as a string. Its text is escaped."))

(defun text-block (entry class who)
  (format nil "<div class=\"entry ~a~a~a\" id=\"~a\">~@[<div class=\"who\">~a</div>~]<div class=\"body\" id=\"~a\">~a</div></div>"
          class
          (child-class entry)
          (if (entry-final-p entry) "" " live")
          (entry-dom-id entry)
          who
          (entry-body-id entry)
          (escape-html (entry-text entry))))

(defmethod entry-html ((entry user-entry))
  (text-block entry "user" "You"))

(defmethod entry-html ((entry assistant-entry))
  (text-block entry "assistant" "Claude"))

(defmethod entry-html ((entry notice-entry))
  (text-block entry (format nil "notice ~(~a~)" (notice-kind entry)) nil))

(defmethod entry-html ((entry thinking-entry))
  (format nil "<details class=\"entry thinking~a~a\" id=\"~a\"><summary>Thinking</summary><div class=\"body\" id=\"~a\">~a</div></details>"
          (child-class entry)
          (if (entry-final-p entry) "" " live")
          (entry-dom-id entry)
          (entry-body-id entry)
          (escape-html (entry-text entry))))

(defmethod entry-html ((entry tool-entry))
  (let ((status (tool-status entry))
        (input (tool-input entry))
        (result (tool-result entry)))
    (with-output-to-string (out)
      (format out "<div class=\"entry tool ~(~a~)~a\" id=\"~a\">"
              status (child-class entry) (entry-dom-id entry))
      (format out "<div class=\"who\">~a <span class=\"state\">~a</span></div>"
              (escape-html (tool-name entry))
              (escape-html (tool-state-words status)))
      (let ((summary (tool-summary (tool-name entry) input)))
        (unless (string= summary (tool-name entry))
          (format out "<div class=\"summary\">~a</div>"
                  (escape-html (subseq summary (min (length summary)
                                                    (+ 2 (length (tool-name entry)))))))))
      (when input
        (format out "<details class=\"input\"><summary>Input</summary><pre>~a</pre></details>"
                (escape-html (pretty-json input))))
      (when (and result (plusp (length result)))
        (multiple-value-bind (shown cut) (shortened result *result-limit*)
          (format out "<details class=\"result\"~:[~; open~]><summary>Result</summary><pre id=\"~a\">~a</pre>~[~:;<div class=\"cut\">~:*~d more character~:p not shown</div>~]</details>"
                  (tool-error-p entry)
                  (entry-body-id entry)
                  (escape-html shown)
                  cut)))
      (write-string "</div>" out))))

(defun transcript-html (entries)
  "Return the HTML of ENTRIES, a sequence, oldest first."
  (with-output-to-string (out)
    (map nil (lambda (entry)
               (write-string (entry-html entry) out))
         entries)))

;;; The status line

(defun state-word (state)
  "Return the word the page shows for a session STATE."
  (ecase state
    ((nil :new) "ready")
    (:idle "ready")
    (:running "working")
    (:dead "ended")))

(defun home-relative (path)
  "Return PATH with the home directory written as ~."
  (let ((home (string-right-trim "/" (namestring (user-homedir-pathname))))
        (path (string-right-trim "/" (if (pathnamep path) (namestring path) path))))
    (cond
      ((string= path home) "~")
      ((and (> (length path) (length home))
            (string= home path :end2 (length home))
            (char= (char path (length home)) #\/))
       (concatenate 'string "~" (subseq path (length home))))
      (t path))))

(defun status-text (session &key directory model)
  "Return one line saying where SESSION stands.
DIRECTORY and MODEL are what the cockpit is set to, shown before the program
has said what it uses. SESSION may be NIL."
  (let* ((info (and session (session-info session)))
         (state (and session (session-state session)))
         (pending (if session (length (session-pending-permissions session)) 0))
         (model (or (jget info "model") model))
         (directory (or (jget info "cwd") directory))
         (cost (if session (session-total-cost session) 0))
         (tokens (and session (session-context-tokens session)))
         (window (and session (session-context-window session))))
    (format nil "~a~@[ · ~a~]~@[ · ~a~]~@[ · $~,4f~]~@[ · context ~d%~]"
            (if (plusp pending)
                (format nil "waiting for your answer~[~;~:; (~:*~d)~]" pending)
                (state-word state))
            model
            (and directory (home-relative directory))
            (and (realp cost) (plusp cost) cost)
            (and tokens window (plusp window)
                 (max 1 (round (* 100 tokens) window))))))

;;; Permissions

(defun permission-question (request)
  "Return the one line that asks the user whether REQUEST's tool may run."
  (format nil "Allow ~a? Type y for once, a for the rest of this session. Anything else is no"
          (tool-summary (request-tool-name request) (request-input request))))

(defun parse-permission-answer (answer)
  "Return :allow, :always or :deny for what the user typed.
Only an explicit yes allows. An empty answer, a cancelled prompt and anything
unexpected deny."
  (let ((word (and (stringp answer)
                   (string-downcase (string-trim '(#\Space #\Tab #\Newline #\Return)
                                                 answer)))))
    (cond
      ((member word '("y" "yes") :test #'equal) :allow)
      ((member word '("a" "always") :test #'equal) :always)
      (t :deny))))

;;; The page's script

(defparameter *runtime-script*
  "(function () {
  if (window.llmCockpit) { return; }
  function scroller() { return document.scrollingElement || document.documentElement; }
  function near() { var e = scroller(); return e.scrollHeight - e.scrollTop - e.clientHeight < 160; }
  function down() { var e = scroller(); e.scrollTop = e.scrollHeight; }
  function input() { return document.getElementById('cockpit-input'); }
  window.llmCockpit = {
    append: function (html) {
      var transcript = document.getElementById('transcript');
      if (!transcript) { return; }
      var stick = near();
      transcript.insertAdjacentHTML('beforeend', html);
      if (stick) { down(); }
    },
    replace: function (id, html) {
      var old = document.getElementById(id);
      if (!old) { return; }
      var stick = near();
      var open = [];
      var parts = old.querySelectorAll('details');
      for (var i = 0; i < parts.length; i++) { open.push(parts[i].open); }
      var wasOpen = old.tagName === 'DETAILS' && old.open;
      old.insertAdjacentHTML('afterend', html);
      var fresh = old.nextElementSibling;
      old.remove();
      if (fresh) {
        if (wasOpen) { fresh.open = true; }
        var now = fresh.querySelectorAll('details');
        for (var j = 0; j < now.length && j < open.length; j++) {
          if (open[j]) { now[j].open = true; }
        }
      }
      if (stick) { down(); }
    },
    text: function (id, text) {
      var node = document.getElementById(id);
      if (!node) { return; }
      var stick = near();
      node.textContent = text;
      if (stick) { down(); }
    },
    status: function (text, state) {
      var node = document.getElementById('cockpit-status');
      if (!node) { return; }
      node.textContent = text;
      node.className = 'state-' + state;
    },
    take: function () {
      var field = input();
      if (!field) { return ''; }
      var value = field.value;
      field.value = '';
      return value;
    },
    restore: function (text) {
      var field = input();
      if (field && !field.value) { field.value = text; }
    },
    focus: function () {
      var tries = 0;
      (function attempt() {
        var field = input();
        if (field) { field.focus(); down(); }
        else if (tries++ < 50) { window.setTimeout(attempt, 30); }
      })();
    }
  };
})();"
  "The script the cockpit page runs once. It defines window.llmCockpit,
whose functions change the page. It starts nothing and calls nothing back:
the page never talks to Lisp, Lisp talks to the page.")

(defun script-call (function &rest arguments)
  "Return a script that calls the page function FUNCTION with ARGUMENTS.
Each argument is written as a JSON literal, so text stays text whatever it holds."
  (format nil "window.llmCockpit&&window.llmCockpit.~a(~{~a~^,~});"
          function
          (mapcar #'to-json arguments)))
