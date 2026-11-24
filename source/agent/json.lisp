;;;; SPDX-FileCopyrightText: Pedro Delfino
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; A JSON reader and writer with no dependencies.
;;;;
;;;;   JSON object      <-> hash table with `equal' string keys
;;;;   JSON array       <-> vector (not a string)
;;;;   JSON string      <-> string
;;;;   JSON number      <-> integer or double-float
;;;;   true/false/null  <-> :true / :false / :null
;;;;
;;;; The mapping loses nothing, because a tool's input is read from the
;;;; program and sent back to it unchanged when the user allows the tool.
;;;; When writing, NIL is also null and a non-empty list is also an array.

(in-package :nyxt-llm-cockpit/agent)

(define-condition json-error (error)
  ((message :initarg :message :reader json-error-message)
   (index :initarg :index :initform nil :reader json-error-index))
  (:report (lambda (condition stream)
             (format stream "Bad JSON~@[ at character ~d~]: ~a"
                     (json-error-index condition)
                     (json-error-message condition))))
  (:documentation "Signaled when text is not JSON, or a value cannot be written as JSON."))

(defconstant +max-depth+ 256
  "How deep arrays and objects may nest before the reader gives up.")

(defconstant +max-exponent+ 400
  "The largest exponent the reader computes. A double holds about 308.")

;;; Building and reading values

(defun jobject (&rest pairs)
  "Return a JSON object from PAIRS, as in (jobject \"key\" value ...)."
  (let ((object (make-hash-table :test 'equal)))
    (loop for (key value) on pairs by #'cddr
          do (setf (gethash key object) value))
    object))

(defun jarray (&rest items)
  "Return a JSON array holding ITEMS."
  (coerce items 'simple-vector))

(defun jget (value &rest path)
  "Follow PATH into VALUE and return what is there, or NIL.
A string in PATH is an object key and an integer is an array index.
A missing key, an index out of range and a JSON null all return NIL."
  (dolist (step path)
    (setf value
          (cond
            ((and (stringp step) (hash-table-p value))
             (gethash step value))
            ((and (integerp step) (vectorp value) (not (stringp value))
                  (<= 0 step) (< step (length value)))
             (aref value step))
            (t nil))))
  (if (eq value :null) nil value))

(defun jtrue-p (value)
  "Return whether VALUE is the JSON true."
  (eq value :true))

;;; Reading

(defun parse-json (string)
  "Return the value of the JSON text STRING. Signal `json-error' when it is not JSON."
  (let ((index 0)
        (end (length string)))
    (labels
        ((fail (control &rest arguments)
           (error 'json-error :message (apply #'format nil control arguments)
                              :index index))
         (skip-space ()
           (loop while (and (< index end)
                            (member (char string index)
                                    '(#\Space #\Tab #\Newline #\Return)))
                 do (incf index)))
         (literal (text value)
           (let ((stop (+ index (length text))))
             (if (and (<= stop end) (string= text string :start2 index :end2 stop))
                 (progn (setf index stop) value)
                 (fail "expected ~a" text))))
         (hex4 ()
           (when (> (+ index 4) end)
             (fail "a \\u escape is cut short"))
           (let ((code (parse-integer string :start index :end (+ index 4)
                                             :radix 16 :junk-allowed t)))
             (unless (and code
                          (every (lambda (character) (digit-char-p character 16))
                                 (subseq string index (+ index 4))))
               (fail "a \\u escape is not four hex digits"))
             (incf index 4)
             code))
         (escape (out)
           (when (>= index end)
             (fail "a string ends inside an escape"))
           (let ((character (char string index)))
             (incf index)
             (case character
               (#\n (write-char #\Newline out))
               (#\t (write-char #\Tab out))
               (#\r (write-char #\Return out))
               (#\b (write-char #\Backspace out))
               (#\f (write-char #\Page out))
               ((#\" #\\ #\/) (write-char character out))
               (#\u
                (let ((code (hex4)))
                  (cond
                    ;; A character outside the basic plane is two escapes.
                    ((and (<= #xD800 code #xDBFF)
                          (< (1+ index) end)
                          (char= (char string index) #\\)
                          (char= (char string (1+ index)) #\u))
                     (incf index 2)
                     (let ((low (hex4)))
                       (if (<= #xDC00 low #xDFFF)
                           (write-char (code-char (+ #x10000
                                                     (ash (- code #xD800) 10)
                                                     (- low #xDC00)))
                                       out)
                           (progn
                             (write-char (code-char #xFFFD) out)
                             (write-char (if (<= #xD800 low #xDFFF)
                                             (code-char #xFFFD)
                                             (code-char low))
                                         out)))))
                    ;; Half a pair is not a character.
                    ((<= #xD800 code #xDFFF)
                     (write-char (code-char #xFFFD) out))
                    (t
                     (write-char (code-char code) out)))))
               (t (fail "unknown escape \\~a" character)))))
         (read-string ()
           (incf index)                 ; the opening quote
           (with-output-to-string (out)
             (loop
               (let ((stop (position-if (lambda (character)
                                          (or (char= character #\")
                                              (char= character #\\)))
                                        string :start index)))
                 (unless stop
                   (fail "a string is not closed"))
                 (write-string string out :start index :end stop)
                 (setf index (1+ stop))
                 (if (char= (char string stop) #\")
                     (return)
                     (escape out))))))
         (digits ()
           (let ((start index))
             (loop while (and (< index end) (digit-char-p (char string index)))
                   do (incf index))
             (when (= start index)
               (fail "expected a digit"))
             (values (parse-integer string :start start :end index)
                     (- index start))))
         (read-number ()
           (let ((sign 1) (fraction 0) (fraction-digits 0) (exponent 0) (float-p nil))
             (when (char= (char string index) #\-)
               (setf sign -1)
               (incf index))
             (let ((whole (digits)))
               (when (and (< index end) (char= (char string index) #\.))
                 (incf index)
                 (setf float-p t)
                 (multiple-value-setq (fraction fraction-digits) (digits)))
               (when (and (< index end) (char-equal (char string index) #\e))
                 (incf index)
                 (setf float-p t)
                 (let ((exponent-sign 1))
                   (when (and (< index end) (find (char string index) "+-"))
                     (when (char= (char string index) #\-)
                       (setf exponent-sign -1))
                     (incf index))
                   (setf exponent (* exponent-sign (digits)))))
               (cond
                 ((not float-p) (* sign whole))
                 ((> (abs exponent) +max-exponent+)
                  (fail "the exponent ~d is too large" exponent))
                 (t
                  (handler-case
                      (coerce (* sign
                                 (+ whole (/ fraction (expt 10 fraction-digits)))
                                 (expt 10 exponent))
                              'double-float)
                    (arithmetic-error ()
                      (fail "the number does not fit a double"))))))))
         (read-array (depth)
           (incf index)
           (skip-space)
           (let ((items '()))
             (if (and (< index end) (char= (char string index) #\]))
                 (incf index)
                 (loop
                   (push (read-value depth) items)
                   (skip-space)
                   (when (>= index end)
                     (fail "an array is not closed"))
                   (let ((character (char string index)))
                     (incf index)
                     (case character
                       (#\] (return))
                       (#\, nil)
                       (t (fail "expected , or ] in an array"))))))
             (coerce (nreverse items) 'simple-vector)))
         (read-object (depth)
           (incf index)
           (skip-space)
           (let ((object (make-hash-table :test 'equal)))
             (if (and (< index end) (char= (char string index) #\}))
                 (incf index)
                 (loop
                   (skip-space)
                   (unless (and (< index end) (char= (char string index) #\"))
                     (fail "expected a key"))
                   (let ((key (read-string)))
                     (skip-space)
                     (unless (and (< index end) (char= (char string index) #\:))
                       (fail "expected : after a key"))
                     (incf index)
                     (setf (gethash key object) (read-value depth)))
                   (skip-space)
                   (when (>= index end)
                     (fail "an object is not closed"))
                   (let ((character (char string index)))
                     (incf index)
                     (case character
                       (#\} (return))
                       (#\, nil)
                       (t (fail "expected , or } in an object"))))))
             object))
         (read-value (depth)
           (when (> depth +max-depth+)
             (fail "nested deeper than ~d" +max-depth+))
           (skip-space)
           (when (>= index end)
             (fail "unexpected end"))
           (let ((character (char string index)))
             (cond
               ((char= character #\{) (read-object (1+ depth)))
               ((char= character #\[) (read-array (1+ depth)))
               ((char= character #\") (read-string))
               ((char= character #\t) (literal "true" :true))
               ((char= character #\f) (literal "false" :false))
               ((char= character #\n) (literal "null" :null))
               ((or (char= character #\-) (digit-char-p character)) (read-number))
               (t (fail "unexpected ~s" character))))))
      (let ((value (read-value 0)))
        (skip-space)
        (unless (= index end)
          (fail "text after the value"))
        value))))

;;; Writing

(defun write-json-string (string stream)
  "Write STRING to STREAM as a JSON string, on one line."
  (write-char #\" stream)
  (loop for character across string
        for code = (char-code character)
        do (case character
             (#\" (write-string "\\\"" stream))
             (#\\ (write-string "\\\\" stream))
             (#\Newline (write-string "\\n" stream))
             (#\Return (write-string "\\r" stream))
             (#\Tab (write-string "\\t" stream))
             (t (cond
                  ;; Control characters, and the two line separators that
                  ;; some readers of line-delimited JSON split on.
                  ((or (< code 32) (= code 127) (= code #x2028) (= code #x2029))
                   (format stream "\\u~4,'0x" code))
                  ;; Half a surrogate pair cannot be encoded as UTF-8.
                  ((<= #xD800 code #xDFFF)
                   (write-string "\\ufffd" stream))
                  (t (write-char character stream))))))
  (write-char #\" stream))

(defun write-json (value stream)
  "Write VALUE to STREAM as JSON on one line. Signal `json-error' for other values."
  (cond
    ((or (eq value :true) (eq value t)) (write-string "true" stream))
    ((eq value :false) (write-string "false" stream))
    ((or (eq value :null) (null value)) (write-string "null" stream))
    ((stringp value) (write-json-string value stream))
    ((integerp value) (format stream "~d" value))
    ((floatp value)
     (let ((double (coerce value 'double-float)))
       (when (or (sb-ext:float-infinity-p double) (sb-ext:float-nan-p double))
         (error 'json-error :message "infinity and NaN are not JSON numbers"))
       (let ((*read-default-float-format* 'double-float))
         (prin1 double stream))))
    ((rationalp value) (write-json (coerce value 'double-float) stream))
    ((hash-table-p value)
     (write-char #\{ stream)
     (let ((first-p t))
       (maphash (lambda (key item)
                  (unless (stringp key)
                    (error 'json-error
                           :message (format nil "the key ~s is not a string" key)))
                  (if first-p
                      (setf first-p nil)
                      (write-char #\, stream))
                  (write-json-string key stream)
                  (write-char #\: stream)
                  (write-json item stream))
                value))
     (write-char #\} stream))
    ((or (vectorp value) (consp value))
     (write-char #\[ stream)
     (let ((first-p t))
       (map nil (lambda (item)
                  (if first-p
                      (setf first-p nil)
                      (write-char #\, stream))
                  (write-json item stream))
            value))
     (write-char #\] stream))
    (t (error 'json-error :message (format nil "cannot write ~s as JSON" value))))
  nil)

(defun to-json (value)
  "Return VALUE as a one-line JSON string."
  (with-output-to-string (stream)
    (write-json value stream)))
