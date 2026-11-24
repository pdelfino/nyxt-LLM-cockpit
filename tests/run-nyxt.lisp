;;;; SPDX-FileCopyrightText: Pedro Delfino
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; Load the cockpit into a Lisp where Nyxt's core is already loaded, and
;;;; run every test. No renderer is loaded, so no window opens.
;;;; `make nyxt-test' loads Nyxt from a source checkout and then this file.

(let ((root (uiop:pathname-parent-directory-pathname
             (uiop:pathname-directory-pathname *load-truename*))))
  (asdf:load-asd (merge-pathnames "nyxt-llm-cockpit.asd" root)))

(defun cockpit-fail (control &rest arguments)
  (format t "~&~?~%" control arguments)
  (uiop:quit 2))

(unless (find-package :nyxt)
  (cockpit-fail "Nyxt is not loaded. Run this through `make nyxt-test'."))

(let ((warnings '()))
  (handler-case
      (handler-bind ((warning (lambda (condition)
                                (unless (typep condition 'sb-kernel:redefinition-warning)
                                  (push (princ-to-string condition) warnings))
                                (muffle-warning condition))))
        (let ((*standard-output* (make-broadcast-stream))
              (*error-output* (make-broadcast-stream)))
          (asdf:load-system :nyxt-llm-cockpit/tests
                            :force '("nyxt-llm-cockpit" "nyxt-llm-cockpit/agent" "nyxt-llm-cockpit/view"
                                     "nyxt-llm-cockpit/plain-tests" "nyxt-llm-cockpit/tests"))))
    (error (condition)
      (cockpit-fail "The cockpit did not load into Nyxt ~a: ~a"
                    (symbol-value (find-symbol "+VERSION+" :nyxt)) condition)))
  (when warnings
    (cockpit-fail "The cockpit compiled with warnings:~%~{  ~a~%~}" (reverse warnings))))

(format t "~&Nyxt ~a, no renderer~%" (symbol-value (find-symbol "+VERSION+" :nyxt)))
(uiop:quit (if (uiop:symbol-call :nyxt-llm-cockpit/tests :run-tests) 0 1))
