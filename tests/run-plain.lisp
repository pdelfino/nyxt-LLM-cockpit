;;;; SPDX-FileCopyrightText: Pedro Delfino
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; Load the agent and view layers and run their tests in a plain SBCL.
;;;; Usage, from the repository root: sbcl --script tests/run-plain.lisp

(require :asdf)

(let ((root (uiop:pathname-parent-directory-pathname
             (uiop:pathname-directory-pathname *load-truename*))))
  (asdf:load-asd (merge-pathnames "nyxt-llm-cockpit.asd" root)))

(defun fail (control &rest arguments)
  (format t "~&~?~%" control arguments)
  (uiop:quit 2))

(let ((warnings '()))
  (handler-case
      (handler-bind ((warning (lambda (condition)
                                ;; Loading a file after compiling it defines
                                ;; its macros a second time. That is not news.
                                (unless (typep condition 'sb-kernel:redefinition-warning)
                                  (push (princ-to-string condition) warnings))
                                (muffle-warning condition))))
        (let ((*standard-output* (make-broadcast-stream))
              (*error-output* (make-broadcast-stream)))
          (asdf:load-system :nyxt-llm-cockpit/plain-tests :force t)))
    (error (condition)
      (fail "The plain layers did not load: ~a" condition)))
  (when warnings
    (fail "The plain layers compiled with warnings:~%~{  ~a~%~}" (reverse warnings))))

(uiop:quit (if (uiop:symbol-call :nyxt-llm-cockpit/tests :run-tests) 0 1))
