;;;; SPDX-FileCopyrightText: Pedro Delfino
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; A test harness in forty lines, so the tests need no library.

(uiop:define-package :nyxt-llm-cockpit/tests
  (:use :cl :nyxt-llm-cockpit/agent)
  (:export #:run-tests #:deftest #:check #:check-equal))
(in-package :nyxt-llm-cockpit/tests)

(defvar *tests* '()
  "Test names, newest first.")

(defvar *checks* 0)

(defvar *failures* '()
  "Strings saying what failed, newest first.")

(defvar *current-test* nil)

(defun note-failure (control &rest arguments)
  (push (format nil "~a: ~?" *current-test* control arguments) *failures*))

(defmacro check (form)
  "Count FORM as a check. It fails when FORM returns NIL."
  `(progn
     (incf *checks*)
     (unless ,form
       (note-failure "~s was false" ',form))))

(defmacro check-equal (expected form)
  "Count a check that FORM returns a value `equal' to EXPECTED."
  (let ((expected-value (gensym "EXPECTED"))
        (actual-value (gensym "ACTUAL")))
    `(let ((,expected-value ,expected)
           (,actual-value ,form))
       (incf *checks*)
       (unless (equal ,expected-value ,actual-value)
         (note-failure "~s~%      expected ~s~%      got      ~s"
                       ',form ,expected-value ,actual-value)))))

(defmacro deftest (name &body body)
  "Define the test NAME. An error inside BODY is a failure, not a crash."
  `(progn
     (defun ,name ()
       (let ((*current-test* ',name))
         (handler-case (progn ,@body)
           (error (condition)
             (note-failure "signaled ~a" condition)))))
     (pushnew ',name *tests*)
     ',name))

(defun run-tests (&key (package nil))
  "Run every test, or the tests of PACKAGE. Return whether all checks passed."
  (setf *checks* 0
        *failures* '())
  (let ((tests (remove-if-not (lambda (name)
                                (or (null package)
                                    (eq (symbol-package name) (find-package package))))
                              (reverse *tests*))))
    (dolist (test tests)
      (funcall test))
    (format t "~&~d test~:p, ~d check~:p, ~d failure~:p~%"
            (length tests) *checks* (length *failures*))
    (dolist (failure (reverse *failures*))
      (format t "  FAIL ~a~%" failure))
    (null *failures*)))
