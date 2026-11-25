;;;; SPDX-FileCopyrightText: Pedro Delfino
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; Prove that the install lines of README.org load the cockpit in a Nyxt
;;;; whose ASDF is set up like the packaged app's.
;;;;
;;;; The other tests load the cockpit by its path, into a Nyxt loaded from
;;;; source. The packaged app differs in three ways that matter here: every
;;;; system of the image is immutable, Nyxt's own source is not on disk, and an
;;;; extension is found only through the extensions directory. With Nyxt
;;;; 5.1.0's macOS app the first lookup of an extension then failed, and no
;;;; test here could see it.
;;;;
;;;; This is a model of the app's ASDF setup, not the app. `make
;;;; nyxt-install-test' loads Nyxt's core from a source checkout, points
;;;; XDG_DATA_HOME at a folder whose extensions directory holds this
;;;; repository, and then loads this file. The cockpit must not be loaded yet.

(in-package :cl-user)

(defvar *install-failures* 0)
(defvar *install-checks* 0)

(defun install-check (ok control &rest arguments)
  "Count a check, and say what failed when OK is false."
  (incf *install-checks*)
  (unless ok
    (incf *install-failures*)
    (format t "~&FAIL: ~?~%" control arguments))
  ok)

(defun install-stop (control &rest arguments)
  (format t "~&~?~%" control arguments)
  (uiop:quit 2))

(unless (find-package :nyxt)
  (install-stop "Nyxt is not loaded. Run this through `make nyxt-install-test'."))
(when (find-package :nyxt-llm-cockpit)
  (install-stop "The cockpit is already loaded, so loading it would prove nothing."))

(defvar *install-root*
  (uiop:pathname-parent-directory-pathname
   (uiop:pathname-directory-pathname *load-truename*))
  "The top of this repository.")

(defun readme-install-forms ()
  "Return the text of the Lisp blocks in the Install section of README.org."
  (let ((in-section nil)
        (in-block nil))
    (with-output-to-string (out)
      (with-open-file (in (merge-pathnames "README.org" *install-root*))
        (loop for line = (read-line in nil)
              while line
              do (cond
                   ((uiop:string-prefix-p "* " line)
                    (setf in-section (string= line "* Install")
                          in-block nil))
                   ((not in-section))
                   ((uiop:string-prefix-p "#+begin_src lisp" line)
                    (setf in-block t))
                   ((uiop:string-prefix-p "#+end_src" line)
                    (setf in-block nil))
                   (in-block
                    (write-line line out))))))))

;;; As far as ASDF can tell, this Lisp is now the packaged app.

;; What Nyxt's build does before it saves the image.
(map () 'asdf:register-immutable-system (asdf:already-loaded-systems))

;; Nyxt's source is not on disk: Nyxt's own value for "not found".
(defmethod nfiles:resolve :around ((profile nyxt::nyxt-profile)
                                   (directory nyxt::nyxt-source-directory))
  #p"")

;; Only ASDF's own ways to find a system: none of what the test setup added.
(setf asdf:*system-definition-search-functions*
      (remove-if-not (lambda (function)
                       (and (symbolp function)
                            (uiop:string-prefix-p
                             "ASDF" (package-name (symbol-package function)))))
                     asdf:*system-definition-search-functions*))

;; What `nyxt:start' does before it reads the config.
(pushnew 'nyxt:nyxt-source-registry asdf:*default-source-registries*)
(asdf:clear-configuration)

;;; 1. As shipped, the first lookup fails. When this stops failing, Nyxt has
;;;    fixed it and the repair in README.org can go.

(let ((failure (handler-case (progn (asdf:find-system "nyxt-llm-cockpit" nil) nil)
                 (error (condition) (princ-to-string condition)))))
  (install-check failure
                 "Nyxt ~a no longer fails the first lookup of an extension. ~
                  Take the repair out of README.org, its test out of here, ~
                  and say so in docs/architecture.org."
                 nyxt:+version+)
  (install-check (or (null failure) (search "absolute pathname" failure))
                 "The lookup failed, but not the way the packaged app fails: ~a"
                 failure))

;;; 2. With the install lines of README.org, read as Nyxt reads a config, the
;;;    cockpit loads from the extensions directory.

(let ((forms (readme-install-forms)))
  (install-check (search "define-nyxt-user-system-and-load" forms)
                 "README.org has no install lines in a Lisp block under `* Install'.")
  (uiop:with-temporary-file (:pathname config :prefix "cockpit-install" :type "lisp")
    (with-open-file (out config :direction :output :if-exists :supersede)
      (write-string forms out))
    (multiple-value-bind (condition backtrace)
        ;; Not from a REPL: an error comes back as values, as it does at startup.
        (let ((nyxt::*run-from-repl-p* nil)
              (*standard-output* (make-broadcast-stream))
              (*error-output* (make-broadcast-stream)))
          (handler-bind ((warning #'muffle-warning))
            (nyxt::load-lisp config :package (find-package :nyxt-user))))
      (install-check (null backtrace)
                     "The install lines signaled: ~a" condition))))

(let ((system (asdf:find-system "nyxt-llm-cockpit" nil)))
  (install-check (and system (asdf:component-loaded-p system))
                 "The cockpit is not loaded after the install lines.")
  (install-check (and (find-package :nyxt-llm-cockpit)
                      (gethash (find-symbol "CLAUDE" :nyxt-llm-cockpit)
                               nyxt::*nyxt-url-commands*))
                 "The `claude' page is not known to Nyxt after the install lines."))

(format t "~&Install lines of README.org, in a Nyxt ~a set up like the packaged app · ~
           ~d checks, ~d failure~:p~%"
        nyxt:+version+ *install-checks* *install-failures*)
(uiop:quit (if (zerop *install-failures*) 0 1))
