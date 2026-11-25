;;;; SPDX-FileCopyrightText: Pedro Delfino
;;;; SPDX-License-Identifier: BSD-3-Clause

;;;; The program as a child process: three pipes, one writer at a time.

(in-package :nyxt-llm-cockpit/agent)

(defparameter *program-directories*
  '(".local/bin/" ".claude/local/" "bin/")
  "Directories under the home directory searched for the program after PATH.
A graphical application on macOS starts with a short PATH, so the place the
installer puts the program is looked at too.")

(defparameter *system-program-directories*
  '("/opt/homebrew/bin/" "/usr/local/bin/")
  "Absolute directories searched for the program after the home ones.")

(defun executable-file-p (path)
  "Return PATH when it names a file, or NIL."
  (let ((found (ignore-errors (probe-file path))))
    (and found
         (pathname-name found)
         found)))

(defun find-program (name)
  "Return a string naming the program NAME that can be started, or NIL.
A NAME holding a slash is used as it is when the file exists. Any other NAME
is looked up on PATH, then in the usual install directories."
  (cond
    ((find #\/ name)
     (and (executable-file-p name) name))
    (t
     (let ((candidates
             (append
              (mapcar (lambda (directory)
                        (concatenate 'string
                                     (string-right-trim "/" directory) "/" name))
                      (remove "" (uiop:split-string (or (uiop:getenv "PATH") "")
                                                    :separator ":")
                              :test #'string=))
              (mapcar (lambda (directory)
                        (namestring
                         (merge-pathnames (concatenate 'string directory name)
                                          (user-homedir-pathname))))
                      *program-directories*)
              (mapcar (lambda (directory)
                        (concatenate 'string directory name))
                      *system-program-directories*))))
       (loop for candidate in candidates
             when (executable-file-p candidate)
               return candidate)))))

(defclass child ()
  ((info
    :initarg :info
    :reader child-info
    :documentation "What `uiop:launch-program' returned.")
   (write-lock
    :initform (sb-thread:make-mutex :name "cockpit child input")
    :reader child-write-lock
    :documentation "Held while one line is written, so lines never interleave."))
  (:documentation "A running program with pipes on its standard streams."))

(defun launch-child (command &key directory)
  "Start COMMAND, a list of strings, and return a `child'.
DIRECTORY is where it runs. Text is UTF-8 both ways, and a byte that is not
UTF-8 is read as the replacement character instead of stopping the reader."
  (make-instance
   'child
   :info (uiop:launch-program command
                              :input :stream
                              :output :stream
                              :error-output :stream
                              :directory directory
                              :element-type 'character
                              :external-format (list :utf-8 :replacement
                                                     (code-char #xFFFD)))))

(defun child-output (child)
  "Return the stream of what CHILD prints."
  (uiop:process-info-output (child-info child)))

(defun child-error-output (child)
  "Return the stream of what CHILD prints as errors."
  (uiop:process-info-error-output (child-info child)))

(defun child-alive-p (child)
  "Return whether CHILD is still running."
  (and child (uiop:process-alive-p (child-info child))))

(defun child-send-line (child line)
  "Write LINE and a newline to CHILD. Return whether it was written."
  (sb-thread:with-mutex ((child-write-lock child))
    (let ((stream (uiop:process-info-input (child-info child))))
      (handler-case
          (progn
            (write-string line stream)
            (write-char #\Newline stream)
            (finish-output stream)
            t)
        (error () nil)))))

(defun child-close-input (child)
  "Close CHILD's input, which tells the program no more prompts are coming.
A writer stuck on a full pipe does not keep the input open: after a second the
stream is closed under it."
  (let* ((lock (child-write-lock child))
         (held-p (sb-thread:grab-mutex lock :timeout 1)))
    (unwind-protect
         (ignore-errors (close (uiop:process-info-input (child-info child))))
      (when held-p
        (sb-thread:release-mutex lock)))))

(defun wait-for-exit (child seconds)
  "Wait up to SECONDS for CHILD to exit. Return whether it did."
  (loop repeat (max 1 (ceiling seconds 0.02))
        do (unless (child-alive-p child)
             (return t))
           (sleep 0.02)
        finally (return (not (child-alive-p child)))))

(defun child-stop (child &key (grace 2))
  "End CHILD: close its input, wait GRACE seconds, then signal it.
Return the exit code, or NIL when it could not be read."
  (child-close-input child)
  (unless (wait-for-exit child grace)
    (ignore-errors (uiop:terminate-process (child-info child)))
    (unless (wait-for-exit child grace)
      (ignore-errors (uiop:terminate-process (child-info child) :urgent t))
      (wait-for-exit child grace)))
  (prog1 (ignore-errors (uiop:wait-process (child-info child)))
    (ignore-errors (uiop:close-streams (child-info child)))))
