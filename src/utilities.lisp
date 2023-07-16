;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* klitter/utilities
;;; NAME
;;; utilities
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-15
;;; 
;;; PURPOSE
;;; Utility functions for klitter.
;;;
;;; $$ Last modified:  19:07:53 Sun Jul 16 2023 CEST
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :klitter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/trailing-slash
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-09
;;; 
;;; DESCRIPTION
;;; This function ensures that a (path) string ends with a trailing slash.
;;; NB: This function is borrowed from Michael Edwards's slippery-chicken.
;;;
;;; ARGUMENTS
;;; A string containing the path to be checked and corrected.
;;; 
;;; RETURN VALUE
;;; The path with a trailing slash.
;;;
;;; SYNOPSIS
(defun trailing-slash (path)
  ;;; ****
  (when (> (length path) 0)
    (if (char= #\/ (elt path (1- (length path))))
        path
        (format nil "~a/" path))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/path-from-same-dir
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-09
;;; 
;;; DESCRIPTION
;;; This function returns the full path to a file reilative to the directory of
;;; the current lisp file.
;;; NB: This function is borrowed from Michael Edwards's slippery-chicken.
;;;
;;; ARGUMENTS
;;; - A string indicating the filename (or pathname) to the file relative to
;;;   the current lisp file.
;;; 
;;; RETURN VALUE
;;; A string with the full path to the file.
;;; 
;;; SYNOPSIS
 (defun path-from-same-dir (file)
   ;;; ****
  (concatenate 'string
               (trailing-slash
                (directory-namestring (truename *load-pathname*)))
               file))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/mins-secs-to-secs
;;; DESCRIPTION
;;; Derive the number of seconds from a minutes-seconds value that is indicated
;;; as a string of the form "0:00.000" or a two-item list in the form '(minutes
;;; seconds) or three-item list in the form '(minutes seconds milliseconds)
;;;
;;; Courtesy of Michael Edwards (http://github.com/mdedwards/slippery-chicken)
;;; 
;;; ARGUMENTS
;;; - A time in minutes and seconds, as described above.
;;;
;;; OPTIONAL ARGUMENTS
;;; - if a string is to be passed, then a character that denotes the separator
;;; between minutes and seconds. Default = #\:
;;; 
;;; RETURN VALUE
;;; A decimal number that is a number in seconds.
;;; 
;;; EXAMPLE
#|
(mins-secs-to-secs '(2 1))
=> 121.0
(mins-secs-to-secs '(16 59 534)))
=> 1019.534 
(mins-secs-to-secs "3:06.829"))
=> 186.829
;; using a different separator character between minutes and seconds
(mins-secs-to-secs "3-36.29" #\-) 0.0001)
=> 216.29
|#
;;; SYNOPSIS
(defun mins-secs-to-secs (time &optional (post-mins #\:))
;;; ****
  (flet ((secs-msecs (secs msecs)
           (when (or (> secs 60)
                     (> msecs 1000))
             (error "utilities::mins-secs-to-secs: secs = ~a ~
                               millisecs = ~a???" secs msecs))))
    (cond ((not time) nil)
          ((numberp time) time)
          ;; MDE Thu Aug 29 13:30:34 2019 -- allow strings like "12:36.23"
          ((stringp time) (mins-secs-to-secs-aux time post-mins))
          ((= 2 (length time))
           (let ((mins (first time))
                 (secs (second time)))
             (secs-msecs secs 0)
             (+ secs (* 60.0 mins))))
          ((= 3 (length time))
           (let ((mins (first time))
                 (secs (second time))
                 (msecs (third time)))
             (secs-msecs secs msecs)
             (+ secs (/ msecs 1000.0) (* 60.0 mins))))
          (t (error "utilities::mins-secs-to-secs: arg must be a 2- or ~
                         3-element list (mins secs [millisecs]): ~a"
                    time)))))

(defun mins-secs-to-secs-aux (string &optional (post-mins ":"))
  (let* ((pos (position post-mins string))
         (mins (read-from-string (subseq string 0 pos)))
         (secs (read-from-string (subseq string (1+ pos)))))
    (+ (* 60.0 mins) secs)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/simple-shell
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-16
;;; 
;;; DESCRIPTION
;;; Run a shell command from lisp and return the exit code.
;;;
;;; ARGUMENTS
;;; - The shell command (i.e., most likely, path to the binary)
;;; 
;;; OPTIONAL ARGUMENTS:
;;; rest:
;;; - The arguments to the shell program.
;;; 
;;; RETURN VALUE
;;; The the exit-code of the process.
;;;
;;; SYNOPSIS
(defun simple-shell (command &rest arguments)
  ;;; ****
  (cl-user::process-exit-code
   (cl-user::run-program command arguments :output *standard-output*
                                           :wait t :input nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/shell
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-16
;;; 
;;; DESCRIPTION
;;; Runs a shell program and return the full result as a string or throws an
;;; error when the call to the program fails.
;;;
;;; ARGUMENTS
;;; - The command (e.g. a path to a binary).
;;; 
;;; OPTIONAL ARGUMENTS
;;; rest:
;;; - The arguments to the shell program. 
;;; 
;;; RETURN VALUE
;;; The result of the shell program call as a string.
;;;
;;; EXAMPLE
#|
(shell (get-kr-config :sa-command) "-v")
;; => "1.6"
|#
;;; SYNOPSIS
(defun shell (command &rest arguments)
  ;;; ****
  (multiple-value-bind (output error-output exit-code)
      (uiop:run-program (cons command arguments)
                        :output :string
                        :error-output :string
                        :ignore-error-status t)
    (unless (zerop exit-code)
      (error "utilities::shell: The call to ~a failed. Error output: ~a ~%"
             command error-output))
    output))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/generic-symbol
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-16
;;; 
;;; DESCRIPTION
;;; This function creates a generic symbol from a given string and interns
;;; it as a symbol into the package. 
;;;
;;; ARGUMENTS
;;; A symbol to generify. 
;;; 
;;; RETURN VALUE
;;; The generic symbol. 
;;;
;;; EXAMPLE
#|
(generic-symbol 'test)
;; => TEST458
|#
;;; SYNOPSIS
(defun generic-symbol (name)
  ;;; ****
  (alexandria:symbolicate
   (alexandria:make-gensym name)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; utilities.lisp
