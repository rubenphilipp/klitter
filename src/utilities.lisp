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
;;; $$ Last modified:  14:53:28 Sat Jul 15 2023 CEST
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :klitter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* klitter/trailing-slash
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
;;; ****f* klitter/path-from-same-dir
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
;;; utilities.lisp
