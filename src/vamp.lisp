;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* kr/vamp
;;; NAME
;;; vamp
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-16
;;; 
;;; PURPOSE
;;; This module implements interaction with VAMP plugins via sonic-annotator
;;; and provides a set of descriptor functions to be used in conjunction
;;; with klitter-descriptors.
;;;
;;; CLASS HIERARCHY
;;;
;;;
;;; $$ Last modified:  16:16:24 Sun Jul 16 2023 CEST
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :klitter)


#|
(let ((plugins (shell (get-kr-config :sa-command) "--list")))
  (print (format nil "~a" plugins)))

(shell (get-kr-config :sa-command) "--list")

(multiple-value-bind (output error-output exit-code)
    (uiop:run-program (list (get-kr-config :sa-command)
                            "--list")
                      :output :string
                      :error-output :string
                      :ignore-error-status t)
  (if (zerop exit-code)
      (format t "outout is: ~a" output)
(format t "error-output is: ~a" error-output)))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF vamp.lisp
