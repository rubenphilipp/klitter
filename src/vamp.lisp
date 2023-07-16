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
;;; $$ Last modified:  16:38:55 Sun Jul 16 2023 CEST
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :klitter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* vamp/get-vamp-plugins
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-16
;;; 
;;; DESCRIPTION
;;; Returns a list with all available VAMP-plugins (transforms) on your system.
;;;
;;; ARGUMENTS
;;; None. 
;;; 
;;; RETURN VALUE
;;; A list with all available VAMP transforms installed on the system. 
;;;
;;; EXAMPLE
#|
(get-vamp-plugins)
;; =>
;; ("vamp:azi:azi:plan" "vamp:bbc-vamp-plugins:bbc-energy:average"
;; "vamp:bbc-vamp-plugins:bbc-energy:lowenergy"
;; "vamp:bbc-vamp-plugins:bbc-energy:pdip"
;; "vamp:bbc-vamp-plugins:bbc-energy:rmsdelta"
;; "vamp:bbc-vamp-plugins:bbc-energy:rmsenergy" ...)
|#
;;; SYNOPSIS
(defun get-vamp-plugins ()
  ;;; ****
  (let* ((result (shell (get-kr-config :sa-command) "--list"))
         (res-split (split #\Newline result)))
    ;; remove results which do not start with "vamp:"
    ;; this is necessary as, e.g. the LibXTract plugins cause
    ;; the list command to print the compilation method
    ;; RP  Sun Jul 16 16:36:27 2023
    (loop for pl in res-split
          for plugin? = (equal "vamp:"
                               (subseq pl 0 5))
          when plugin?
            collect pl)))
        



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
