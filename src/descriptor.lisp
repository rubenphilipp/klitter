;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* named-object/descriptor
;;; NAME
;;; descriptor
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-16
;;; 
;;; PURPOSE
;;; Implementation of the descriptor class. This class describes an analysis
;;; process to be performed via VAMP plugins using the sonic-annotator CLI.
;;; It hence provides support for creating/modifying (esp. hop and window size)
;;; of VAMP transform description files (i.e. RDF files) which are used for
;;; the analysis of a sound file.
;;; A list of available VAMP plugins on the system can be obtained by running
;;; `sonic-annotator -l` in your terminal (cf. documentation of
;;; sonic-annotator).
;;;
;;; CLASS HIERARCHY
;;; named-object->descriptor
;;;
;;; $$ Last modified:  15:44:16 Sun Jul 16 2023 CEST
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :klitter)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF descriptor.lisp
