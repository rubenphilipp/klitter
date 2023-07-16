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
;;; $$ Last modified:  16:47:10 Sun Jul 16 2023 CEST
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
        
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* vamp/get-vamp-plugin-skeleton
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-16
;;; 
;;; DESCRIPTION
;;; Returns the skeleton of the RDF-file for a given VAMP transform which is
;;; required for analysing a sndfile.
;;; Cf. https://code.soundsoftware.ac.uk/projects/sonic-annotator/wiki\\
;;; #2-What-features-to-extract
;;;
;;; ARGUMENTS
;;; - The transform id (e.g. retrieved via get-vamp-plugins). Must be a string.
;;; 
;;; RETURN VALUE
;;; A string with the sekelton of the VAMP transform's RDF file. 
;;;
;;; EXAMPLE
#|
(get-vamp-plugin-skeleton "vamp:azi:azi:plan")
;; =>
;; "@prefix xsd:      <http://www.w3.org/2001/XMLSchema#> .
;; @prefix vamp:     <http://purl.org/ontology/vamp/> .
;; @prefix :         <#> .

;; :transform_plugin a vamp:Plugin ;
;;     vamp:identifier \"azi\" .
;; [...]
|#
;;; SYNOPSIS
(defun get-vamp-plugin-skeleton (transform-id)
  ;;; ****
  (let ((result (shell (get-kr-config :sa-command)
                       "--skeleton"
                       transform-id)))
    result))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF vamp.lisp
