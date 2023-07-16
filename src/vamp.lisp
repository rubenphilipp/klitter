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
;;; $$ Last modified:  17:45:00 Sun Jul 16 2023 CEST
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
;;; ****f* vamp/change-vamp-transform-parameter
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-16
;;; 
;;; DESCRIPTION
;;; Replace the value of a VAMP transform plugin RDF file parameter with another
;;; value. This is useful, e.g. in order to adjust the step or window size
;;; according to the desired analysis options. 
;;;
;;; ARGUMENTS
;;; - The parameter to change. The following options are available:
;;;   - :block-size (adjusts the numerical value of "vamp:block_size")
;;;   - :step-size (adjusts the numerical value of "vamp:step_size")
;;; - The new value. Either a number or a string. Must be a valid value and
;;;   type according to the specs of the VAMP transform (cf. sonic-annotator
;;;   doc).
;;; - A string containing the full RDF transform description (e.g. retrieved
;;;   via get-vamp-plugin-skeleton).
;;; 
;;; RETURN VALUE
;;; The changed RDF transform description string. 
;;;
;;; EXAMPLE
#|
(change-vamp-transform-parameter :block-size
                                 512
                                 (get-vamp-plugin-skeleton "vamp:azi:azi:plan"))
;; =>
;; "@prefix xsd:      <http://www.w3.org/2001/XMLSchema#> .
;; @prefix vamp:     <http://purl.org/ontology/vamp/> .
;; @prefix :         <#> .

;; :transform_plugin a vamp:Plugin ;
;;     vamp:identifier \"azi\" .

;; :transform_library a vamp:PluginLibrary ;
;;     vamp:identifier \"azi\" ;
;;     vamp:available_plugin :transform_plugin .

;; :transform a vamp:Transform ;
;;     vamp:plugin :transform_plugin ;
;;     vamp:step_size \"256\"^^xsd:int ; 
;;     vamp:block_size \"512\"^^xsd:int ; 
;;     vamp:plugin_version \"\"\"1\"\"\" ; 
;;     vamp:output [ vamp:identifier \"plan\" ] .
;; "
|#
;;; SYNOPSIS
(defun change-vamp-transform-parameter (parameter value transform-rdf)
   ;;; ****
  (case parameter
    (:block-size (setf parameter "vamp:block_size"))
    (:step-size (setf parameter "vamp:step_size"))
    (t (error "vamp::change-vamp-transform-parameter: The parameter ~a is ~
                not supported." parameter)))
  (unless (stringp transform-rdf)
    (error "vamp::change-vamp-transform-parameter: The transform-rdf must be ~
            be of type string, not ~a" (type-of transform-rdf)))
  (let ((search-regex (concatenate 'string
                                   parameter
                                   " \\\"([0-9]+)\\\""))
        (new-value (concatenate 'string
                                parameter
                                " \""
                                (if (stringp value)
                                    value
                                    (format nil "~a" value))
                                "\"")))
    (regex-replace search-regex transform-rdf new-value)))




(let* ((rdf (get-vamp-plugin-skeleton "vamp:azi:azi:plan"))
       (hop-size 512)
       (window-size 1024))
  (cl-ppcre::regex-replace "vamp:step_size \\\"([0-9]+)\\\""
                           rdf
                           "bla"))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF vamp.lisp
