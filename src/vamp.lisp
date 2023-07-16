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
;;; $$ Last modified:  18:56:54 Sun Jul 16 2023 CEST
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; shortcut in order to change multiple parameters
;;; RP  Sun Jul 16 17:55:39 2023
;;;
;;; - parvals should be a list of lists consisting of parameter and value, e.g.
;;;   '((:step-size 256) (:block-size 512))
;;;
;;; EXAMPLE
#|
(change-vamp-pars '((:step-size 256) (:block-size 1024))
                   (get-vamp-plugin-skeleton "vamp:azi:azi:plan"))
|#

(defmacro change-vamp-pars (parvals rdf)
  `(loop for pv in ,parvals
         for param = (first pv)
         for val = (second pv)
         with result = ,rdf
         do
            (setf result (change-vamp-transform-parameter param val result))
         finally
            (return result)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* vamp/save-rdf-to-file
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-16
;;; 
;;; DESCRIPTION
;;; A shortcut so store an RDF-string to a file. 
;;;
;;; ARGUMENTS
;;; - The RDF string (e.g. obtained via get-vamp-plugin-skeleton)
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :outfile. The output file path for the RDF/N3 file. Defaults to the
;;;   a path retrieved from the standard output directory and a generic
;;;   filename. 
;;; 
;;; RETURN VALUE
;;; The path to the file stored.
;;;
;;; EXAMPLE
#|
(save-rdf-to-file (get-vamp-plugin-skeleton "vamp:azi:azi:plan"))
;; => "/tmp/vamp426.n3"
|#
;;; SYNOPSIS
(defun save-rdf-to-file (rdf &key
                             (outfile (concatenate
                                       'string
                                       (get-kr-config :default-dir)
                                       (format nil "~a"
                                               (alexandria:symbolicate
                                                (alexandria:make-gensym
                                                 "vamp")))
                                       ".n3")))
  ;;; ****
  (with-open-file (stream outfile
                          :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (format stream rdf))
  outfile)
                                                   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* vamp/load-rdf-from-file
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-16
;;; 
;;; DESCRIPTION
;;; Shortcut to load a stored RDF-file to a string. 
;;;
;;; ARGUMENTS
;;; The path to the RDF-file. 
;;; 
;;; RETURN VALUE
;;; The string containing the RDF-data. 
;;;
;;; EXAMPLE
#|
(load-rdf-from-file
 (save-rdf-to-file (get-vamp-plugin-skeleton "vamp:azi:azi:plan")))
|#
;;; SYNOPSIS
(defun load-rdf-from-file (path)
  ;;; ****
  (read-file-into-string path))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* vamp/run-vamp-transform
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-16
;;; 
;;; DESCRIPTION
;;; Runs a vamp transform on a sound file and returns the analysis result
;;; as a list of the form:
;;; '((timestamp-1 valuea-1 valueb-1 ... valuen-1)
;;;   (timestamp-2 valuea-2 valueb-2 ... valuen-2)
;;;   ...
;;;   (timestamp-n valuea-n valueb-n ... valuen-n)
;;; NB: The timestamp is in seconds. 
;;;
;;; ARGUMENTS
;;; - A string containing the path to the sound file to be analysed.
;;; - A string containing the path to the RDF-file used for the analysis.
;;;
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :sa-command. The path to the sonic-annotator binary. Defaults to the
;;;   path defined in +kr-config-data+.
;;; 
;;; RETURN VALUE
;;; A list of lists with the results of the analysis (see above). 
;;;
;;; EXAMPLE
#|
(let* ((rdf-data
         (change-vamp-pars
          '((:step-size 512) (:block-size 1024))
          (get-vamp-plugin-skeleton
           "vamp:vamp-example-plugins:amplitudefollower:amplitude")))
       (sndfile (path-from-same-dir "../examples/snd/kalimba.wav"))
       (rdf-file "/tmp/amp.n3"))
  ;; store RDF-file
  (with-open-file (stream rdf-file
                          :direction :output
                          :if-does-not-exist :create
                          :if-exists :supersede)
    (format stream "~a" rdf-data))
  ;; run the analysis
  (run-vamp-transform sndfile rdf-file))
|#
;;; SYNOPSIS
(defun run-vamp-transform (sndfile-path rdffile-path
                           &key
                             (sa-command (get-kr-config :sa-command)))
  ;;; ****
  (let ((result (shell sa-command
                       "-t"
                       rdffile-path
                       sndfile-path
                       "-w"
                       "csv"
                       "--csv-stdout")))
    ;; parse strings to float
    (mapcar #'(lambda (x)
                (loop for i in x
                      collect
                      (parse-float i)))
            ;; remove first column
            (mapcar #'cdr
                    ;;remove first row
                    (cdr
                     (cl-csv:read-csv result))))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF vamp.lisp
