;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE
;;; standard-descriptors.lisp
;;;
;;; NAME
;;; kr/standard-descriptors
;;;
;;; DESCRIPTION
;;; This file implements a set of standard descriptors using VAMP plugins.
;;; The selection of descriptors is loosely based on the implementation of
;;; MATConcat.
;;; [sturm2004]: Cf. Sturm, Bob. 2004. „MATConcat: An Application for Exploring
;;; Concatenative Sound Synthesis Using MATLAB“.
;;;
;;; NB: This file requires a few VAMP plugins to be present on your system
;;;     (cf. readme.org).
;;;
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-16
;;;
;;; $$ Last modified:  21:38:20 Sun Jul 16 2023 CEST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :klitter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****P* standard-descriptors/+kr-standard-descriptors+
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-16
;;; 
;;; DESCRIPTION
;;; This global hols a set of descriptor objects as implemented in this file.
;;; 
(defparameter +kr-standard-descriptors+ '())
;;; ****


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* standard-descriptors/get-kr-standard-descriptor
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-16
;;; 
;;; DESCRIPTION
;;; Returns the object of a standard descriptor from +kr-standard-descriptors+.
;;;
;;; SYNOPSIS
(defun get-kr-standard-descriptor (key)
  ;;; ****
  (declare (special +kr-standard-descriptors+))
  (assoc-value +kr-standard-descriptors+ key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* standard-descriptors/set-kr-standard-descriptor
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-16
;;; 
;;; DESCRIPTION
;;; Set the value of a element in the +kr-config-data+ global. If an element
;;; with this key does not exist, it will be added to the alist. 
;;;
;;; SYNOPSIS
(defun set-kr-standard-descriptor (key value)
  ;;; ****
  (declare (special +kr-standard-descriptors+))
  (setf (assoc-value +kr-standard-descriptors+ key) value))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* standard-descriptors/make-vamp-descriptor-fun
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-16
;;; 
;;; DESCRIPTION
;;; This function generates a descriptor function (to be used in descriptor
;;; objects) from a given RDF-file. 
;;;
;;; ARGUMENTS
;;; - The path to the RDF-file as a string. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :ignore-window-size. Then T, the function will ignore changes in the
;;;   window size during analysis. Default = NIL.
;;; 
;;; RETURN VALUE
;;; The (lambda) function name of the descriptor-fun. 
;;;
;;; EXAMPLE
#|
(let ((fname (generic-symbol 'centroid))
      (rdf-file "/tmp/test.n3"))
  (make-vamp-descriptor-fun fname rdf-file :ignore-window-size nil))

;; => #<FUNCTION (LAMBDA (SNDFILE HOP-SIZE WINDOW-SIZE)
;;        :IN MAKE-VAMP-DESCRIPTOR-FUN) {700982F89B}>
|#
;;; SYNOPSIS
(defun make-vamp-descriptor-fun (rdf-file
                                 &key (ignore-window-size nil))
  ;;; ****
  (lambda (sndfile hop-size window-size)
    (do-vamp-description sndfile hop-size window-size
      rdf-file :ignore-window-size ignore-window-size)))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****** standard-descriptors/zero-crossings
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-16
;;; 
;;; DESCRIPTION
;;; This returns the number of zero crossings, signifying:
;;; "General noisiness, existence of transients" [sturm2004]
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let* ((id 'zero-crossings)
       (rdf-skeleton
         (get-vamp-plugin-skeleton
          "vamp:vamp-example-plugins:zerocrossing:counts"))
       (description "Detects and counts zero crossing points")
       (type :number)
       (descriptor-gensym (generic-symbol id))
       (rdf-file (save-rdf-to-file rdf-skeleton
                                   :outfile
                                   (concatenate
                                    'string
                                    (get-kr-config :default-dir)
                                    (format nil "~a"
                                            descriptor-gensym)
                                    ".n3")))
       (descriptor-fun (make-vamp-descriptor-fun rdf-file
                                                 :ignore-window-size nil))
       (descriptor (make-descriptor type descriptor-fun
                                    :description description
                                    :id id)))
  (set-kr-standard-descriptor :zero-crossings descriptor))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF standard-descriptors.lisp
