;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* kr/standard-descriptors
;;; NAME
;;; standard-descriptors
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-16
;;; 
;;; PURPOSE
;;; This file implements a set of standard descriptors using VAMP plugins.
;;; The selection of descriptors is loosely based on the implementation of
;;; MATConcat.
;;; [sturm2004]: Cf. Sturm, Bob. 2004. „MATConcat: An Application for Exploring
;;; Concatenative Sound Synthesis Using MATLAB“.
;;;
;;; NB: This file requires a few VAMP plugins to be present on your system
;;;     (cf. readme.org).
;;;
;;; CLASS HIERARCHY
;;; none. no classes defined
;;;
;;; $$ Last modified:  18:15:28 Thu Jul 18 2024 CEST
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :klitter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****v* standard-descriptors/+kr-standard-descriptors+
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****v* standard-descriptors/:zero-crossings
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-16
;;; 
;;; DESCRIPTION
;;; This returns the number of zero crossings, signifying:
;;; "General noisiness, existence of transients" [sturm2004]
;;;
;;; USAGE
;;; (get-kr-standard-descriptor :zero-crossings)
;;;

(let* ((id 'zero-crossings)
       (rdf-skeleton
         (get-vamp-plugin-skeleton
          "vamp:vamp-example-plugins:zerocrossing:counts"))
       (description "Detects and counts zero crossing points")
       (type :number)
       (descriptor-gensym (generic-symbol id))
       (rdf-file (path-from-src-dir "vamp/zero-crossings.n3"))
       (descriptor-fun (make-vamp-descriptor-fun rdf-file
                                                 :ignore-window-size nil))
       (descriptor (make-descriptor type descriptor-fun
                                    :description description
                                    :id id)))
  (set-kr-standard-descriptor :zero-crossings descriptor))
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****v* standard-descriptors/:rms
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-16
;;; 
;;; DESCRIPTION
;;; This returns the root mean square (RMS), signifying:
;;; "Mean acoustic energy (loudness)" [sturm2004]
;;;
;;; NB: Requires LibXtract Vamp plugins
;;;     https://code.soundsoftware.ac.uk/projects/vamp-libxtract-plugins
;;;
;;; USAGE
;;; (get-kr-standard-descriptor :rms)
;;; 
(let* ((id 'rms)
       (rdf-skeleton
         (get-vamp-plugin-skeleton
          "vamp:vamp-libxtract:rms_amplitude:rms_amplitude"))
       (description "Extract the RMS amplitude of an audio signal.")
       (type :number)
       (descriptor-gensym (generic-symbol id))
       (rdf-file (path-from-src-dir "vamp/rms.n3"))
       (descriptor-fun (make-vamp-descriptor-fun rdf-file
                                                 :ignore-window-size t))
       (descriptor (make-descriptor type descriptor-fun
                                    :description description
                                    :id id)))
  (set-kr-standard-descriptor :rms descriptor))
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****v* standard-descriptors/:spectral-centroid
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-16
;;; 
;;; DESCRIPTION
;;; This returns the spectral centroid, signifying:
;;; "Mean frequency of total spectral energy distribution" [sturm2004]
;;;
;;; NB: Requires LibXtract Vamp plugins
;;;     https://code.soundsoftware.ac.uk/projects/vamp-libxtract-plugins
;;;
;;; USAGE
;;; (get-kr-standard-descriptor :spectral-centroid)
;;; 
(let* ((id 'spectral-centroid)
       (rdf-skeleton
         (get-vamp-plugin-skeleton
          "vamp:vamp-libxtract:spectral_centroid:spectral_centroid"))
       (description "Extract the spectral centroid of an audio spectrum.")
       (type :number)
       (descriptor-gensym (generic-symbol id))
       (rdf-file (path-from-src-dir "vamp/spectral-centroid.n3"))
       (descriptor-fun (make-vamp-descriptor-fun rdf-file
                                                 :ignore-window-size t))
       (descriptor (make-descriptor type descriptor-fun
                                    :description description
                                    :id id)))
  (set-kr-standard-descriptor :spectral-centroid descriptor))
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****v* standard-descriptors/:spectral-rolloff
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-16
;;; 
;;; DESCRIPTION
;;; This returns the spectral rolloff, signifying:
;;; "Frequency below which 85% of energy exists." [sturm2004]
;;;
;;; NB: Requires LibXtract Vamp plugins
;;;     https://code.soundsoftware.ac.uk/projects/vamp-libxtract-plugins
;;;
;;; USAGE
;;; (get-kr-standard-descriptor :spectral-rolloff)
;;; 
(let* ((id 'spectral-rolloff)
       (rdf-skeleton
         (get-vamp-plugin-skeleton
          "vamp:vamp-libxtract:rolloff:rolloff"))
       (description "Extract the rolloff point of an audio spectrum.")
       (type :number)
       (descriptor-gensym (generic-symbol id))
       (rdf-file (path-from-src-dir "vamp/spectral-rolloff.n3"))
       (descriptor-fun (make-vamp-descriptor-fun rdf-file
                                                 :ignore-window-size t))
       (descriptor (make-descriptor type descriptor-fun
                                    :description description
                                    :id id)))
  (set-kr-standard-descriptor :spectral-rolloff descriptor))
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****v* standard-descriptors/:inharmonicity
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-16
;;; 
;;; DESCRIPTION
;;; This returns the inharmonicity of the audio spectrum.
;;;
;;; NB: Requires LibXtract Vamp plugins
;;;     https://code.soundsoftware.ac.uk/projects/vamp-libxtract-plugins
;;;
;;; This is currently not used as it produces NILS which crash the selection
;;; algorithm. 
;;;
;;; USAGE
;;; (get-kr-standard-descriptor :inharmonicity)
;;; 
;; (let* ((id 'inharmonicity)
;;        (rdf-skeleton
;;          (get-vamp-plugin-skeleton
;;           "vamp:vamp-libxtract:spectral_inharmonicity:spectral_inharmonicity"))
;;        (description "Extract the inharmonicity of an audio spectrum.")
;;        (type :number)
;;        (descriptor-gensym (generic-symbol id))
;;        (rdf-file (path-from-src-dir "vamp/inharmonicity.n3"))
;;        (descriptor-fun (make-vamp-descriptor-fun rdf-file
;;                                                  :ignore-window-size t))
;;        (descriptor (make-descriptor type descriptor-fun
;;                                     :description description
;;                                     :id id)))
;;   (set-kr-standard-descriptor :inharmonicity descriptor))
;;; ****

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****v* standard-descriptors/:f0
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-16
;;; 
;;; DESCRIPTION
;;; This returns an estimation of the fundamental frequency.
;;;
;;; NB: Requires LibXtract Vamp plugins
;;;     https://code.soundsoftware.ac.uk/projects/vamp-libxtract-plugins
;;; 
;;; USAGE
;;; (get-kr-standard-descriptor :f0)
(let* ((id 'f0)
       (rdf-skeleton
         (get-vamp-plugin-skeleton
          "vamp:vamp-libxtract:f0:f0"))
       (description "Extract the fundamental frequency of an audio signal.")
       (type :number)
       (descriptor-gensym (generic-symbol id))
       (rdf-file (path-from-src-dir "vamp/f0.n3"))
       (descriptor-fun (make-vamp-descriptor-fun rdf-file
                                                 :ignore-window-size t))
       (descriptor (make-descriptor type descriptor-fun
                                    :description description
                                    :id id)))
  (set-kr-standard-descriptor :f0 descriptor))
;;; ****


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TESTS:
#|
(let* ((drf (descriptor-fun
             (get-kr-standard-descriptor :spectral-rolloff)))
       (sndfile (make-sndfile
                 "/Users/rubenphilipp/code/klitter/examples/snd/kalimba.wav")))
(funcall drf sndfile 512 1024))
|#

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF standard-descriptors.lisp
