;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* kr/klitter
;;; NAME
;;; klitter
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-17
;;; 
;;; PURPOSE
;;; This module implements the main methods and functions for a simple
;;; concatenation algorithm.
;;; NB: It is still work in progress.
;;;
;;; CLASS HIERARCHY
;;; none. no classes defined
;;;
;;; $$ Last modified:  18:43:14 Mon Jul 17 2023 CEST
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :klitter)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* klitter/get-segments
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-17
;;; 
;;; DESCRIPTION
;;; This methods generates segments from a sndfile object. It generates a list
;;; of lists with onsets and durations of each segment according to the length
;;; of the sndfile and an envelope which indicates the duration of each segment
;;; along the length of the sndfile (in seconds). 
;;;
;;; ARGUMENTS
;;; - A sndfile object.
;;; - An envelope with the duration of the segments. 
;;; 
;;; RETURN VALUE
;;; A list of lists with onsets and durations of the segments. 
;;;
;;; EXAMPLE
#|
(let ((sndfile (make-sndfile
                 (path-from-same-dir "../examples/snd/kalimba.wav")))
       (env '(0 .02 20 .04 50 .06 80 .03 100 .01)))
  (get-segments sndfile env))
|#
;;; SYNOPSIS
(defmethod get-segments ((obj sndfile) env)
  ;;; ****
  (when (numberp env)
    (setf env `(0 ,env 100 ,env)))
  (let* ((snd-dur (snd-duration obj))
         (env-interp
           (sc::auto-scale-env env
                               :x-min 0.0
                               :x-max snd-dur
                               :y-min (sc::env-y-min env)
                               :y-max (sc::env-y-max env))))
    (loop with onset = 0
          for dur = (sc::interpolate onset env-interp)
          with result = '()
          while (<= (+ onset dur) snd-dur)
          do
             (push (list onset dur) result)
             (setf onset (+ onset dur))
          finally
             (return (reverse result)))))
                                         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; get segments from description-corpus
;;; RP  Mon Jul 17 12:51:38 2023

(defmethod get-segments ((obj description-corpus) env)
  (get-segments (sndfile obj) env))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* klitter/get-feature-vectors-from-target
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-17
;;; 
;;; DESCRIPTION
;;; This method returns for each onset in a list of segments (e.g. generated
;;; via get-segments) an alist containing the target vector for each analysis
;;; frame in a description-corpus object closest to the onset in the segment
;;; list.
;;;
;;; ARGUMENTS
;;; - A description-corpus object.
;;; - A list of segments (e.g. generated via get-segments).
;;; 
;;; OPTIONAL ARGUMENTS
;;; none
;;; 
;;; RETURN VALUE
;;; A list of lists of the form:
;;; '((seg-onset1 ((:descriptor1 . value1)
;;;                ...
;;;                (:descriptorn . valuen)))
;;;   ...)
;;;
;;; SYNOPSIS
(defmethod get-feature-vectors-from-target ((descr description-corpus) segments)
  ;;; ****
  (unless (listp segments)
    (error "klitter::get-feature-vectors-from-target: The segments must be of ~
            type list."))
  (let ((description-keys (assoc-keys (descriptors
                                       (descriptor-corpus descr)))))
    (loop for segment in segments
          for onset = (first segment)
          collect
          (list onset
                (loop for key in description-keys
                      for description = (assoc-value (data descr) key)
                      for data = (data description)
                      collect
                      ;; find closest time point and collect value
                      (let* ((dn-onsets (mapcar #'car data))
                             (nearest (sc::nearest onset dn-onsets))
                             (pos (position nearest dn-onsets))
                             (value (cdr (nth pos data))))
                        (cons key value)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* klitter/get-candidate-from-feature-vector
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-17
;;; 
;;; DESCRIPTION
;;; This methods performs a search for matches of a target feature-vector
;;; (cf. get-feature-vectors-from-target) in a description-corpus. From all
;;; possible candidates, it selects a random candidate and thus returns a
;;; list of the following form:
;;; '((tfv-onset-n c-onset-n) ...)
;;;
;;; - tfv-onset is the onset time in the target feature-vector
;;; - c-onset is the onset time in the sndfile of the description-corpus object.
;;;
;;; The search is performed by looking up for frames in the description-corpus
;;; that match the target feature-vector within a given tolerance (relative
;;; to the range of the feature values. 
;;;
;;; ARGUMENTS
;;; - A description-corpus object.
;;; - A target feature-vector (e.g. generated via
;;;   get-feature-vectors-from-target)
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :features. A list with the ids of features to be included in the
;;;   matching process. Default = '()
;;; - :tolerance. An alist of the form '((:descriptor-n . 0.2) ...)
;;;   The descriptor must be named according to the descriptor name of the
;;;   description-corpus. The value is relative to the value range. When
;;;   a descriptor is not present in this alist, the :default-tolerance will
;;;   be applied. Default = '()
;;; - :default-tolerance. This is a number which is the default tolerance,
;;;   applied to all descriptors which are not explicitly allocated a tolerance
;;;   value via :tolerance.
;;; 
;;; RETURN VALUE
;;; A list of lists of the form as described above. 
;;;
;;; EXAMPLE


;;; SYNOPSIS
(defmethod get-candidate-from-feature-vector ((dc description-corpus)
                                               target-vector
                                               &key
                                                 (features '())
                                                 (tolerance '())
                                                 (default-tolerance 0.0))
  ;;; ****
  ;; sanity checks
  (unless (alistp tolerance)
    (error "klitter::get-candidate-from-feature-vector: :tolerance must ~
            be of type ALIST, not ~a" (type-of tolerance)))
  (unless (listp features)
    (error "klitter::get-candidate-from-feature-vector: :tolerance must ~
            be of type LIST, not ~a" (type-of tolerance)))
  (let* ((descriptors (descriptors (descriptor-corpus dc)))
         (descriptor-keys (assoc-keys descriptors))
         (ranges (loop for key in descriptor-keys
                       for description = (assoc-value (data dc) key)
                       collect
                       `(,key . ,(get-range description))))
         (tolerance-abs (loop for key in descriptor-keys
                              for tval = (assoc-value tolerance key)
                              for range = (assoc-value ranges key)
                              collect
                              (cons key
                                    (if tval
                                        (* tval
                                           (- (second range) (first range)))
                                        (* default-tolerance
                                           (- (second range)
                                              (first range))))))))
    (format t "~%klitter::get-candidate-from-feature-vector: Starting ~
               the analysis now. This might take a while...~% ~
               **********~%")
    ;; now loop through the target vector
    ;; seg = segment / frame
    (loop for seg in target-vector
          for target-onset = (car seg)
          for target-descriptors = (second seg)
          with descriptor-keys = features
          collect
          (list
           target-onset
           ;; descriptor by descriptor
           ;; this results in a list with ids of elements
           (loop
             for descriptor-key in descriptor-keys
             for descriptor-data = (data (assoc-value
                                          (data dc)
                                          descriptor-key))
             for target-val = (let ((tv (assoc-value
                                         target-descriptors
                                         descriptor-key)))
                                (if (listp tv) (car tv) tv))
             for dtolerance = (assoc-value tolerance-abs
                                           descriptor-key)
             ;; index needed to check whether this is
             ;; the first search run
             for ki from 0
             ;; just the indices
             with matches = '()
             if (or (= ki 0) matches)
               do
                  ;;(print (alistp target-descriptors))
                  ;; just do a complete run the first time
                  (if (= 0 ki)
                      ;; do a complete search
                      (loop for ddata in descriptor-data
                            ;; index
                            for i from 0
                            for val = (second ddata)
                            do
                               (when (sc::equal-within-tolerance
                                      val
                                      target-val
                                      dtolerance)
                                 (push i matches)))
                      ;; when there are already matches just
                      ;; validate the, for this descriptor and remove
                      ;; index when matching
                      (loop
                        for match in matches
                        for val = (second (nth match descriptor-data))
                        with new-matches = '()
                        do
                           (when (sc::equal-within-tolerance
                                  val
                                  target-val
                                  dtolerance)
                             (push match new-matches))
                           (setf matches new-matches)))
             else
               do (warn "klitter::get-candidate-from-feature-vector: ~
                              No candidates found. Check your tolerance ~
                              values.")
             finally
                ;; randomly select a candidate and get the onset
                ;; time; when nothing is found, return NIL
                (if matches
                    (let* ((random-candidate
                             (nth (random (length matches)) matches))
                           ;; the data of the first descriptor
                           (descriptor-data (data (assoc-value
                                                   (data dc)
                                                   (first descriptor-keys))))
                           (candidate-onset (first
                                             (nth random-candidate
                                                  descriptor-data))))
                      (return candidate-onset))
                    nil))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pan functions

(defun no-pan (start-time duration index total-fragments)
  (declare (ignore start-time duration index total-fragments))
  0.5)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* klitter/synthesize-score
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-17
;;; 
;;; DESCRIPTION
;;; This method generates a Csound score from a candidates list (cf.
;;; get-candidate-from-feature-vector) to be used in conjunction with the
;;; synth1.orc csound orchestra.
;;;
;;; ARGUMENTS
;;; - A sndfile object (should be the sndfile object which has been used
;;;   to generate the candidate list.
;;; - A candidate list (generated by get-candidate-from-feature-vector).
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :sco-file. The location for the output score. Default = A file path
;;;   relative to the default output directory.
;;; - :amp. A number or an envelope used to scale the global
;;;   amplitude. Default = 1.0
;;; - :amp-exp. The exponent for amplitude-scaling. Default = 1
;;; - :insnum. The instrument number used for the p-fields. Default = 1
;;;   (this is the standard instrument in synth1.orc)
;;; - :overlap. The overlap time of the fragments/frames (in seconds).
;;;   Can be a number or an envelope.
;;;   Default = .005
;;; - :overlap-exp. The exponent for overlap-scaling. Default = 1
;;; - :pitch. The pitch alteration (via playback speed). Either a number or an
;;;   envelope. Default = 1.0
;;; - :pitch-exp. The exponent for pitch-env scaling. Default = 1
;;; - :pan-fun. A function which takes four arguments:
;;;   - The start time (in seconds) of the fragment
;;;   - The duration (in seconds) of the fragment
;;;   - The index of the fragment, as an indicator of the location in the
;;;     synthesis-process (zero-based).
;;;   - The total fragments to be generated in the synthesis-process
;;;     (zero-based).
;;;   and returns a value 0<=f<=1. Default = #'no-pan
;;; 
;;; RETURN VALUE
;;; The path of the score-file.
;;;
;;; EXAMPLE


;;; SYNOPSIS
(defmethod synthesize-score ((sndfile sndfile) candidate-list
                             &key
                               (sco-file (concatenate
                                          'string
                                          (get-kr-config :default-dir)
                                          "kr-synth.sco"))
                               (amp 1.0)
                               (amp-exp 1)
                               (insnum 1)
                               (overlap .005)
                               (overlap-exp 1)
                               (pitch 1.0)
                               (pitch-exp 1)
                               (pan-fun #'no-pan))
  ;;; ****
  )






;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF klitter.lisp1
