;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE
;;; example1.lisp
;;;
;;; NAME
;;; example1
;;;
;;; DESCRIPTION
;;; This example demonstrates the process of concatenative synthesis with
;;; klitter using sound files both as source and target input. Here, one
;;; soundfile will be used as sound material during the process of concatenation
;;; (i.e. the source), while the other guides the selection process (and thus
;;; serves as the target). Hence, the sonic properties/features of the target
;;; considered during the analysis determine the segments chosen from the source
;;; sound file. In other words one could say that this process "resynthesises"
;;; the target sound with material from the source sound.
;;;
;;; Note: This file needs to be loaded, otherwise the sound files cannot be
;;; located as `path-from-same-dir` relies on the `*load-pathname*`.
;;;
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2024-07-18
;;;
;;; $$ Last modified:  15:40:49 Sun Jul 28 2024 CEST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :klitter)

;; If necessary, set the Csound command here.
(set-kr-config :csound-command "/opt/homebrew/bin/csound")

(let* (;; Make sndfile-objects from actual sound files.
       (target-snd (make-sndfile
                    (path-from-same-dir "snd/gente-kurz.wav")))
       (source-snd (make-sndfile
                    (path-from-same-dir "snd/kalimba.wav")))
       ;; Specify the location of the resulting Csound-score.
       (sco-file "/tmp/example1.sco")
       ;; Specify the location of the wav-file generated by Csound.
       (out-sndfile "/tmp/example1.wav")
       ;; Create a descriptor corpus using the pre-defined descriptors.
       ;; A descriptor corpus contains a set of descriptor objects which
       ;; themselves contain "recipes" to extract specific low-/high-level
       ;; descriptors from a sndfile.
       (descr-corpus (make-descriptor-corpus +kr-standard-descriptors+))
       ;; Define some general properties for the analyses. Ideally, they should
       ;; be the same for all descriptors in order to facilitate the synthesis
       ;; process.
       (hop-size 512)
       (window-size 1024)
       ;; Now, analyse both files. You might note that the analyses's data will,
       ;; for each sndfile, be stored in a description-corpus which --
       ;; analoguous to the descriptor-corpus -- contains the actual analysis
       ;; data for all descriptors in the descriptor-corpus applied as the first
       ;; argument to make-description-corpus.
       (target-description (make-description-corpus descr-corpus
                                                    target-snd
                                                    hop-size
                                                    window-size))
       (source-description (make-description-corpus descr-corpus
                                                    source-snd
                                                    hop-size
                                                    window-size))
       ;; In order to perform concatenative synthesis, the target needs to be
       ;; segmented in short fragments. In the next steps, klitter will try to
       ;; find fragments in the source sndfile with similar sonic properties. 
       ;; The unit used here are seconds which determine the length of each
       ;; segment. The method get-segments can both handle numbers (thus, the
       ;; duration of the segments will be static throughout the sndfile) or
       ;; envelopes (given as a list with alternating x y-pairs). In the latter
       ;; case, the segment's length varies throughout the sndfile. The x-range
       ;; will be scaled to the length of the respective sndfile.
       (segments (get-segments target-description '(0 .001 50 .0005 100 .001)))
       ;; Now, in order to perform the concatenation, we need to build a target
       ;; vector. The target vector contains information on the analysis data
       ;; (e.g. spectral centroid, loudness etc.) for each segment.
       (target-vector (get-feature-vectors-from-target target-description
                                                       segments))
       ;; Using the target-vector, klitter now tries fo find segments in the
       ;; source sndfile that match the features as outlined in the
       ;; target-vector. The resulting fragments will then be considered as
       ;; candidates for the concatenation.
       ;; The :features keyword specifies which descriptors to consider. 
       ;; It is possible (and crucial) to specify a tolerance relative to the
       ;; value range of the description data for each descriptor. Otherwise,
       ;; klitter would just accept exact matches as candidates.
       (candidates (get-candidate-from-feature-vector
                    source-description target-vector
                    :features '(:rms :spectral-centroid :f0)
                    :tolerance '((:rms . 0.01)
                                 (:f0 . 0.8)
                                 (:spectral-centroid . 0.01)
                                 ;; these will currently be ignored
                                 ;; (cf. :features) but could be included by
                                 ;; extending the :features list.
                                 (:spectral-rolloff . 0.002)
                                 (:zero-crossings . 1.0)))))
  ;; Now, use the data to create a Csound score.
  (synthesize-score source-snd
                    candidates
                    :sco-file sco-file
                    ;; Some random panning.
                    ;; Please note that the arguments to the lambda function are
                    ;; required but not used in this case. 
                    :pan-fun #'(lambda (start fragdur idx totalfrag)
                                 (declare (ignore start fragdur idx totalfrag))
                                 (random 1.0))
                    :amp .8
                    ;; The windowing function corresponds to a Csound GEN table.
                    ;; The elements of the list will be converted to a proper
                    ;; ftable definition.
                    ;; The following parameters relate to GEN20 (cf. the Csound-
                    ;; manual):
                    ;; '(# time size window max [opt])
                    :windowing-function '(100 0 8192 20 2)
                    :overlap .00002)
  ;; Write a WAV-file using Csound and the example synth-implementation
  ;; (synth1.orc). 
  (shell (get-kr-config :csound-command)
         "-d" "-W" "-f" "-o"
         out-sndfile
         (path-from-src-dir "synth1.orc")
         sco-file))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF example1.lisp
