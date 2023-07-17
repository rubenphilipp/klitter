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
;;; $$ Last modified:  12:51:48 Mon Jul 17 2023 CEST
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
;;; EOF klitter.lisp
