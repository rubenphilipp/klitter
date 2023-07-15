;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* named-object/sndfile
;;; NAME
;;; sndfile
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-15
;;; 
;;; PURPOSE
;;; Implementation of the sndfile class. This class holds general information
;;; about sound files (e.g. duration, number of channels etc.) but does not
;;; contain the sound file itself. 
;;;
;;; CLASS HIERARCHY
;;; named-object -> sndfile
;;;
;;; $$ Last modified:  17:13:32 Sat Jul 15 2023 CEST
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :klitter)

(defclass sndfile (named-object)
  ;; the path to the sndfile
  ((path :accessor path :initarg :path :initform nil)
   ;; the full duration of the sound (in seconds)
   (snd-duration :accessor snd-duration :initform nil)
   ;; the duration the user wants
   (duration :accessor duration :initarg :duration :initform nil)
   ;; this should not be used when duration is given
   (end :accessor end :initarg :end :initform nil)
   ;; the number of channels in the sound file
   (channels :accessor channels :initform nil)
   ;; where to start in the sound file (in secs)
   (start :accessor start :initarg :start :initform 0.0)
   ;; a simple textual description for identification purposes
   (description :accessor description :initarg :description :initform "")
   ;; an amplitude scaler
   (amplitude :accessor amplitude :initarg :amplitude :initform 1.0)
   ;; the samplerate of the sound file
   (sample-rate :accessor sample-rate :initform nil)))


   


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF sndfile.lisp
