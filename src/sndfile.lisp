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
;;; $$ Last modified:  17:07:04 Sat Jul 15 2023 CEST
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :klitter)

(defclass sndfile (named-object)
  ;; the path to the sndfile
  ((path :accessor path :initarg :path :initform nil)
   ;; the full duration of the sound (in seconds)
   (snd-duration :accessor snd-dur


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF sndfile.lisp
