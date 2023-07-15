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
;;; This implementation is loosely based on the sndfile class of
;;; slippery-chicken (http://github.com/mdedwards/slippery-chicken)
;;;
;;; CLASS HIERARCHY
;;; named-object -> sndfile
;;;
;;; $$ Last modified:  19:46:47 Sat Jul 15 2023 CEST
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


(defmethod initialize-instance :after ((sf sndfile) &rest initargs)
  (declare (ignore initargs))
  (setf (slot-value sf 'start) (mins-secs-to-secs (start sf))
        (slot-value sf 'end) (mins-secs-to-secs (end sf))
        (slot-value sf 'duration) (mins-secs-to-secs (duration sf)))
  (when (and (duration sf) (end sf))
    (error "sndfile::initialize-instance: ~
            The duration and end slots annot both be specified! ~%~a"
           sf))
  (update sf))


(defmethod print-object :before ((sf sndfile) stream)
  (format stream "~%SNDFILE: path: ~a, ~
                    ~%         snd-duration: ~a, channels: ~a, ~
                    ~%         start: ~a, end: ~a, amplitude: ~a, duration: ~a~
                    ~%         description: ~a"
          (path sf) (snd-duration sf) (channels sf) (start sf)
          (end sf) (amplitude sf) (duration sf)
          (description sf)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod (setf path) :after (value (sf sndfile))
  (declare (ignore value))
  (update sf))

(defmethod (setf description) :after (value (sf sndfile))
  (declare (ignore value))
  (setf (slot-value sf 'description) value)
(update sf))

(defmethod (setf duration) :after (value (sf sndfile))
  (declare (ignore value))
  (setf (slot-value sf 'duration) (mins-secs-to-secs (duration sf)))
  (when (data-consistent sf)
    (setf (data-consistent sf) nil)
    (set-end sf)
    (update sf)))

(defmethod (setf end) :after (value (sf sndfile))
  (declare (ignore value))
  (setf (slot-value sf 'end) (mins-secs-to-secs (end sf)))
  (when (data-consistent sf)
    (setf (data-consistent sf) nil)
    (set-dur sf)
    (update sf)))

(defmethod (setf start) :after (value (sf sndfile))
  (declare (ignore value))
  (setf (slot-value sf 'start) (mins-secs-to-secs (start sf)))
  (when (data-consistent sf)
    (setf (data-consistent sf) nil)
    (set-dur sf)
    (update sf)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* sndfile/make-sndfile
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-15
;;; 
;;; DESCRIPTION
;;; This is a helper function in order to create an instance of the
;;; sndfile-class.
;;;
;;; ARGUMENTS
;;; The path to the sound file.
;;; 
;;; OPTIONAL ARGUMENTS
;;; - :id. An id for the object. Default = NIL
;;; - :data. Additional data linked to the object. Default = the path of them
;;;   sndfile
;;; - :duration. The desired duration of the sndfile (in seconds). This should
;;;   not be specified if :end has been specified. When neither :end nor
;;;   :duration is set, the sound file will be considered as having to be
;;;   considered until its end. Default = NIL
;;; - :end. The desired end time of the sndfile (in seconds). This should not
;;;   be specified if :duration has been specified. Default = NIL
;;; - :start. The desired start time (in seconds). Default = 0.0
;;; - :amplitude. An amplitude scaler (used in some processes). Default = 1.0
;;; 
;;; RETURN VALUE
;;; The sndfile-object. 
;;;
;;; SYNOPSIS
(defun make-sndfile (path &key id data duration end (start 0.0)
                            (amplitude 1.0))
  ;;; ****
  (make-instance 'sndfile :id id :data data :path path :duration duration
                 :end end :start start :amplitude amplitude))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This method updates/initializes the values/slots of the sndfile-object.

(defmethod update ((sf sndfile) &key ignore)
  (declare (ignore ignore))
  ;; test if path is correctly set and populate data with the path unless
  ;; there is data specified
  (let ((path (path sf)))
    (when path
      (unless (and path (probe-file path))
        (error "sndfile::update: ~
                Data slot of sndfile must be set to an existing sound file:~%~a"
               path))
      (unless (data sf)
        (setf (slot-value sf 'data) path))
      ;; get basic information of the sound file
      (setf (snd-duration sf) (clm::sound-duration path)
            (channels sf) (clm::sound-chans path)
            (sample-rate sf) (clm::sound-srate path))
      (cond ((and (not (end sf)) (duration sf))
             (set-end sf))
            ((and (not (duration sf)) (end sf)) 
             (set-dur sf))
            ((and (not (end sf)) (not (duration sf)) (snd-duration sf))
             (setf (slot-value sf 'end) (snd-duration sf))
             (set-dur sf)))
      (let ((st (start sf))
            (end (end sf)))
        (when (< st 0)
          (error "sndfile::update: start < 0???: ~a" sf))
        (when (and end (<= end st))
          (error "sndfile::update: end <= start???: ~a" sf))
        ;; MDE Thu Jan 28 16:55:48 2021, Heidhausen -- signal a warning only and
        ;; adjust  
        (when (and (snd-duration sf) (> end (snd-duration sf)))
          (warn "sndfile::update: ~
                 Given end point (or duration: ~a) ~%  is > sound duration: ~
                 ~a ~%~a"
                end (snd-duration sf) sf)
          (setf (slot-value sf 'end) (snd-duration sf))
          (set-dur sf))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod set-dur ((sf sndfile))
  (let ((end (end sf))
        (start (start sf)))
    (when (and start end)
      (setf (slot-value sf 'duration) (- end start)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmethod set-end ((sf sndfile))
  (let ((dur (duration sf))
        (start (start sf)))
    (when (and start dur)
      (setf (slot-value sf 'end) (+ start dur)))))
   


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF sndfile.lisp
