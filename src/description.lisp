;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* named-object/description
;;; NAME
;;; description
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-16
;;; 
;;; PURPOSE
;;; Implementation of the description class and related methods. This class
;;; contains actual feature descriptions retrieved via descriptors and linked
;;; to a sndfile. 
;;;
;;; CLASS HIERARCHY
;;; named-object -> description
;;;
;;; $$ Last modified:  14:15:20 Mon Jul 17 2023 CEST
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :klitter)

(defclass description (named-object)
  ;; the sndfile this description relates to
  ((sndfile :accessor sndfile :initarg :sndfile :initform nil)
   ;; the descriptor used to retrieve the features stored in the data
   ;; slot
   (descriptor :accessor descriptor :initarg :descriptor :initform nil)
   ;; the hop-size used to obtain the feature descriptions
   ;; will be used for performing the analysis
   (hop-size :accessor hop-size :initarg :hop-size :initform nil)
   ;; the window-size used to obtain the feature descriptions
   ;; will be used for performing the analysis
   (window-size :accessor window-size :initarg :window-size :initform nil)))


(defmethod initialize-instance :after ((dn description) &rest initargs)
  (declare (ignore initargs))
  ;; sanity-checks
  (unless (typep (slot-value dn 'sndfile) 'sndfile)
    (error "description::initialize-instance: The sndfile given is not of ~
            type sndfile, but: ~a"
           (type-of (slot-value dn 'sndfile))))
  (unless (typep (slot-value dn 'descriptor) 'descriptor)
    (error "description::initialize-instance: The descriptor given is not of ~
            type descriptor, but: ~a"
           (type-of (slot-value dn 'descriptor))))
  (unless (and (numberp (slot-value dn 'hop-size))
               (numberp (slot-value dn 'window-size)))
    (error "description::initialize-instance: hop- and window-size have to ~
            be of type number. hop-size: ~a; window-size: ~a"
           (slot-value dn 'hop-size) (slot-value dn 'window-size)))
  ;;just perform analysis when data is not already set
  (unless (slot-value dn 'data)
    (analyse dn)))


(defmethod (setf hop-size) :after (value (dn description))
  (declare (ignore value))
  ;; perform analysis with new hop-size
  (analyse dn))

(defmethod (setf window-size) :after (value (dn description))
  (declare (ignore value))
  ;; perform analysis with new window-size
  (analyse dn))


(defmethod print-object :before ((dn description) stream)
  (format stream "~%DESCRIPTION: sndfile: ~a, descriptor: ~a, ~
                  hop-size: ~a, window-size: ~a"
          (sndfile dn) (descriptor dn) (hop-size dn) (window-size dn)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* description/analyse
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-16
;;; 
;;; DESCRIPTION
;;; Performs an analysis of the sndfile according to the descriptor given and
;;; updates the slots of the object accordingly. 
;;;
;;; ARGUMENTS
;;; The description object.
;;; 
;;; RETURN VALUE
;;; The updated description object. 
;;;
;;; EXAMPLE
#|
(let* ((descriptor (get-kr-standard-descriptor :rms))
       (sndfile (make-sndfile (path-from-src-dir
                               "../examples/snd/kalimba.wav")))
       (hop 512)
       (window 1024)
       (description (make-instance 'description
                                   :window-size window
                                   :hop-size hop
                                   :sndfile sndfile
                                   :descriptor descriptor)))
  (get-range description))

;; => (0.0 0.195491)
|#
;;; SYNOPSIS
(defmethod analyse ((dn description) &key args)
  ;;; ****
  (declare (ignore args))
  (let* ((descriptor-fun (descriptor-fun (descriptor dn)))
         (sndfile (sndfile dn))
         (hop-size (hop-size dn))
         (window-size (window-size dn))
         (result (funcall descriptor-fun sndfile hop-size window-size)))
    ;;update data slot in dn object
    (setf (data dn) result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Returns the value range as a list '(min max)
;;; RP  Mon Jul 17 14:12:09 2023

(defmethod get-range ((dn description))
  ;;; ****
  (let* ((data (data dn))
         (vals (mapcar #'second data)))
    (list (apply #'min vals) (apply #'max vals))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* description/make-description
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-16
;;; 
;;; DESCRIPTION
;;; A shortcut to initialize a description object.
;;;
;;; ARGUMENTS
;;; - A sndfile object.
;;; - A descriptor object.
;;; - A hop-size value (must be a number).
;;; - A window-size value (must be a number).
;;; 
;;; RETURN VALUE
;;; A description object.
;;;
;;; SYNOPSIS
(defun make-description (sndfile descriptor hop-size window-size)
  ;;; ****
  (make-instance 'description
                 :hop-size hop-size
                 :window-size window-size
                 :sndfile sndfile
                 :descriptor descriptor))

  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF description.lisp
