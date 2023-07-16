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
;;; $$ Last modified:  23:41:49 Sun Jul 16 2023 CEST
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
  just perform analysis when data is not already set
  (unless (slot-value dn 'data)
    (analyse dn)))


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
;;; 
;;; RETURN VALUE
;;; The updated description object. 
;;;
;;; EXAMPLE
#|
(let* ((descriptor (get-kr-standard-descriptor :spectral-centroid))
       (sndfile (make-sndfile (path-from-src-dir
                               "../examples/snd/kalimba.wav")))
       (hop 512)
       (window 1024)
       (description (make-instance 'description
                                   :window-size window
                                   :hop-size hop
                                   :sndfile sndfile
                                   :descriptor descriptor)))
  description)
|#
;;; SYNOPSIS
(defmethod analyse ((dn description))
  ;;; ****
  (let* ((descriptor-fun (descriptor-fun (descriptor dn)))
         (sndfile (sndfile dn))
         (hop-size (hop-size dn))
         (window-size (window-size dn))
         (result (funcall descriptor-fun sndfile hop-size window-size)))
    update data slot in dn object
    (setf (data dn) result)))


  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF description.lisp
