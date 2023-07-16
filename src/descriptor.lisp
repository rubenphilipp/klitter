;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* named-object/descriptor
;;; NAME
;;; descriptor
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-16
;;; 
;;; PURPOSE
;;; Implementation of the descriptor class. This class contains generic
;;; informations on descriptor types and links to a descriptor function
;;; which should take three arguments (a sndfile-object, the hop- and
;;; window-size of the analysis) and is supposed to return a list of lists
;;; formatted as described in make-descriptor.
;;; The timestamp is in seconds, the value depends on the descriptor-type
;;; (could be either :text or :number).
;;;
;;; CLASS HIERARCHY
;;; named-object -> descriptor
;;;
;;; $$ Last modified:  17:18:20 Sun Jul 16 2023 CEST
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :klitter)

(defclass descriptor (named-object)
  ;; the descriptor type (should bei either :text or :number)
  ((type :accessor type :initarg :type :initform :number)
   ;; the descriptor-fun (see above)
   (descriptor-fun :accessor descriptor-fun :initarg :descriptor-fun
                   :initform nil)
   ;; a description (for identification purposes) of the descriptor
   (description :accessor description :initarg :description :initform "")))


(defmethod initialize-instance :after ((dr descriptor) &rest initargs)
  (declare (ignore initargs))
  ;; test if type is set correctly
  (unless (or (eq (slot-value dr 'type) :number)
              (eq (slot-value dr 'type) :text))
    (error "descriptor::initialize-instance: The descriptor type should be ~
            either :number or :text, not ~a"
           (slot-value dr 'type)))
  ;;set data to descriptor-type
  (setf (slot-value dr 'data) (slot-value dr 'type)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* descriptor/make-descriptor
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-16
;;; 
;;; DESCRIPTION
;;; Helper function to create a descriptor object.
;;;
;;; ARGUMENTS
;;; - The type of the descriptor. Either :number or :text.
;;; - The descriptor-fun. This function must take three arguments:
;;;   - a sndfile-object to analyse/described
;;;   - the hop-size of the analysis (in samples)
;;;   - the window-size of the analysis (in samples)
;;;   and must return a list of lists of the form:
;;;   '((timestamp-1 valuea-1 valueb-1 ... valuen-1)
;;;     (timestamp-2 valuea-2 valueb-2 ... valuen-2)
;;;     ...
;;;     (timestamp-n valuea-n valueb-n ... valuen-n)
;;;   The timestamp is in seconds, the value corresponds to the type of the
;;;   descriptor, thus could be either a number (e.g. a float) or a string
;;;   (e.g. "metallic").
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :description. An optional description of the descriptor. Must be a
;;;   string.
;;; - :id. The id of the descriptor.
;;; 
;;; RETURN VALUE
;;; The descriptor object. 
;;;
;;; SYNOPSIS
(defun make-descriptor (type descriptor-fun
                        &key (description "")
                          id)
  ;;; ****
  ;; sanity checks
  (unless (functionp descriptor-fun)
    (error "descriptor::make-descriptor: The descriptor-fun must be a ~
            function, not a ~a" (type-of descriptor-fun)))
  (make-instance 'descriptor
                 :type type
                 :descriptor-fun descriptor-fun
                 :id id
                 :description description))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF descriptor.lisp
