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
;;; formatted as follows: '((timestamp value))
;;; The timestamp is in seconds, the value depends on the descriptor-type
;;; (could be either :text or :number).
;;;
;;; CLASS HIERARCHY
;;; named-object -> descriptor
;;;
;;; $$ Last modified:  16:16:22 Sun Jul 16 2023 CEST
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
           (slot-value dr 'type))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF descriptor.lisp
