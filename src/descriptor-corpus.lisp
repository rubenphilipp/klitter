;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****c* named-object/descriptor-corpus
;;; NAME
;;; descriptor-corpus
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-16
;;; 
;;; PURPOSE
;;; Implementation of the descriptor-corpus class. This class implements the
;;; functionality of creating descriptor sets, to be used e.g. in
;;; description-corpus objects.
;;;
;;; CLASS HIERARCHY
;;; named-object -> descriptor-corpus
;;;
;;; $$ Last modified:  18:14:01 Thu Jul 18 2024 CEST
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :klitter)

(defclass descriptor-corpus (named-object)
  ;; an alist of the descriptors to be used in this corpus
  ((descriptors :accessor descriptors :initarg :descriptors :initform nil)))


(defmethod initialize-instance :after ((dc descriptor-corpus) &rest initargs)
  (declare (ignore initargs))
  ;; sanity checks
  (unless (alistp (slot-value dc 'descriptors))
    (error "descriptor-corpus::initialize-instance: The descriptors must ~
            given as an alist (key-value/descriptor pair). Yours is ~a"
           (type-of (slot-value dc 'descriptors))))
  (let ((descr (slot-value dc 'descriptors)))
    (loop for key in (assoc-keys descr)
          for val = (assoc-value descr key)
          do
             (unless (typep val 'descriptor)
               (error "descriptor-corpus::initialize-instance: Every ~
                        element of the descriptors list has to be type of ~
                        DESCRIPTOR. Yours is ~a" (type-of val)))))
  (setf (slot-value dc 'data) (slot-value dc 'descriptors)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* descriptor-corpus/make-descriptor-corpus
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-16
;;; 
;;; DESCRIPTION
;;; Helper function in order to create a corpus of descriptors. 
;;;
;;; ARGUMENTS
;;; - An alist of descriptors. Must be of type:
;;;   '((:key . descriptor) ...)
;;;   E.g.:
;;;   `((:rms . ,(get-kr-standard-descriptor :rms))
;;;     (:spectral-centroid . ,(get-kr-standard-descriptor
;;;                            :spectral-centroid)))
;;; 
;;; Optional ARGUMENTS
;;; keyword-arguments:
;;; - :id. The id of the descriptor-corpus.
;;; 
;;; RETURN VALUE
;;; The descriptor-corpus object. 
;;;
;;; EXAMPLE
#|
(let ((ds `((:spectral-centroid . ,(get-kr-standard-descriptor
                                   :spectral-centroid))
            (:rms . ,(get-kr-standard-descriptor :rms)))))
  (length (descriptors (make-descriptor-corpus ds))))
;; => 2
|#
;;; SYNOPSIS
(defun make-descriptor-corpus (descriptors
                               &key (id nil))
  ;;; ****
  (unless (alistp descriptors)
    (error "descriptor-corpus::make-descriptor-corpus: The descriptors must be~
            of type ALIST"))
  (make-instance 'descriptor-corpus
                 :descriptors descriptors
                 :id id))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF descriptor-corpus.lisp
