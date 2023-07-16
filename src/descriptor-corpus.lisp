;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE
;;; descriptor-corpus.lisp
;;;
;;; NAME
;;; named-object/descriptor-corpus
;;;
;;; DESCRIPTION
;;; Implementation of the descriptor-corpus class. This class implements the
;;; functionality of creating descriptor sets, to be used e.g. in
;;; description-corpus objects.
;;;
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-16
;;;
;;; $$ Last modified:  00:36:28 Mon Jul 17 2023 CEST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :klitter)

(defclass descriptor-corpus (named-object)
  ;; a list of the descriptors to be used in this corpus
  ((descriptors :accessor descriptors :initarg :descriptors :initform nil)))


(defmethod initialize-instance :after ((dc descriptor-corpus) &rest initargs)
  (declare (ignore initargs))
  ;; sanity checks
  (mapcar #'(lambda (x)
              (unless (typep x 'descriptor)
                (error "descriptor-corpus::initialize-instance: Every ~
                        element of the descriptors list has to be type of ~
                        DESCRIPTOR")))
          (slot-value dc 'descriptors))
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
;;; - A list of descriptors. 
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :id. The id of the descriptor-corpus.
;;; 
;;; RETURN VALUE
;;; The descriptor-corpus object. 
;;;
;;; EXAMPLE
#|
(let ((ds (list
           (get-kr-standard-descriptor :spectral-centroid)
           (get-kr-standard-descriptor :rms))))
  (length (descriptors (make-descriptor-corpus ds))))
;; => 2
|#
;;; SYNOPSIS
(defun make-descriptor-corpus (descriptors
                               &key (id nil))
  ;;; ****
  (unless (listp descriptors)
    (error "descriptor-corpus::make-descriptor-corpus: The descriptors must be~
            of type LIST"))
  (make-instance 'descriptor-corpus
                 :descriptors descriptors
                 :id id))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF descriptor-corpus.lisp
