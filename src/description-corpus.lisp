;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE
;;; description-corpus.lisp
;;;
;;; NAME
;;; named-object/description-corpus
;;;
;;; DESCRIPTION
;;; Implementation of the description-corpus class. This class contains the
;;; descriptions for a single sndfile object (as a list of description objects)
;;; retrieved from a descriptor-corpus object in the data slot. 
;;;
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-16
;;;
;;; $$ Last modified:  13:15:42 Mon Jul 17 2023 CEST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :klitter)

(defclass description-corpus (named-object)
  ;; a descriptor corpus object
  ((descriptor-corpus :accessor descriptor-corpus :initarg :descriptor-corpus
                      :initform nil)
   ;; the sndfile to be analysed according with the descriptors of the
   ;; descriptor-corpus
   (sndfile :accessor sndfile :initarg :sndfile :initform nil)
   ;; the hop-size used to obtain the feature descriptions
   ;; will be used for performing the analysis
   (hop-size :accessor hop-size :initarg :hop-size :initform nil)
   ;; the window-size used to obtain the feature descriptions
   ;; will be used for performing the analysis
   (window-size :accessor window-size :initarg :window-size :initform nil)))


(defmethod initialize-instance :after ((dnc description-corpus) &rest initargs)
  (declare (ignore initargs))
  ;; sanity checks
  (unless (typep (slot-value dnc 'sndfile) 'sndfile)
    (error "description-corpus::initialize-instance: The sndfile given is not ~
            of type sndfile, but: ~a"
           (type-of (slot-value dnc 'sndfile))))
  (unless (and (numberp (slot-value dnc 'hop-size))
               (numberp (slot-value dnc 'window-size)))
    (error "description-corpus::initialize-instance: hop- and window-size have ~
            to be of type number. hop-size: ~a; window-size: ~a"
           (slot-value dnc 'hop-size) (slot-value dnc 'window-size)))
  (unless (typep (slot-value dnc 'descriptor-corpus) 'descriptor-corpus)
    (error "description-corpus::initialize-instance: The descriptor-corpus is ~
            not of type DESCRIPTOR-CORPUS, but ~a"
           (type-of (slot-value dnc 'descriptor-corpus))))
  ;;just perform analysis when data is not already set
  (unless (slot-value dnc 'data)
    (analyse dnc)))

(defmethod (setf hop-size) :after (value (dnc description-corpus))
  (declare (ignore value))
  ;; perform analysis with new hop-size
  (analyse dnc))

(defmethod (setf window-size) :after (value (dnc description-corpus))
  (declare (ignore value))
  ;; perform analysis with new window-size
  (analyse dnc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****m* description-corpus/analyse
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
;;; The description-corpus object.
;;;
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :verbose. Print the status of the analysis. Default = T.
;;; 
;;; RETURN VALUE
;;; The updated description-corpus object. 
;;;
;;; EXAMPLE
#|
(let* ((dr-corpus (make-descriptor-corpus
                   `((:rms . ,(get-kr-standard-descriptor :rms))
                     (:spectral-centroid . ,(get-kr-standard-descriptor
                                             :spectral-centroid)))))
       (sndfile (make-sndfile (path-from-src-dir
                               "../examples/snd/kalimba.wav")))
       (hop-size 512)
       (window-size 1024)
       (dnc (make-instance 'description-corpus
                           :window-size window-size
                           :hop-size hop-size
                           :sndfile sndfile
                           :descriptor-corpus dr-corpus)))
  (print (assoc-keys (data dnc))))

;; => (:SPECTRAL-CENTROID :RMS)
|#
;;; SYNOPSIS
(defmethod analyse ((dnc description-corpus) &key (verbose t))
  ;;; ****
  (let ((descriptors (descriptors (descriptor-corpus dnc))))
    (loop for key in (assoc-keys descriptors)
          for descr = (assoc-value descriptors key)
          with result = '()
          do
             (when verbose
               (format t "~%Analysing sndfile: ~a, ~% ~
                          with descriptor: ~a"
                       (path (sndfile dnc))
                       (id descr)))
             (push
              `(,key
                . ,(make-instance 'description
                                  :sndfile (sndfile dnc)
                                  :descriptor descr
                                  :hop-size (hop-size dnc)
                                  :window-size (window-size dnc)))
              result)
          finally
             (setf (data dnc) result)
             (return dnc))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* description-corpus/make-description-corpus
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-17
;;; 
;;; DESCRIPTION
;;; Shortcut to create a description-corpus object. This could be used to
;;; efficiently analyse a sndfile according to a set of descriptors given
;;; via a descriptor-corpus object. 
;;;
;;; ARGUMENTS
;;; - A descriptor-corpus object.
;;; - A sndfile object.
;;; - The hop-size for the analysis (in samples).
;;; - The window-size for the analysis (in samples).
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword-arguments:
;;; - :id. The id of the description-corpus object. 
;;; 
;;; RETURN VALUE
;;; A description-corpus object.
;;;
;;; SYNOPSIS
(defun make-description-corpus (descriptor-corpus
                                sndfile
                                hop-size
                                window-size
                                &key (id nil))
  ;;; ****
  (make-instance 'description-corpus
                 :sndfile sndfile
                 :descriptor-corpus descriptor-corpus
                 :hop-size hop-size
                 :window-size window-size
                 :id id))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF description-corpus.lisp
