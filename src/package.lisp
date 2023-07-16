;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* package
;;; NAME
;;; package
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-15
;;; 
;;; PURPOSE
;;; Package definition for klitter.
;;;
;;;
;;; $$ Last modified:  16:31:19 Sun Jul 16 2023 CEST
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

(defpackage :klitter
  (:use :common-lisp)
  (:nicknames :kr :kittler)
  (:import-from
   :alexandria
   :read-file-into-string
   :assoc-value)
  (:import-from
   :parse-float
   :parse-float)
  (:import-from
   :cl-ppcre
   :split))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF package.lisp
