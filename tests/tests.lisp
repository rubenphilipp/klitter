;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* klitter tests
;;; NAME
;;; klitter tests
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-15
;;; 
;;; PURPOSE
;;; Regression test suite for klitter.
;;;
;;; $$ Last modified:  16:35:49 Sat Jul 15 2023 CEST
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage :klitter.tests
  (:use :cl :klitter :fiveam)
  (:shadow :test)
  (:export :run-tests))

(in-package :klitter.tests)

(def-suite klitter)
(in-suite klitter)

(defmacro test (name &body body)
  `(5am:test ,name
     ,@body))

(defun run-tests ()
  (run! 'klitter))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TESTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; test trailing slash
;;; RP  Sat Jul 15 14:36:07 2023
(test trailing-slash
  (is (equal "/trailing/test/"
             (klitter::trailing-slash "/trailing/test"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF tests.lisp
