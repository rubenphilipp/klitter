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
;;; $$ Last modified:  19:41:55 Sat Jul 15 2023 CEST
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

(defmacro test-pathname (path)
  `(namestring (asdf::SYSTEM-RELATIVE-PATHNAME :klitter
                                               (concatenate 'string
                                                            "tests/"
                                                            ,path))))

(defun run-tests ()
  (run! 'klitter))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; TESTS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; test trailing slash
;;; RP  Sat Jul 15 14:36:07 2023
(test test-trailing-slash
  (is (equal "/trailing/test/"
             (klitter::trailing-slash "/trailing/test"))))


;;; test sndfile
;;; RP  Sat Jul 15 18:39:44 2023
(test test-sndfile1
  (let* ((path
           ;; necessary as asdf::test-system does not work
           ;; with *load-pathname*
           (test-pathname "snd/kalimba.wav"))
         (sndfile (make-instance 'klitter::sndfile
                                 :path path)))
    (is (and
         (= (klitter::sample-rate sndfile) 48000)
         (= (klitter::channels sndfile) 2)))))

;;; test make-sndfile
;;; RP  Sat Jul 15 19:39:29 2023
(test test-make-sndfile
  (let* ((path (test-pathname "snd/kalimba.wav"))
         (sndfile (klitter::make-sndfile path
                                         :id "kalimba"
                                         :start 2.0
                                         :duration 5.0)))
    (is (and
         (= (klitter::start sndfile) 2.0)
         (equal "kalimba" (klitter::id sndfile))
         (= (klitter::end sndfile) 7.0)
         (= (klitter::duration sndfile) 5.0)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF tests.lisp
