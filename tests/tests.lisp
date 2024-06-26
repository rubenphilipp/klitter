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
;;; $$ Last modified:  13:38:43 Mon Jul 17 2023 CEST
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

;;; test shell
;;; RP  Sun Jul 16 15:53:24 2023
(test test-shell
  (let* ((result (klitter::shell "pwd")))
    (is (stringp result))))


;;; test make-descriptor
;;; RP  Sun Jul 16 16:39:08 2023
(test test-make-descriptor
  (let ((descr (klitter::make-descriptor :text #'(lambda (x y z) "nothing"))))
    (is (typep descr 'klitter::descriptor))))

;;; test get-vamp-plugins
;;; RP  Sun Jul 16 16:39:16 2023
(test test-get-vamp-plugins
  (let ((result (klitter::get-vamp-plugins)))
    (is (listp result))))

;;; test get-vamp-plugin-skeleton
;;; RP  Sun Jul 16 16:47:27 2023
(test test-get-vamp-plugin-skeleton
  (let ((result (klitter::get-vamp-plugin-skeleton
                 "vamp:vamp-example-plugins:amplitudefollower:amplitude")))
    (is (stringp result))))

;;; test change-vamp-transform-parameter
;;; RP  Sun Jul 16 17:42:16 2023
(test test-change-vamp-transform-parameter
  (let ((result (klitter::change-vamp-transform-parameter
                 :window-size 256
                 (klitter::get-vamp-plugin-skeleton
                  "vamp:vamp-example-plugins:amplitudefollower:amplitude"))))
    (is (stringp result))))


;;; test run-vamp-transform
;;; RP  Sun Jul 16 18:20:51 2023
(test test-run-vamp-transform
  (let ((rdf-data
          (klitter::change-vamp-pars
           '((:window-size 512) (:window-size 1024))
           (klitter::get-vamp-plugin-skeleton
            "vamp:vamp-example-plugins:amplitudefollower:amplitude")))
        (sndfile (test-pathname "snd/kalimba.wav"))
        (rdf-file "/tmp/amp.n3"))
    ;; store RDF-file
    (with-open-file (stream rdf-file
                            :direction :output
                            :if-does-not-exist :create
                            :if-exists :supersede)
      (format stream "~a" rdf-data))
    (let ((result (klitter::run-vamp-transform sndfile rdf-file)))
      (is (listp result)))))

;;; test do-vamp-description
;;; RP  Sun Jul 16 19:41:55 2023
(test test-do-vamp-description
  (let ((rdf-data
          (klitter::change-vamp-pars
           '((:window-size 512) (:window-size 1024))
           (klitter::get-vamp-plugin-skeleton
            "vamp:vamp-example-plugins:amplitudefollower:amplitude")))
        (sndfile (test-pathname "snd/kalimba.wav"))
        (rdf-file "/tmp/amp.n3"))
    ;; store RDF-file
    (with-open-file (stream rdf-file
                            :direction :output
                            :if-does-not-exist :create
                            :if-exists :supersede)
      (format stream "~a" rdf-data))
    (let ((result (klitter::do-vamp-description
                      (klitter::make-sndfile sndfile)
                    256 1024 rdf-file :ignore-window-size nil)))
      (is (listp result)))))

;;; test standard-descriptor-zc
;;; RP  Sun Jul 16 21:36:15 2023
(test test-standard-descriptor-zc
  (let* ((drf (klitter::descriptor-fun
               (klitter::get-kr-standard-descriptor :zero-crossings)))
         (sndfile (klitter::make-sndfile (test-pathname "snd/kalimba.wav")))
         (result (funcall drf sndfile 512 1024)))
    (is (listp result))))

;;; test descriptions1
;;; RP  Sun Jul 16 23:42:06 2023
(test test-descriptions1
  (let* ((descriptor (klitter::get-kr-standard-descriptor :spectral-centroid))
         (sndfile (klitter::make-sndfile (test-pathname "snd/kalimba.wav")))
         (hop 512)
         (window 1024)
         (description (make-instance 'klitter::description
                                     :window-size window
                                     :hop-size hop
                                     :sndfile sndfile
                                     :descriptor descriptor)))
    (is (listp (klitter::data description)))))

;;; test description-corpus1
;;; RP  Mon Jul 17 02:08:58 2023
(test test-description-corpus1
  (let* ((dr-corpus (klitter::make-descriptor-corpus
                     `((:rms . ,(klitter::get-kr-standard-descriptor
                                 :rms))
                       (:spectral-centroid
                        . ,(klitter::get-kr-standard-descriptor
                            :spectral-centroid)))))
         (sndfile (klitter::make-sndfile (test-pathname "snd/kalimba.wav")))
         (hop-size 512)
         (window-size 1024)
         (dnc (make-instance 'klitter::description-corpus
                             :window-size window-size
                             :hop-size hop-size
                             :sndfile sndfile
                             :descriptor-corpus dr-corpus)))
    (is (= 2 (length (klitter::data dnc))))))


;;; test get-segments
;;; RP  Mon Jul 17 12:53:01 2023
(test test-get-segments
  (let* ((sndfile (klitter::make-sndfile (test-pathname "snd/kalimba.wav")))
         (env '(0 .02 20 .04 50 .06 80 .03 100 .01))
         (result (klitter::get-segments sndfile env)))
    (is (listp result))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF tests.lisp
