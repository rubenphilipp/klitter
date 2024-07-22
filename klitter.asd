;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* system
;;; NAME
;;; system
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-15
;;; 
;;; PURPOSE
;;; System definition (ASDF) for klitter.
;;;
;;; $$ Last modified:  20:32:45 Mon Jul 22 2024 CEST
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; main system
(defsystem "klitter"
  :description "Common Lisp implemetation of concatenative synthesis."
  :version "0.1.0"
  :author "Ruben Philipp <me@rubenphilipp.com>"
  :license "GPL Version 2.0 or later"
  :serial nil ;; could also be T; TODO: test/elaborate
  :in-order-to ((test-op (test-op "klitter/tests")))
  :depends-on ("alexandria"
               "cl-ppcre"
               "cl-csv"
               "cl-fad"
               "slippery-chicken"
               "parse-float")
  :pathname "src/"
  :components ((:file "package")
               (:file "globals")
               (:file "named-object")
               (:file "utilities")
               (:file "sndfile")
               (:file "vamp")
               (:file "descriptor")
               (:file "standard-descriptors")
               (:file "description")
               (:file "descriptor-corpus")
               (:file "description-corpus")
               (:file "klitter")))

(defsystem "klitter/tests"
  :description "Test suite for klitter."
  :author "Ruben Philipp <me@rubenphilipp.com>"
  :license "GPL Version 2.0 or later"
  :depends-on ("klitter"
               "fiveam")
  :pathname "tests/"
  :perform (test-op (o c) (symbol-call :klitter.tests :run-tests))
  :components ((:file "tests")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF klitter.asd
