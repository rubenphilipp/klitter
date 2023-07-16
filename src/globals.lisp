;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* klitter/globals
;;; NAME
;;; globals
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-15
;;; 
;;; PURPOSE
;;; Definition of the configuration data and globals for klitter.
;;;
;;; CLASS HIERARCHY
;;;
;;;
;;; $$ Last modified:  22:48:28 Sun Jul 16 2023 CEST
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :klitter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****v* globals/+kr-config-data+
;;; NAME
;;; +kr-config-data+
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-15
;;; 
;;; DESCRIPTION
;;; A global holding information about the configuration of klitter (e.g.
;;; paths to executables, output file preferences etc.).
;;; 
(defparameter +kr-config-data+
  ;; The full path to the Csound command. This is required in order to
  ;; play/render Csound scores. 
  `((:csound-command . "/usr/local/bin/csound")
    ;; The path to the sonic-annotator binary, which is used to run the
    ;; analyses done with VAMP-plugins.
    (:sa-command . "/opt/local/bin/sonic-annotator")
    ;; The default directory for file output.
    (:default-dir . "/tmp/")
    ;; the directory for temp files.
    (:temp-dir . "/tmp/")))
  ;;; ****


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* globals/get-kr-config
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-15
;;; 
;;; DESCRIPTION
;;; Returns the value of a configuration setting from the global
;;; +kr-config-data+.
;;;
;;; RETURN VALUE
;;; The value of the +kr-config-data+ element.
;;; 
;;; SYNOPSIS
(defun get-kr-config (key)
  ;;; ****
  (declare (special +kr-config-data+))
  (assoc-value +kr-config-data+ key))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* globals/set-kr-config
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-15
;;; 
;;; DESCRIPTION
;;; Set the value of a element in the +kr-config-data+ global.
;;;
;;; ARGUMENTS
;;; - The key to the element in +kr-config-data+ (e.g. :csound-command)
;;; - The new value.
;;; 
;;; RETURN VALUE
;;; The new value of the config element.
;;;
;;; SYNOPSIS
(defun set-kr-config (key value)
  ;;; ****
  (declare (special +kr-config-data+))
  (setf (assoc-value +kr-config-data+ key) value))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF globals.lisp
