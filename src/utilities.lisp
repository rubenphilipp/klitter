;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****h* klitter/utilities
;;; NAME
;;; utilities
;;; 
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-15
;;; 
;;; PURPOSE
;;; Utility functions for klitter.
;;;
;;; $$ Last modified:  21:06:05 Mon Jul 22 2024 CEST
;;; ****
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :klitter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/trailing-slash
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-09
;;; 
;;; DESCRIPTION
;;; This function ensures that a (path) string ends with a trailing slash.
;;; NB: This function is borrowed from Michael Edwards's slippery-chicken.
;;;
;;; ARGUMENTS
;;; A string containing the path to be checked and corrected.
;;; 
;;; RETURN VALUE
;;; The path with a trailing slash.
;;;
;;; SYNOPSIS
(defun trailing-slash (path)
  ;;; ****
  (when (> (length path) 0)
    (if (char= #\/ (elt path (1- (length path))))
        path
        (format nil "~a/" path))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/path-from-same-dir
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-09
;;; 
;;; DESCRIPTION
;;; This function returns the full path to a file reilative to the directory of
;;; the current lisp file.
;;; NB: This function is borrowed from Michael Edwards's slippery-chicken.
;;; NB2: This function does not work with files which have been loaded via
;;;      ASDF/quicklisp.
;;;
;;; ARGUMENTS
;;; - A string indicating the filename (or pathname) to the file relative to
;;;   the current lisp file.
;;; 
;;; RETURN VALUE
;;; A string with the full path to the file.
;;; 
;;; SYNOPSIS
(defun path-from-same-dir (file)
   ;;; ****
  (concatenate 'string
               (trailing-slash
                (directory-namestring (truename *load-pathname*)))
               file))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/path-from-src-dir
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-16
;;; 
;;; DESCRIPTION
;;; This function returns a path from the src dir. It is intended to be used
;;; within the main lisp files.
;;; NB: This function requires ASDF. 
;;;
;;; ARGUMENTS
;;; - A string indicating the filename (or pathname) to the file relative to
;;;   the src directory file.
;;; 
;;; RETURN VALUE
;;; A string with the full path to the file.
;;; 
;;; SYNOPSIS
(defun path-from-src-dir (file)
   ;;; ****
  (namestring (asdf::SYSTEM-RELATIVE-PATHNAME
               :klitter
               (concatenate 'string
                            "src/"
                            file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/mins-secs-to-secs
;;; DESCRIPTION
;;; Derive the number of seconds from a minutes-seconds value that is indicated
;;; as a string of the form "0:00.000" or a two-item list in the form '(minutes
;;; seconds) or three-item list in the form '(minutes seconds milliseconds)
;;;
;;; Courtesy of Michael Edwards (http://github.com/mdedwards/slippery-chicken)
;;; 
;;; ARGUMENTS
;;; - A time in minutes and seconds, as described above.
;;;
;;; OPTIONAL ARGUMENTS
;;; - if a string is to be passed, then a character that denotes the separator
;;; between minutes and seconds. Default = #\:
;;; 
;;; RETURN VALUE
;;; A decimal number that is a number in seconds.
;;; 
;;; EXAMPLE
#|
(mins-secs-to-secs '(2 1))
=> 121.0
(mins-secs-to-secs '(16 59 534)))
=> 1019.534 
(mins-secs-to-secs "3:06.829"))
=> 186.829
;; using a different separator character between minutes and seconds
(mins-secs-to-secs "3-36.29" #\-) 0.0001)
=> 216.29
|#
;;; SYNOPSIS
(defun mins-secs-to-secs (time &optional (post-mins #\:))
;;; ****
  (flet ((secs-msecs (secs msecs)
           (when (or (> secs 60)
                     (> msecs 1000))
             (error "utilities::mins-secs-to-secs: secs = ~a ~
                               millisecs = ~a???" secs msecs))))
    (cond ((not time) nil)
          ((numberp time) time)
          ;; MDE Thu Aug 29 13:30:34 2019 -- allow strings like "12:36.23"
          ((stringp time) (mins-secs-to-secs-aux time post-mins))
          ((= 2 (length time))
           (let ((mins (first time))
                 (secs (second time)))
             (secs-msecs secs 0)
             (+ secs (* 60.0 mins))))
          ((= 3 (length time))
           (let ((mins (first time))
                 (secs (second time))
                 (msecs (third time)))
             (secs-msecs secs msecs)
             (+ secs (/ msecs 1000.0) (* 60.0 mins))))
          (t (error "utilities::mins-secs-to-secs: arg must be a 2- or ~
                         3-element list (mins secs [millisecs]): ~a"
                    time)))))

(defun mins-secs-to-secs-aux (string &optional (post-mins ":"))
  (let* ((pos (position post-mins string))
         (mins (read-from-string (subseq string 0 pos)))
         (secs (read-from-string (subseq string (1+ pos)))))
    (+ (* 60.0 mins) secs)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/simple-shell
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-16
;;; 
;;; DESCRIPTION
;;; Run a shell command from lisp and return the exit code.
;;;
;;; ARGUMENTS
;;; - The shell command (i.e., most likely, path to the binary)
;;; 
;;; OPTIONAL ARGUMENTS:
;;; rest:
;;; - The arguments to the shell program.
;;; 
;;; RETURN VALUE
;;; The the exit-code of the process.
;;;
;;; SYNOPSIS
(defun simple-shell (command &rest arguments)
  ;;; ****
  (cl-user::process-exit-code
   (cl-user::run-program command arguments :output *standard-output*
                                           :wait t :input nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/shell
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-16
;;; 
;;; DESCRIPTION
;;; Runs a shell program and return the full result as a string or throws an
;;; error when the call to the program fails.
;;;
;;; ARGUMENTS
;;; - The command (e.g. a path to a binary).
;;; 
;;; OPTIONAL ARGUMENTS
;;; rest:
;;; - The arguments to the shell program. 
;;; 
;;; RETURN VALUE
;;; The result of the shell program call as a string.
;;;
;;; EXAMPLE
#|
(shell (get-kr-config :sa-command) "-v")
;; => "1.6"
|#
;;; SYNOPSIS
(defun shell (command &rest arguments)
  ;;; ****
  (multiple-value-bind (output error-output exit-code)
      (uiop:run-program (cons command arguments)
                        :output :string
                        :error-output :string
                        :ignore-error-status t)
    (unless (zerop exit-code)
      (error "utilities::shell: The call to ~a failed. Error output: ~a ~%"
             command error-output))
    output))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/generic-symbol
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-16
;;; 
;;; DESCRIPTION
;;; This function creates a generic symbol from a given string and interns
;;; it as a symbol into the package. 
;;;
;;; ARGUMENTS
;;; A symbol to generify. 
;;; 
;;; RETURN VALUE
;;; The generic symbol. 
;;;
;;; EXAMPLE
#|
(generic-symbol 'test)
;; => TEST458
|#
;;; SYNOPSIS
(defun generic-symbol (name)
  ;;; ****
  (alexandria:symbolicate
   (alexandria:make-gensym name)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/alistp
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-17
;;; 
;;; DESCRIPTION
;;; Tests if an object is of type alist.
;;;
;;; ARGUMENTS
;;; The object to test.
;;; 
;;; RETURN VALUE
;;; Either t or NIL
;;;
;;; EXAMPLE
#|
(alistp '((a . b)
          (c . d)))

;; => T
|#
;;; SYNOPSIS
(defun alistp (object)
  ;;; ****
  (and (listp object)
       (every #'consp object)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/assoc-keys
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-17
;;; 
;;; DESCRIPTION
;;; Returns a list with all keys of an alist. 
;;;
;;; ARGUMENTS
;;; An alist. 
;;; 
;;; RETURN VALUE
;;; A list with keys of the alist.
;;;
;;; EXAMPLE
#|
(let ((lst '((:test . 12)
             (:value2 . 13)
             (:something . 'of-importance))))
  (assoc-keys lst))

;; => '(:TEST :VALUE2 :SOMETHING)
|#
;;; SYNOPSIS
(defun assoc-keys (alist)
  ;;; ****
  ;; sanity checks
  (unless (alistp alist)
    (error "utilities::assoc-keys: The value is not of type alist."))
  (loop for item in alist
        collect
        (car item)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CODE PORTED FROM SLIPPERY-CHICKEN
;;; 
;;; COPYRIGHT: Michael Edwards
;;; ported from Michael Edwards's slippery-chicken
;;; RP  Mon Jul 22 20:54:44 2024
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COPYRIGHT: Michael Edwards
;;; ported from Michael Edwards's slippery-chicken
;;; RP  Mon Jul 22 20:54:44 2024
(defun env-y-min (env)
  (loop for y in (cdr env) by #'cddr minimize y))

(defun env-y-max (env)
  (loop for y in (cdr env) by #'cddr maximize y))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/auto-scale-env
;;; DATE
;;; August 29th 2013
;;;
;;; COPYRIGHT
;;; Michael Edwards (ported from slippery-chicken, utilities.lsp)
;;;
;;; DESCRIPTION
;;; Automatically scale both the x and y values of an envelope to fit within
;;; the given ranges. Normally we'll assume that the minimum and maximum Y
;;; values are present in the original envelope and so the automatically scaled
;;; envelope will represent these with the new minimum and maximum
;;; values. However sometimes an envelope doesn't range over the possible
;;; extremes, for example (0 .3 100 .6) where the y range is from 0 to 1. If
;;; this is the case and you need a scaled envelope to take this into account,
;;; then how is the original envelopes minimum and maximum values to the
;;; keyword argument :orig-y-range.
;;; 
;;; ARGUMENTS
;;; - The envelope: a list of x y pairs
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :x-min: The new minimum (starting) x value
;;; - :x-max: The new maximum (last) x value
;;; - :y-min: The new minimum (not necessarily starting!) y value
;;; - :y-max: The new maximum (not necessarily starting!) y value
;;; - :orig-y-range: a two-element list specifying the original envelope's
;;;   minimum and maximum values (see above).
;;; 
;;; RETURN VALUE
;;; The new envelope (list).
;;; 
;;; EXAMPLE
#|
                                        ; ; ; ; ;
(auto-scale-env '(0 0 10 1))            ; ; ; ; ;
=>                                      ; ; ; ; ;
(0.0 0.0 100.0 10.0)                    ; ; ; ; ;
                                        ; ; ; ; ;
(auto-scale-env '(-1 0 .3 -3 1 1) :y-min 5 :y-max 6 :x-min 2) ; ; ; ; ;
=>                                      ; ; ; ; ;
(2.0 5.75 65.7 5.0 100.0 6.0))          ; ; ; ; ;
                                        ; ; ; ; ;
(auto-scale-env '(0 1 5 1.5 7 0 10 1) :y-min -15 :y-max -4) ; ; ; ; ;
=>                                      ; ; ; ; ;
(0.0 -7.6666665 50.0 -4.0 70.0 -15.0 100.0 -7.6666665)) ; ; ; ; ;
                                        ; ; ; ; ;
(auto-scale-env '(0 .5 100 .5) :y-min 1 :y-max 2) ; ; ; ; ;
=> (0.0 1.0 100.0 1.0)                  ; ; ; ; ;
                                        ; ; ; ; ;
(auto-scale-env '(0 .5 100 .5) :y-min 1 :y-max 2 :orig-y-range '(0 1)) ; ; ; ; ;
=> (0.0 1.5 100.0 1.5)                  ; ; ; ; ;
                                        ; ; ; ; ;
|#
;;; SYNOPSIS
(defun auto-scale-env (env &key
                             (x-min 0.0) (x-max 100.0)
                             (y-min 0.0) (y-max 10.0)
                             orig-y-range)
;;; ****
  (unless (and (> x-max x-min) (>= y-max y-min))
    (error "utilities::auto-scale-env: x-max (~a) must be > x-min (~a) and ~
            ~%y-max (~a) >= y-min (~a): ~%~a" x-max x-min y-max y-min env))
  (let* ((env-x-min (first env))
         (env-x-max (lastx env))
         (env-x-range (- env-x-max env-x-min))
         (env-y-min (if orig-y-range (first orig-y-range) (env-y-min env)))
         (env-y-max (if orig-y-range (second orig-y-range) (env-y-max env)))
         (env-y-range (- env-y-max env-y-min))
         (new-env-x-range (abs (- x-max x-min)))
         (new-env-y-range (abs (- y-max y-min)))
         (x-scaler (/ new-env-x-range env-x-range))
         ;; MDE Wed Jul 29 21:00:23 2015 -- we could have an envelope like (0
         ;; .5 100 .5) whereupon  there's no y-range at all and we'd get a
         ;; division-by-zero error
         (y-scaler (if (zerop env-y-range)
                       1.0
                       (/ new-env-y-range env-y-range))))
    (loop for x in env by #'cddr and y in (cdr env) by #'cddr
          collect (float (+ x-min (* (- x env-x-min) x-scaler)))
          collect (float (+ y-min (* (- y env-y-min) y-scaler))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/interpolate
;;; DESCRIPTION
;;; Get the interpolated value at a specified point within an envelope. The
;;; envelope must be specified in the form of a list of break-point pairs.
;;;
;;; COPYRIGHT
;;; Michael Edwards (ported from slippery-chicken, utilities.lsp)
;;; 
;;; ARGUMENTS
;;; - A number that is the point within the specified envelope for which to
;;;   return the interpolated value.
;;; - A list of break-point pairs.
;;; 
;;; OPTIONAL ARGUMENTS
;;; keyword arguments:
;;; - :scaler. A number that is the factor by which to scale the values of
;;;   the break-point pairs in the given envelope before retrieving the
;;;   interpolated value. Default = 1.
;;; - :exp. A number that is the exponent to which the result should be
;;;   raised. Default = 1.
;;; - :warn. T or NIL to indicate whether the method should print a warning if
;;;   the specified point is outside of the bounds of the x-axis specified in
;;;   the list of break-point pairs. T = warn. Default = T.
;;; 
;;; RETURN VALUE
;;; 
;;; 
;;; EXAMPLE
#|
;;; Using the defaults                  ; ;
(interpolate 50 '(0 0 100 1))           ; ;
                                        ; ;
=> 0.5                                  ; ;
                                        ; ;
;;; Specifying a different scaler       ; ;
(interpolate 50 '(0 0 100 1) :scaler 2) ; ;
                                        ; ;
=> 1.0                                  ; ;
                                        ; ;
;;; Specifying a different exponent by which the result is to be raised ; ;
(interpolate 50 '(0 0 100 1) :exp 2)    ; ;
                                        ; ;
=> 0.25                                 ; ;
                                        ; ;
|#
;;; SYNOPSIS
(defun interpolate (point env &key (scaler 1) (exp 1) (warn t))
;;; ****
  "e.g. (interpolate 50 '(0 0 100 1) :scaler .5 :exp 2)
   => 0.0625
   The :EXP arg is the exponent that the interpolation result should
   be raised to."
  ;; MDE Thu Jul 14 21:29:59 2016 -- could happen...
  (if (not env)
      point
      (let ((lastx (lastx env))
            (lasty (first (last env))))
        (cond ((> point lastx)
               (when warn
                 (warn "interpolate: ~a is off the x axis of ~%~a~
                      ~%returning last y value: ~a"
                       point env lasty))
               lasty)
              ((< point (car env))
               (let (y1)
                 ;; (error "interpolate: Can't interpolate ~a in ~a" point env))
                 ;; MDE Thu Apr 23 09:48:57 2020, Heidhausen -- if our x values
                 ;; start > the point we're looking for, return the first y value
                 (warn "utilities::interp-aux: envelope starts later than point!~
                      ~%Returning first y value: ~a" (setq y1 (second env)))
                 y1))
              (t (interp-aux point env scaler exp))))))
  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun interp-aux (point env scaler exp)
  (let ((here (loop for i in env by #'cddr and j from 1 do
    (if (<= point i) (return (+ j j -2))))))
    ;; rounding in making the new-env with new-lastx can cause the very last
    ;; event to be just a little bigger than the last x value in the new-env.
    ;; If this is the case, <here> will be nil so we better deal with this:
    (unless here 
      (setq here (- (length env) 2)))
    (when (zerop here)
      (setq here 2))
    ;; MDE Mon Apr  9 13:08:16 2012 -- catch divide by zero error
    (let ((x1 (nth (- here 2) env))
          (x2 (nth here env)))
      ;; MDE Mon May 14 12:26:16 2012 
      (unless (and (numberp x1) (numberp x2))
        (error "utilities::interp-aux: y values in envelope must be ~
                numbers: ~a"
               env))
      (if (= x1 x2)
        (progn
          (warn "utilities::interp-aux: weird values for interp-aux: ~a in ~a."
                point env)
          (nth (1- here) env))
        (get-interpd-y point 
                       x1 
                       (* scaler (nth (- here 1) env))
                       x2
                       (* scaler (nth (+ here 1) env))
                       exp)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ****f* utilities/nearest
;;; DATE
;;; 15th May 2020, Heidhausen
;;; 
;;; DESCRIPTION
;;; Return the nearest number in a list to the first argument
;;;
;;; COPYRIGHT
;;; Michael Edwards (ported from slippery-chicken, utilities.lsp)
;;; 
;;; ARGUMENTS
;;; - the number we're looking to get the closest to
;;; - the list of numbers we'll search
;;; 
;;; RETURN VALUE
;;; the element of the list that's closest to the first argument, the list
;;; sorted by nearest to the number, the distances to the number for the sorted
;;; list. 
;;;
;;; OPTIONAL ARGUMENTS
;;; none
;;; 
;;; EXAMPLE
#|
(nearest 1.21 '(4 2 5 3 5 4 1.2 1.3 1.1999)) ; ;
--> 1.2                                 ; ;
|#
;;; SYNOPSIS
(defun nearest (num list)
;;; ****
  (unless (every #'numberp (cons num list))
    (error "utilities::nearest: first argument and list must be numbers"))
  (let* ((lds (loop for n in list collect (list n (abs (- num n)))))
         (sorted (sort lds #'(lambda (x y)
                               (< (second x) (second y)))))
         (ls (mapcar #'first sorted))
         (deltas (mapcar #'second sorted)))
    (values (first ls) ls deltas)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ****f* utilities/equal-within-tolerance
;;; DESCRIPTION
;;; Test whether the difference between two decimal numbers falls within a
;;; specified tolerance.
;;;
;;; This test is designed to compensate for calculation discrepancies caused by
;;; floating-point errors (such as 2.0 vs. 1.9999997), in which the equations
;;; should yield equal numbers. It is intended to be used in place of = in such
;;; circumstances.
;;;
;;; COPYRIGHT
;;; Michael Edwards (ported from slippery-chicken, utilities.lsp)
;;; 
;;; ARGUMENTS
;;; - A first number.
;;; - A second number.
;;;
;;; OPTIONAL ARGUMENTS
;;; - A decimal value that is the maximum difference allowed between the two
;;;   numbers that will still return T. Default = 0.000001d0.
;;; 
;;; RETURN VALUE
;;; T if the two tested numbers are equal within the specified tolerance,
;;; otherwise NIL.
;;; 
;;; EXAMPLE
#|
;; An example of floating-point error   ; ;
(loop for i from 0.0 below 1.1 by 0.1 collect i) ; ;
                                        ; ;
=> (0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.70000005 0.8000001 0.9000001 1.0000001) ; ;
                                        ; ;
;; Using =                              ; ;
(loop for i from 0.0 below 1.1 by 0.1   ; ;
for j in '(0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0) ; ;
collect (= i j))                        ; ;
                                        ; ;
=> (T T T T T T T NIL NIL NIL NIL)      ; ;
                                        ; ;
;; Using equal-within-tolerance         ; ;
(loop for i from 0.0 below 1.1 by 0.1   ; ;
for j in '(0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0) ; ;
collect (equal-within-tolerance i j))   ; ;
                                        ; ;
=> (T T T T T T T T T T T)              ; ;
                                        ; ;
|#
;;; SYNOPSIS
(defun equal-within-tolerance (a b &optional (tolerance 0.000001d0))
;;; ****
  (when (and (numberp a) (numberp b))
    ;; BTW coercing a and b to 'double-float doesn't help if they are of
    ;; different float types to begin with.
    (<= (abs (- a b)) tolerance)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COPYRIGHT: Michael Edwards
;;; ported from Michael Edwards's slippery-chicken
;;; RP  Mon Jul 22 20:54:44 2024
(defun lastx (env)
  "lastx returns the last x value in the given envelope.
   e.g. (lastx '(0 0 20 4 30 5 100 0)) => 100"
  (unless env
    (warn "utilities::lastx: env should not be nil."))
  (let ((len (length env)))
    (when (oddp len) 
      (error "utilities::lastx: Wrong number of elements in ~a." env))
    (when env (nth (- len 2) env))))


(defun get-interpd-y (point x1 y1 x2 y2 exp)
  "The arguments are the point we want interpolated,
   the x,y coordinates of the line the point lies within, and an exponent.  
   The calculation is how far away point is from x1 divided by how far x2 is 
   from x1 raised to the exponent exp, multiplied by the difference of y2 and 
   y1 all added to y1."
  (float (+ y1 (* (expt (/ (- point x1) 
                           (- x2 x1)) exp)
                  (- y2 y1)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; utilities.lisp
