;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; FILE
;;; synth1.orc
;;;
;;; NAME
;;; synth1
;;;
;;; DESCRIPTION
;;; This is an implementation of a simple concatenative synthesizer intended
;;; to be used in conjunction with scores generated with klitter (esp. using
;;; the method synthesize-score).
;;;
;;; AUTHOR
;;; Ruben Philipp <me@rubenphilipp.com>
;;;
;;; CREATED
;;; 2023-07-17
;;;
;;; $$ Last modified:  19:52:37 Mon Jul 17 2023 CEST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; change this according to your preferences

sr = 48000
nchnls = 2
ksmps = 32
0dbfs = 1.0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; output channels
giOutch[] init 2
giOutch[0] = 1
giOutch[1] = 2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The default synthesis instrument

instr 1

  iStart, iDur, iAmp, iFn, iWinFun, iSkip, iPitch, iPan passign 2

  ;; start time of the sample
  iSkipVal = iSkip*ftsr(iFn)

  aReader line iSkipVal, iDur, iSkipVal+iDur*ftsr(iFn)*iPitch
  aFragment table3 aReader, iFn, 0, 0, 0

  ;; apply window
  aWind table3 line:a(0, iDur, 1), iWinFun, 1, 0, 0
  aFragment = aFragment * aWind

  ;; mixing
  aMix = aFragment * iAmp

  ;; panning
  aOut[] init 2
  aOut[0], aOut[1] pan2 aMix, iPan, 0

  outch giOutch[0], aOut[0], giOutch[1], aOut[1]

endin


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; EOF synth1.orc
