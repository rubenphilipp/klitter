(in-package :klitter)

(let* ((sndpath (path-from-same-dir "snd/kalimba.wav"))
       (sndfile (make-instance 'sndfile
                               :path sndpath)))
  (print sndfile))

