;;;; The T0 suite. The tests are the specification; the file headers in src/
;;;; state the rules they check.

(defpackage #:transcribe-protocol/tests
  (:use #:cl #:rove #:transcribe-protocol #:transcribe-protocol/testing)
  (:local-nicknames (#:audio #:audio-protocol)
                    (#:dsp #:audio-protocol/dsp)
                    (#:mt #:music-theory)))

(in-package #:transcribe-protocol/tests)
