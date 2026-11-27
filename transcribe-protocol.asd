(defsystem "transcribe-protocol"
  :version "0.1.0"
  :description "Monophonic audio-to-notes transcription: a transcriber protocol, the result model (note events in seconds with MIDI pitches and cents, the f0 contour with voicing, the take's tuning offset, per-phrase drift, a music-theory key fit), tuning and drift removal, mir_eval note F-measure, an oracle and a plain YIN baseline transcriber"
  :author "egao1980"
  :license "MIT"
  :depends-on ((:version "audio-protocol" "0.2.0")
               (:version "music-theory" "0.6.0"))
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "conditions")
               (:file "notes")
               (:file "contour")
               (:file "hints")
               (:file "tuning")
               (:file "transcription")
               (:file "protocol")
               (:file "evaluate")
               (:file "backends"))
  :in-order-to ((test-op (test-op "transcribe-protocol/tests"))))

(defsystem "transcribe-protocol/testing"
  :version "0.1.0"
  :description "Test support for transcribe-protocol and its backends: melodies as note events, detuned takes, and PCM synthesis of note events (sine or band-limited sawtooth, vibrato, drift, a global offset, an envelope, seeded noise)"
  :author "egao1980"
  :license "MIT"
  :depends-on ("transcribe-protocol")
  :pathname "src"
  :components ((:file "testing")))

(defsystem "transcribe-protocol/tests"
  :depends-on ("transcribe-protocol" "transcribe-protocol/testing"
               (:version "rove" "0.11.1"))
  :pathname "tests"
  :serial t
  :components ((:file "package")
               (:file "support")
               (:file "conditions-test")
               (:file "model-test")
               (:file "notes-test")
               (:file "contour-test")
               (:file "hints-test")
               (:file "tuning-test")
               (:file "transcription-test")
               (:file "evaluate-test")
               (:file "testing-test")
               (:file "protocol-test")
               (:file "oracle-test")
               (:file "yin-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
