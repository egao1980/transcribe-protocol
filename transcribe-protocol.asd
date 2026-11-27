(defsystem "transcribe-protocol"
  :version "0.0.1"
  :description "Monophonic audio-to-notes transcription: protocol, result model, tuning and drift, evaluation, test synthesis"
  :author "egao1980"
  :license "MIT"
  :pathname "src"
  :serial t
  :components ((:file "package"))
  :in-order-to ((test-op (test-op "transcribe-protocol/tests"))))

(defsystem "transcribe-protocol/tests"
  :depends-on ("transcribe-protocol" (:version "rove" "0.11.1"))
  :pathname "tests"
  :serial t
  :components ((:file "transcribe-protocol-test"))
  :perform (test-op (o c)
             (unless (symbol-call :rove :run c)
               (error "tests failed for ~A" (component-name c)))))
