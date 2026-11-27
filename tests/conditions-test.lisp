;;;; Conditions: the tree, slots and reports (given in the contract).

(in-package #:transcribe-protocol/tests)

(deftest-parametrize condition-tree
    ((child parent)
     ('transcribe-error 'error)
     ('unusable-audio 'transcribe-error)
     ('audio-too-short 'unusable-audio)
     ('silent-audio 'unusable-audio)
     ('invalid-hint 'transcribe-error)
     ('invalid-notes 'transcribe-error)
     ('transcriber-error 'transcribe-error))
  (ok (subtypep child parent)))

(deftest condition-slots
  (let ((c (make-condition 'audio-too-short :duration 1/20 :minimum 1/10 :reason "short")))
    (ok (= 1/20 (unusable-audio-duration c)))
    (ok (= 1/10 (audio-too-short-minimum c)))
    (ok (equal "short" (transcribe-error-reason c))))
  (let ((c (make-condition 'silent-audio :duration 2 :peak 0d0)))
    (ok (= 2 (unusable-audio-duration c)))
    (ok (eql 0d0 (silent-audio-peak c))))
  (let ((c (make-condition 'invalid-hint :key :tempo :value -3 :cause nil :reason "r")))
    (ok (eq :tempo (invalid-hint-key c)))
    (ok (eql -3 (invalid-hint-value c)))
    (ok (null (invalid-hint-cause c))))
  (ok (= 4 (invalid-notes-index (make-condition 'invalid-notes :index 4))))
  (ok (eq :x (transcriber-error-transcriber (make-condition 'transcriber-error :transcriber :x)))))

(deftest-parametrize condition-reports
    ((type initargs fragment)
     ('transcribe-error '(:reason "bad") "bad")
     ('audio-too-short '(:duration 1/20 :minimum 1/10) "too short")
     ('silent-audio '(:duration 1 :peak 0d0) "Silent")
     ('invalid-hint '(:key :tempo :value -3 :reason "not a tempo or a positive bpm") ":TEMPO")
     ('invalid-notes '(:index 2 :reason "out of order") "index 2")
     ('transcriber-error '(:transcriber :yin :reason "no DETECT-NOTES method") "DETECT-NOTES"))
  (ok (search fragment (princ-to-string (apply #'make-condition type initargs)))))
