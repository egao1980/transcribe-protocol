;;;; FINISH-TRANSCRIPTION: the protocol computes tuning, drift and key fit.

(in-package #:transcribe-protocol/tests)

(defun finish (notes &rest keys)
  (apply #'finish-transcription notes (make-f0-contour) keys))

(defun shifted (spec semitones)
  "SPEC's melody with every pitch moved by SEMITONES (a real)."
  (melody (mapcar (lambda (entry)
                    (destructuring-bind (pitch duration) entry
                      (list (and pitch (+ pitch semitones)) duration)))
                  spec)))

(defun fit-of (notes key)
  (mt:fit-key (map 'list (lambda (n) (cons (note-event-pitch n) (note-event-duration n))) notes)
              :hint key))

(deftest finish-in-tune
  (let* ((raw (melody *melody-c*))
         (contour (make-f0-contour :f0 '(261.6 nil)))
         (tr (finish-transcription raw contour :backend '(:name "test") :duration 17/2)))
    (ok (transcription-p tr))
    (ok (equalp raw (transcription-notes tr)))
    (ok (equalp raw (transcription-raw-notes tr)))
    (ok (not (eq raw (transcription-raw-notes tr))))
    (ok (eq contour (transcription-contour tr)))
    (ok (eql 0 (transcription-tuning-offset tr)))
    (ok (equal '(0 0 0) (map 'list #'phrase-drift-cents (transcription-drift tr))))
    (ok (equal '(0 5 10) (map 'list #'phrase-drift-first (transcription-drift tr))))
    (ok (equalp (fit-of raw nil) (transcription-key-fit tr)))
    (ok (eql 0 (mt:key-fit-offset (transcription-key-fit tr))))
    (ok (null (transcription-hints tr)))
    (ok (equal '(:name "test") (transcription-backend tr)))
    (ok (eql 17/2 (transcription-duration tr)))))

(deftest finish-echoes-hints
  (let ((tr (finish (melody *melody-c*) :hints '(:tempo 100 :key "C" :pitch-range (55 75)))))
    (ok (equalp (normalize-hints '(:key "C" :tempo 100 :pitch-range (55 75)))
                (transcription-hints tr)))
    (ok (equalp (mt:parse-key "C") (mt:key-fit-hint (transcription-key-fit tr)))))
  (ok (eql 5/2 (transcription-duration (finish #() :duration 2.5d0))))
  (ok (null (transcription-duration (finish #()))))
  (ok (null (transcription-backend (finish #())))))

(deftest-parametrize finish-hint-distance
    ((semitones distance)
     (0 0)
     (-1 -1)
     (1 1)
     ;; tuning is removed first: 30 cents flat in D still reads D
     (-3/10 0)
     (1/4 0)
     ;; a semitone flat and 20 cents sharp of that: still a semitone below
     (-4/5 -1))
  (let ((tr (finish (shifted *melody-d* semitones) :hints '(:key "D"))))
    (ok (eql distance (mt:key-fit-hint-distance (transcription-key-fit tr))))
    (ok (every (lambda (p) (< (cents-from-semitone p) 1d-6)) (pitches (transcription-notes tr)))
        "the corrected notes sit on semitones")))

(deftest finish-retunes-and-reports
  (let* ((tr (finish (shifted *melody-d* -3/10) :hints '(:key "D")))
         (fit (transcription-key-fit tr)))
    (ok (eql -30 (transcription-tuning-offset tr)))
    (ok (every (lambda (a b) (< (abs (- a b)) 1d-9))
               (pitches (melody *melody-d*)) (pitches (transcription-notes tr))))
    (ok (every (lambda (a b) (< (abs (- (+ a -3/10) b)) 1d-9))
               (pitches (melody *melody-d*)) (pitches (transcription-raw-notes tr))))
    (ok (eql 0 (mt:key-fit-offset fit)) "the fit runs on the corrected notes")
    (ok (equalp (fit-of (transcription-notes tr) (mt:parse-key "D")) fit))))

(deftest finish-drifting-take
  (let* ((truth (melody *melody-c*))
         (sung (detune-notes truth :offset 15 :drift-rate -8 :jitter 5 :seed 7))
         (tr (finish sung)))
    (ok (eql (estimate-tuning sung) (transcription-tuning-offset tr)))
    (ok (equalp (estimate-drift sung) (transcription-drift tr)))
    (ok (= 3 (length (transcription-drift tr))))
    (ok (equalp sung (transcription-raw-notes tr)))
    (ok (every (lambda (a b) (<= (* 100 (abs (- (note-event-pitch a) (note-event-pitch b)))) 20))
               truth (transcription-notes tr)))
    (ok (equalp (fit-of (transcription-notes tr) nil) (transcription-key-fit tr)))))

(deftest finish-phrase-keywords
  (let ((sung (detune-notes (melody *melody-c*) :drift-rate -8)))
    (ok (= 1 (length (transcription-drift (finish sung :phrase-gap 1)))))
    (ok (= 1 (length (transcription-drift (finish sung :min-phrase-notes 6)))))
    (ok (= 15 (length (transcription-drift (finish sung :phrase-gap 0 :min-phrase-notes 1)))))))

(deftest finish-without-key-evidence
  (let ((tr (finish #())))
    (ok (null (transcription-key-fit tr)))
    (ok (equalp #() (transcription-notes tr)))
    (ok (equalp #() (transcription-drift tr)))
    (ok (eql 0 (transcription-tuning-offset tr))))
  (testing "the twelve chromatic notes at equal durations: no key evidence, answered"
    (let ((tr (finish (melody (loop for p from 60 below 72 collect (list p 1/2)))
                      :hints '(:key "C"))))
      (ok (null (transcription-key-fit tr)))
      (ok (= 12 (length (transcription-notes tr)))))))

(deftest finish-arguments
  (ok (signals (finish (list (note 0 2 60) (note 1 3 62))) 'invalid-notes))
  (ok (signals (finish-transcription #() :contour) 'type-error))
  (ok (signals (finish #() :backend '(:name)) 'type-error))
  (ok (signals (finish #() :duration -1) 'type-error))
  (ok (signals (finish #() :phrase-gap -1/4) 'type-error))
  (ok (signals (finish #() :min-phrase-notes 0) 'type-error))
  (ok (signals (finish #() :hints '(:key 7)) 'invalid-hint))
  (let ((tr (handler-bind ((invalid-hint #'continue))
              (finish (melody *melody-c*) :hints '(:key 7 :tempo 90)))))
    (ok (equalp (list :tempo (mt:make-tempo :bpm 90)) (transcription-hints tr)))
    (ok (null (mt:key-fit-hint (transcription-key-fit tr))))))
