;;;; ORACLE-TRANSCRIBER: given notes, through the whole protocol.

(in-package #:transcribe-protocol/tests)

(deftest oracle-notes-and-contour
  (let* ((ns (melody *melody-c*))
         (oracle (make-instance 'oracle-transcriber :notes (coerce ns 'list)))
         (audio (sine-buffer 220 9)))
    (ok (equalp ns (oracle-transcriber-notes oracle)))
    (ok (typep (oracle-transcriber-notes oracle) 'simple-vector))
    (ok (null (transcriber-sample-rate oracle)))
    (ok (equal '(:name "oracle-transcriber") (transcriber-info oracle)))
    (multiple-value-bind (notes contour) (detect-notes oracle audio nil)
      (ok (equalp ns notes))
      (ok (not (eq notes (oracle-transcriber-notes oracle))) "a fresh copy")
      ;; 9 s at 16 kHz in frames of 256: ceiling(144000 / 256) = 563
      (ok (= 563 (contour-length contour)))
      (ok (equalp (notes-contour ns :sample-rate 16000 :hop 256 :frames 563) contour)))))

(deftest oracle-through-transcribe
  (let* ((ns (melody *melody-d*))
         (tr (transcribe (make-instance 'oracle-transcriber :notes ns)
                         (sine-buffer 220 1/2 :rate 44100)
                         :hints '(:key "D" :meter "3/4"))))
    (ok (equalp ns (transcription-notes tr)))
    (ok (equalp ns (transcription-raw-notes tr)))
    (ok (eql 0 (transcription-tuning-offset tr)))
    (ok (= 44100 (f0-contour-sample-rate (transcription-contour tr))) "no resampling")
    (ok (= 87 (contour-length (transcription-contour tr))))
    (ok (eql 0 (mt:key-fit-hint-distance (transcription-key-fit tr))))
    (ok (equal '(:name "oracle-transcriber") (transcription-backend tr)))
    (ok (equalp (mt:parse-meter "3/4") (getf (transcription-hints tr) :meter))))
  (testing "detuned notes come back retuned"
    (let* ((truth (melody *melody-d*))
           (sung (detune-notes truth :offset -20 :drift-rate -5 :jitter 4 :seed 2))
           (tr (transcribe (make-instance 'oracle-transcriber :notes sung) (sine-buffer 220 1/2)
                           :hints '(:key "D"))))
      (ok (equalp sung (transcription-raw-notes tr)))
      (ok (every (lambda (a b) (<= (* 100 (abs (- (note-event-pitch a) (note-event-pitch b)))) 20))
                 truth (transcription-notes tr)))
      (ok (eql 0 (mt:key-fit-hint-distance (transcription-key-fit tr))))))
  (testing "a semitone flat of the hint"
    (let ((tr (transcribe (make-instance 'oracle-transcriber
                                         :notes (detune-notes (melody *melody-d*) :offset -100))
                          (sine-buffer 220 1/2)
                          :hints '(:key "D"))))
      (ok (eql -1 (mt:key-fit-hint-distance (transcription-key-fit tr)))))))

(deftest oracle-arguments
  (ok (equalp #() (oracle-transcriber-notes (make-instance 'oracle-transcriber))))
  (let ((tr (transcribe (make-instance 'oracle-transcriber) (sine-buffer 220 1/2))))
    (ok (equalp #() (transcription-notes tr)))
    (ok (null (transcription-key-fit tr))))
  (ok (signals (make-instance 'oracle-transcriber :notes (list (note 0 2 60) (note 1 3 62)))
               'invalid-notes))
  (ok (signals (make-instance 'oracle-transcriber :notes 60) 'type-error)))
