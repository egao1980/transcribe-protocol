;;;; The protocol: TRANSCRIBE, DETECT-NOTES, PREPARE-AUDIO, the audio
;;;; conditions and their restarts.

(in-package #:transcribe-protocol/tests)

(defclass recording-transcriber (transcriber)
  ((calls :initform '() :accessor recording-calls)
   (rate :initarg :rate :initform 16000 :reader recording-rate)
   (result :initarg :result :initform nil :reader recording-result
           :documentation "NIL, or a function (audio hints) -> notes, contour."))
  (:documentation "Remembers what DETECT-NOTES was given."))

(defmethod transcriber-sample-rate ((x recording-transcriber))
  (recording-rate x))

(defmethod detect-notes ((x recording-transcriber) audio hints)
  (push (list audio hints) (recording-calls x))
  (if (recording-result x)
      (funcall (recording-result x) audio hints)
      (values '() (make-f0-contour :sample-rate (audio:pcm-buffer-sample-rate audio)))))

(defun recorder (&rest initargs)
  (apply #'make-instance 'recording-transcriber initargs))

(defun last-audio (x) (first (first (recording-calls x))))
(defun last-hints (x) (second (first (recording-calls x))))

(defclass silent-backend (transcriber) ()
  (:documentation "A transcriber without a DETECT-NOTES method."))

(deftest transcribe-calls-the-backend
  (let* ((x (recorder))
         (audio (sine-buffer 220 1))
         (tr (transcribe x audio :hints '(:tempo 96 :key "e"))))
    (ok (transcription-p tr))
    (ok (= 1 (length (recording-calls x))))
    (ok (equalp audio (last-audio x)) "a mono 16 kHz buffer as it is")
    (ok (equalp (normalize-hints '(:key "e" :tempo 96)) (last-hints x)) "normalized hints")
    (ok (equalp (last-hints x) (transcription-hints tr)))
    (ok (equal '(:name "recording-transcriber") (transcription-backend tr)))
    (ok (eql 1 (transcription-duration tr)))
    (ok (equalp #() (transcription-notes tr)))
    (ok (null (transcription-key-fit tr)))))

(deftest transcribe-finishes-what-the-backend-found
  (let* ((sung (detune-notes (melody *melody-c*) :offset -20 :drift-rate 4))
         (contour (make-f0-contour :f0 '(100 nil 120)))
         (x (recorder :result (lambda (audio hints)
                                (declare (ignore audio hints))
                                (values (coerce sung 'list) contour))))
         (tr (transcribe x (sine-buffer 220 1/2) :hints '(:key "C"))))
    (ok (equalp sung (transcription-raw-notes tr)))
    (ok (eq contour (transcription-contour tr)))
    (ok (equalp (finish-transcription sung contour :hints '(:key "C")
                                                   :backend '(:name "recording-transcriber")
                                                   :duration 1/2)
                tr))))

(deftest transcribe-prepares-the-audio
  (testing "stereo is downmixed"
    (let* ((x (recorder))
           (left (sine-samples 220 1/2))
           (stereo (audio:pcm-from-channels (list left (map 'vector (lambda (s) (* s 1/2)) left))
                                            :sample-rate 16000)))
      (transcribe x stereo)
      (ok (= 1 (audio:pcm-buffer-channels (last-audio x))))
      (ok (equalp (dsp:downmix stereo) (last-audio x)))))
  (testing "another rate is resampled to the backend's"
    (let* ((x (recorder))
           (audio (sine-buffer 220 1/2 :rate 8000))
           (tr (transcribe x audio)))
      (ok (= 16000 (audio:pcm-buffer-sample-rate (last-audio x))))
      (ok (= 8000 (audio:pcm-buffer-frames (last-audio x))))
      (ok (equalp (audio:resample audio 16000) (last-audio x)))
      (ok (eql 1/2 (transcription-duration tr)))))
  (testing "a backend of rate NIL keeps the audio's"
    (let ((x (recorder :rate nil))
          (audio (sine-buffer 220 1/2 :rate 22050)))
      (transcribe x audio)
      (ok (= 22050 (audio:pcm-buffer-sample-rate (last-audio x))))
      (ok (equalp audio (last-audio x)))))
  (testing "a backend's own rate"
    (let ((x (recorder :rate 8000)))
      (transcribe x (sine-buffer 220 1/2))
      (ok (= 8000 (audio:pcm-buffer-sample-rate (last-audio x)))))))

(deftest transcribe-checks-the-backend
  (let ((c (caught (lambda () (transcribe (make-instance 'silent-backend) (sine-buffer 220 1/2)))
                   'transcriber-error)))
    (ok c)
    (ok (and c (typep (transcriber-error-transcriber c) 'silent-backend)))
    (ok (and c (equal "no DETECT-NOTES method" (transcribe-error-reason c)))))
  (ok (signals (transcribe (recorder :result (lambda (a h) (declare (ignore a h))
                                               (values '() :contour)))
                           (sine-buffer 220 1/2))
               'transcriber-error))
  (ok (signals (transcribe (recorder :result (lambda (a h) (declare (ignore a h))
                                               (values (list (note 0 1 60) (note 1/2 2 62))
                                                       (make-f0-contour))))
                           (sine-buffer 220 1/2))
               'invalid-notes))
  (ok (signals (transcribe (recorder :rate 0) (sine-buffer 220 1/2)) 'transcriber-error))
  (ok (signals (transcribe (recorder :rate 16000.0) (sine-buffer 220 1/2)) 'transcriber-error)))

(deftest transcribe-argument-order
  (ok (signals (transcribe (recorder) #(0.1 0.2)) 'type-error))
  (ok (signals (transcribe (recorder) :audio :hints '(:key 7)) 'type-error) "audio first")
  (ok (signals (transcribe (recorder) (silence-buffer 1) :hints '(:key 7)) 'invalid-hint)
      "hints before the audio checks")
  (ok (signals (transcribe :not-a-transcriber (sine-buffer 220 1/2)) 'error))
  (ok (signals (prepare-audio :x (sine-buffer 220 1/2)) 'type-error))
  (ok (signals (prepare-audio (recorder) '(0.5)) 'type-error)))

(deftest prepare-audio-values
  (let ((x (recorder :rate nil))
        (audio (sine-buffer 220 1/2)))
    (multiple-value-bind (buffer duration) (prepare-audio x audio)
      (ok (eq audio buffer) "nothing to do: AUDIO itself")
      (ok (eql 1/2 duration))))
  (multiple-value-bind (buffer duration) (prepare-audio (recorder) (sine-buffer 220 1/2 :rate 44100))
    (ok (= 16000 (audio:pcm-buffer-sample-rate buffer)))
    (ok (eql 1/2 duration))))

(deftest prepare-audio-too-short
  (let ((c (caught (lambda () (prepare-audio (recorder) (sine-buffer 220 1/20))) 'audio-too-short)))
    (ok c)
    (ok (and c (eql 1/20 (unusable-audio-duration c))))
    (ok (and c (eql 1/10 (audio-too-short-minimum c)))))
  (ok (prepare-audio (recorder) (sine-buffer 220 1/10)) "exactly the minimum is enough")
  (ok (signals (prepare-audio (recorder) (silence-buffer 0)) 'audio-too-short))
  (let ((*minimum-duration* 2))
    (ok (signals (prepare-audio (recorder) (sine-buffer 220 1)) 'audio-too-short)))
  (let ((*minimum-duration* 0))
    (ok (signals (prepare-audio (recorder) (silence-buffer 0)) 'silent-audio))))

(deftest prepare-audio-silent
  (let ((c (caught (lambda () (prepare-audio (recorder) (silence-buffer 1))) 'silent-audio)))
    (ok c)
    (ok (and c (eql 1 (unusable-audio-duration c))))
    (ok (and c (eql 0d0 (silent-audio-peak c)))))
  (ok (signals (prepare-audio (recorder) (sine-buffer 220 1 :amplitude 0.0009d0)) 'silent-audio))
  (ok (prepare-audio (recorder) (sine-buffer 220 1 :amplitude 0.0011d0)))
  (testing "the downmix's peak: channels that cancel are silent"
    (let* ((left (sine-samples 220 1/2))
           (stereo (audio:pcm-from-channels (list left (map 'vector #'- left)) :sample-rate 16000)))
      (ok (signals (prepare-audio (recorder) stereo) 'silent-audio))))
  (testing "a peak of exactly the level is not silent"
    (let ((*silence-peak-db* 0))
      (ok (prepare-audio (recorder) (buffer (concatenate 'vector #(1.0) (make-array 3199 :initial-element 0)))))
      (ok (signals (prepare-audio (recorder) (sine-buffer 220 1/2 :amplitude 0.99d0)) 'silent-audio)))))

(deftest prepare-audio-continue
  (let ((short (sine-buffer 220 1/20)))
    (multiple-value-bind (buffer duration)
        (handler-bind ((audio-too-short #'continue))
          (prepare-audio (recorder) short))
      (ok (equalp short buffer))
      (ok (eql 1/20 duration))))
  (testing "CONTINUE waives one check: a short silent take is silent next"
    (let ((seen '()))
      (handler-bind ((unusable-audio (lambda (c) (push (type-of c) seen) (continue c))))
        (prepare-audio (recorder) (silence-buffer 1/20)))
      (ok (equal '(silent-audio audio-too-short) seen))))
  (testing "through TRANSCRIBE: silence transcribed anyway"
    (let ((tr (handler-bind ((silent-audio #'continue))
                (transcribe (recorder) (silence-buffer 1)))))
      (ok (transcription-p tr))
      (ok (equalp #() (transcription-notes tr)))
      (ok (null (transcription-key-fit tr))))))

(deftest prepare-audio-use-value
  (let ((long (sine-buffer 330 1/2 :rate 8000)))
    (multiple-value-bind (buffer duration)
        (handler-bind ((audio-too-short (lambda (c) (use-value long c))))
          (prepare-audio (recorder) (sine-buffer 220 1/20)))
      (ok (= 16000 (audio:pcm-buffer-sample-rate buffer)))
      (ok (equalp (audio:resample long 16000) buffer))
      (ok (eql 1/2 duration) "the replacement's duration")))
  (testing "the replacement is checked from the start"
    (let ((seen '()))
      (handler-bind ((unusable-audio
                       (lambda (c)
                         (push (type-of c) seen)
                         (use-value (if (typep c 'audio-too-short) (silence-buffer 1) (sine-buffer 220 1))
                                    c))))
        (prepare-audio (recorder) (sine-buffer 220 1/20)))
      (ok (equal '(silent-audio audio-too-short) seen))))
  (testing "a replacement too short again, then a stereo one"
    (let* ((seen '())
           (left (sine-samples 220 1/2))
           (stereo (audio:pcm-from-channels (list left left) :sample-rate 16000))
           (buffer (handler-bind ((audio-too-short
                                    (lambda (c)
                                      (push (unusable-audio-duration c) seen)
                                      (use-value (if (= 1 (length seen)) (sine-buffer 220 3/40) stereo)
                                                 c))))
                     (prepare-audio (recorder) (sine-buffer 220 1/20)))))
      (ok (equal '(3/40 1/20) seen))
      (ok (= 1 (audio:pcm-buffer-channels buffer)))
      (ok (equalp (dsp:downmix stereo) buffer))))
  (testing "a replacement that isn't a buffer"
    (ok (signals (handler-bind ((silent-audio (lambda (c) (use-value 42 c))))
                   (prepare-audio (recorder) (silence-buffer 1)))
                 'type-error)))
  (testing "through TRANSCRIBE"
    (let* ((x (recorder))
           (tr (handler-bind ((silent-audio (lambda (c) (use-value (sine-buffer 220 3/4) c))))
                 (transcribe x (silence-buffer 1)))))
      (ok (eql 3/4 (transcription-duration tr)))
      (ok (= 12000 (audio:pcm-buffer-frames (last-audio x)))))))
