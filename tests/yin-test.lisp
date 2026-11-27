;;;; YIN-TRANSCRIBER, the baseline, on synthesized takes with known notes.

(in-package #:transcribe-protocol/tests)

(defun yin (&rest initargs)
  (apply #'make-instance 'yin-transcriber initargs))

(defun f-of (reference estimate)
  "Onset-only note F-measure, onsets within 50 ms and pitches within 50 cents."
  (values (note-f-measure reference estimate)))

(deftest yin-steady-tones
  (dolist (waveform '(:sine :sawtooth))
    (testing (format nil "~(~A~) at 220 Hz from 0.25 s to 1.25 s" waveform)
      (let* ((audio (synthesize-notes (notes '(1/4 5/4 57)) :waveform waveform :duration 3/2))
             (tr (transcribe (yin) audio))
             (ns (transcription-raw-notes tr))
             (contour (transcription-contour tr)))
        (ok (= 1 (length ns)))
        (when (= 1 (length ns))
          (let ((n (svref ns 0)))
            (ok (close-to (note-event-pitch n) 57 0.05d0) "within 5 cents")
            (ok (close-to (note-event-onset n) 1/4 0.03d0))
            (ok (close-to (note-event-offset n) 5/4 0.03d0))
            (ok (> (note-event-confidence n) 0.8d0))))
        (ok (= 16000 (f0-contour-sample-rate contour)))
        (ok (= 128 (f0-contour-hop contour)))
        (ok (eql 125 (contour-frame-rate contour)))
        (ok (= 188 (contour-length contour)) "ceiling(24000 / 128)")
        (ok (every #'null (subseq (f0-contour-f0 contour) 0 25)) "silence before the note")
        (ok (every (lambda (f) (and f (close-to f 220 1.1d0))) (subseq (f0-contour-f0 contour) 40 150)))
        (ok (every (lambda (f v) (if f (<= 0 v 1) (zerop v)))
                   (f0-contour-f0 contour) (f0-contour-voicing contour)))))))

(deftest yin-silence-and-noise
  (let ((tr (handler-bind ((silent-audio #'continue))
              (transcribe (yin) (silence-buffer 1)))))
    (ok (equalp #() (transcription-notes tr)))
    (ok (every #'null (f0-contour-f0 (transcription-contour tr)))))
  (let ((tr (transcribe (yin) (synthesize-notes #() :noise 0.3 :duration 1 :seed 5))))
    (ok (equalp #() (transcription-notes tr)) "white noise has no pitch"))
  (multiple-value-bind (notes contour)
      (detect-notes (yin) (audio:make-pcm-buffer :sample-rate 16000) nil)
    (ok (= 0 (length notes)) "no frames")
    (ok (= 0 (contour-length contour)))))

(deftest yin-level-gate
  ;; A loud note, then a periodic tone 50 dB quieter: the gate (40 dB below the
  ;; loudest frame) leaves the quiet one unvoiced; a gate of -60 dB lets it in.
  (let* ((loud (synthesize-notes (notes '(1/5 1 57)) :duration 2))
         (quiet (synthesize-notes (notes '(13/10 9/5 64)) :duration 2
                                                         :amplitude (* 1/2 (expt 10d0 -5/2))))
         (audio (buffer (map 'vector #'+ (audio:pcm-buffer-samples loud)
                             (audio:pcm-buffer-samples quiet)))))
    (ok (equal '(57) (mapcar #'round (pitches (transcription-raw-notes (transcribe (yin) audio))))))
    (ok (equal '(57 64) (mapcar #'round (pitches (transcription-raw-notes
                                                  (transcribe (yin :gate-db -60) audio))))))))

(deftest yin-pitch-range
  ;; 55 Hz (A1) is below the default range, C2 .. C6.
  (let ((audio (synthesize-notes (notes '(1/4 5/4 33)) :duration 3/2)))
    (ok (equalp #() (transcription-notes (transcribe (yin) audio))))
    (let ((ns (transcription-raw-notes (transcribe (yin) audio :hints '(:pitch-range (28 60))))))
      (ok (= 1 (length ns)))
      (ok (and (= 1 (length ns)) (close-to (note-event-pitch (svref ns 0)) 33 0.1d0))))))

(defparameter *yin-takes*
  ;; (waveform offset drift-rate vibrato-depth noise)
  '((:sine 0 0 0 0)
    (:sawtooth 0 0 0 0)
    (:sine 0 0 40 0)
    (:sawtooth 0 0 40 1/100)
    (:sine -15 -6 0 0)
    (:sawtooth 10 5 30 1/200)))

(deftest-parametrize yin-melodies
    ((waveform offset drift-rate vibrato-depth noise)
     :rows *yin-takes*)
  (let* ((truth (melody *melody-c* :start 1/5))
         (audio (synthesize-notes truth :waveform waveform :offset offset :drift-rate drift-rate
                                        :vibrato-rate 11/2 :vibrato-depth vibrato-depth
                                        :noise noise :seed 17))
         (tr (transcribe (yin) audio :hints '(:key "C")))
         (f (f-of truth (transcription-notes tr))))
    (ok (>= f 9/10) (format nil "note F-measure ~,3F" f))
    (ok (every (lambda (pair)
                 (<= (* 100 (abs (- (note-event-pitch (svref truth (car pair)))
                                    (note-event-pitch (svref (transcription-notes tr) (cdr pair))))))
                     20))
               (match-notes truth (transcription-notes tr)))
        "every matched note within 20 cents after the tuning and drift are removed")
    ;; The take's tuning as the notes were sung. Vibrato that starts upward and
    ;; ends part way through a period lifts a note's median a few cents.
    (ok (close-to (transcription-tuning-offset tr)
                  (estimate-tuning (detune-notes truth :offset offset :drift-rate drift-rate))
                  (if (zerop vibrato-depth) 3 8))
        "the take's tuning")))

(deftest yin-hint-distance
  (dolist (case '((0 0) (-100 -1) (100 1) (-30 0)))
    (destructuring-bind (offset distance) case
      (let* ((audio (synthesize-notes (melody *melody-d* :start 1/5) :waveform :sawtooth
                                      :offset offset))
             (tr (transcribe (yin) audio :hints '(:key "D"))))
        (ok (eql distance (mt:key-fit-hint-distance (transcription-key-fit tr)))
            (format nil "sung ~@D cents: distance ~@D" offset distance))))))

(deftest yin-resamples
  (let* ((truth (melody (subseq *melody-c* 0 5) :start 1/5))
         (audio (synthesize-notes truth :sample-rate 22050 :waveform :sawtooth))
         (tr (transcribe (yin) audio)))
    (ok (= 16000 (f0-contour-sample-rate (transcription-contour tr))))
    (ok (>= (f-of truth (transcription-notes tr)) 9/10))))
