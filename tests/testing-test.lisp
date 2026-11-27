;;;; transcribe-protocol/testing: the generator, melodies, detuned takes and
;;;; synthesis. Frequencies are measured by rising zero crossings, spectra
;;;; with audio-protocol/dsp's RFFT.

(in-package #:transcribe-protocol/tests)

(defconstant +m+ 2147483647)

(deftest park-miller-sequence
  (let ((random (make-park-miller 1)))
    (ok (eql (/ 16807d0 +m+) (funcall random)))
    (ok (eql (/ 282475249d0 +m+) (funcall random)))
    (loop repeat 9997 do (funcall random))
    ;; Park and Miller's check: x_10000 = 1043618065 from seed 1
    (ok (eql (/ 1043618065d0 +m+) (funcall random))))
  (let ((a (make-park-miller 42)) (b (make-park-miller 42)) (c (make-park-miller 43)))
    (let ((xs (loop repeat 50 collect (funcall a))))
      (ok (equal xs (loop repeat 50 collect (funcall b))))
      (ok (not (equal xs (loop repeat 50 collect (funcall c)))))
      (ok (every (lambda (x) (and (typep x 'double-float) (< 0d0 x 1d0))) xs))))
  (ok (signals (make-park-miller 0) 'type-error))
  (ok (signals (make-park-miller +m+) 'type-error))
  (ok (signals (make-park-miller 1.5) 'type-error)))

(deftest melody-notes-spec
  (let ((ns (melody-notes '((60 1/2) (nil 1/4) (62 0.25d0) (64 1)) :start 1 :confidence 1/2)))
    (ok (typep ns 'simple-vector))
    (ok (equalp (vector (note 1 3/2 60 1/2) (note 7/4 2 62 1/2) (note 2 3 64 1/2)) ns)))
  (ok (equalp #() (melody-notes '())))
  (ok (equalp #() (melody-notes '((nil 1)))))
  (ok (equalp (vector (note 0 1 61.5d0)) (melody-notes '((61.5d0 1)))))
  (ok (signals (melody-notes '((60))) 'type-error))
  (ok (signals (melody-notes '((60 0))) 'type-error))
  (ok (signals (melody-notes '((128 1))) 'type-error))
  (ok (signals (melody-notes '((60 1 2))) 'type-error))
  (ok (signals (melody-notes '((60 1)) :start -1) 'type-error)))

(deftest detune-notes-formula
  (let* ((ns (notes '(0 1 60 1/4) '(1 3 62)))
         (sung (detune-notes ns :offset 10 :drift-rate -4)))
    ;; midpoints 1/2 and 2: 10 - 2 = 8 cents, 10 - 8 = 2 cents
    (ok (equal (list (float (+ 60 8/100) 1d0) (float (+ 62 2/100) 1d0)) (pitches sung)))
    (ok (equal '(0 1) (map 'list #'note-event-onset sung)))
    (ok (equal '(1 3) (map 'list #'note-event-offset sung)))
    (ok (equal '(0.25d0 1d0) (map 'list #'note-event-confidence sung))))
  (let* ((ns (notes '(0 1 60) '(1 2 62) '(2 3 64)))
         (random (make-park-miller 5))
         (expected (map 'list (lambda (n)
                                (float (+ (rational (note-event-pitch n))
                                          (/ (* 6 (- (* 2 (rational (funcall random))) 1)) 100))
                                       1d0))
                        ns)))
    (ok (equal expected (pitches (detune-notes ns :jitter 6 :seed 5))))
    (ok (every (lambda (a b) (<= (* 100 (abs (- a b))) 6))
               (pitches ns) (pitches (detune-notes ns :jitter 6 :seed 5)))))
  (ok (equalp (melody *melody-c*) (detune-notes (melody *melody-c*))))
  (ok (signals (detune-notes (list (note 1 2 60) (note 0 1 60))) 'invalid-notes))
  (ok (signals (detune-notes #() :offset "10") 'type-error)))

(deftest synthesize-length-and-silence
  (let ((b (synthesize-notes (notes '(1/2 1 69)))))
    (ok (audio:pcm-buffer-p b))
    (ok (= 16000 (audio:pcm-buffer-sample-rate b)))
    (ok (= 1 (audio:pcm-buffer-channels b)))
    ;; the last offset + 1/4 s
    (ok (= 20000 (audio:pcm-buffer-frames b)))
    (let ((x (audio:pcm-buffer-samples b)))
      (ok (every #'zerop (subseq x 0 8000)) "silence before the note")
      (ok (every #'zerop (subseq x 16000)) "silence after it")
      (ok (some #'plusp (subseq x 8000 16000)))))
  (ok (= 4000 (audio:pcm-buffer-frames (synthesize-notes #()))))
  (ok (= 2205 (audio:pcm-buffer-frames (synthesize-notes #() :sample-rate 44100 :duration 1/20))))
  (ok (= 8000 (audio:pcm-buffer-frames (synthesize-notes (notes '(0 2 60)) :duration 1/2)))
      "DURATION cuts the take"))

(deftest synthesize-sine-samples
  ;; No envelope: sample n0 + k is A sin(2 pi f k / rate).
  (let* ((b (synthesize-notes (notes '(1/10 1/2 69)) :attack 0 :release 0 :amplitude 0.8))
         (x (audio:pcm-buffer-samples b)))
    (ok (zerop (aref x 1599)))
    (ok (every (lambda (k)
                 (close-to (aref x (+ 1600 k))
                           (* 0.8d0 (sin (/ (* 2 (float pi 1d0) 440d0 k) 16000))) 1d-6))
               '(0 1 2 3 17 100 1000 6399)))
    (ok (zerop (aref x 8000)) "the offset sample is silent")
    (ok (close-to (crossing-frequency x 1600 8000 16000) 440 0.01d0))))

(deftest synthesize-envelope
  ;; Linear ramps: e = min(1, u / attack, (D - u) / release).
  (let* ((b (synthesize-notes (notes '(0 1 69)) :attack 1/10 :release 1/5 :amplitude 1))
         (x (audio:pcm-buffer-samples b)))
    (flet ((expected (n)
             (let ((u (/ n 16000d0)))
               (* (min 1d0 (/ u 0.1d0) (/ (- 1d0 u) 0.2d0))
                  (sin (/ (* 2 (float pi 1d0) 440d0 n) 16000))))))
      (ok (every (lambda (n) (close-to (aref x n) (expected n) 1d-6))
                 '(1 37 800 1599 1600 5000 12800 13000 15999)))
      (ok (close-to (reduce #'max (subseq x 0 400) :key #'abs) 0.25d0 0.01d0)
          "a quarter of the attack: a quarter of the level")
      (ok (close-to (reduce #'max (subseq x 4000 12000) :key #'abs) 1d0 0.001d0)))))

(deftest synthesize-offset
  (let* ((b (synthesize-notes (notes '(0 1 69)) :offset 50))
         (x (audio:pcm-buffer-samples b)))
    (ok (close-to (crossing-frequency x 800 15200 16000) (* 440 (expt 2d0 (/ 50 1200))) 0.01d0))))

(deftest synthesize-drift
  ;; 10 cents flatter every second, from time 0: 25 cents between 0.75 s and 3.25 s.
  (let* ((b (synthesize-notes (notes '(0 4 69)) :drift-rate -10))
         (x (audio:pcm-buffer-samples b))
         (early (crossing-frequency x 8000 16000 16000))
         (late (crossing-frequency x 48000 56000 16000)))
    (ok (close-to early (* 440 (expt 2d0 (/ -7.5d0 1200))) 0.05d0))
    (ok (close-to late (* 440 (expt 2d0 (/ -32.5d0 1200))) 0.05d0))
    (ok (close-to (* 1200 (log (/ late early) 2)) -25 0.2d0))))

(deftest synthesize-vibrato
  ;; 5 Hz, 50 cents peak: each period's frequency swings 440 * 2^(+-50/1200).
  (let* ((b (synthesize-notes (notes '(0 2 69)) :vibrato-rate 5 :vibrato-depth 50))
         (x (audio:pcm-buffer-samples b))
         (fs (crossing-frequencies x 1600 30400 16000)))
    (ok (close-to (reduce #'max fs) (* 440 (expt 2d0 (/ 50 1200))) 1d0))
    (ok (close-to (reduce #'min fs) (* 440 (expt 2d0 (/ -50 1200))) 1d0))
    (ok (close-to (crossing-frequency x 1600 30400 16000) 440 1d0))
    (testing "no vibrato: a steady frequency"
      (let ((steady (crossing-frequencies
                     (audio:pcm-buffer-samples (synthesize-notes (notes '(0 2 69))))
                     1600 30400 16000)))
        (ok (< (- (reduce #'max steady) (reduce #'min steady)) 0.5d0))))))

(deftest synthesize-sawtooth-spectrum
  ;; 250 Hz is 64 samples a period at 16 kHz, so 4096 samples are 64 whole
  ;; periods and harmonic h falls on bin 64 h exactly. K = floor(7200 / 250) =
  ;; 28 partials, amplitudes (2 / pi) / h; nothing above.
  (let* ((b (synthesize-notes (vector (note 0 1 (hz-to-midi 250d0))) :waveform :sawtooth
                              :amplitude 1/2 :attack 0 :release 0))
         (x (subseq (audio:pcm-buffer-samples b) 4096 8192))
         (spectrum (dsp:magnitude (dsp:rfft x)))
         (scale (* 2048 1/2 (/ 2 (float pi 1d0)))))
    (ok (every (lambda (h) (close-to (/ (aref spectrum (* 64 h)) (/ scale h)) 1 0.01d0))
               (loop for h from 1 to 28 collect h))
        "the 28 partials at (2 / pi) / h")
    (ok (every (lambda (h) (< (aref spectrum (* 64 h)) (* 1d-3 scale)))
               '(29 30 31 32))
        "no partial past 0.45 of the rate")
    (ok (< (reduce #'max (subseq spectrum (* 64 29))) (* 1d-3 scale)))
    (ok (< (aref spectrum 32) (* 1d-3 scale)) "nothing between the partials")))

(deftest synthesize-noise
  (let* ((b (synthesize-notes #() :noise 1/10 :seed 3 :duration 1/10))
         (x (audio:pcm-buffer-samples b))
         (random (make-park-miller 3)))
    (ok (= 1600 (length x)))
    (ok (every (lambda (s) (= s (coerce (* 0.1d0 (- (* 2 (funcall random)) 1)) 'single-float))) x))
    (ok (every (lambda (s) (<= (abs s) 0.1)) x))
    (ok (close-to (dsp:rms x) (/ 0.1d0 (sqrt 3d0)) 0.005d0)))
  (ok (equalp (synthesize-notes (notes '(0 1/2 60)) :noise 0.01 :seed 9)
              (synthesize-notes (notes '(0 1/2 60)) :noise 0.01 :seed 9)))
  (ok (not (equalp (synthesize-notes (notes '(0 1/2 60)) :noise 0.01 :seed 9)
                   (synthesize-notes (notes '(0 1/2 60)) :noise 0.01 :seed 10)))))

(deftest-parametrize synthesize-arguments
    ((notes keys type)
     ((notes '(0 1 60)) '(:waveform :square) 'type-error)
     ((notes '(0 1 60)) '(:sample-rate 0) 'type-error)
     ((notes '(0 1 60)) '(:amplitude -1) 'type-error)
     ((notes '(0 1 60)) '(:noise -1) 'type-error)
     ((notes '(0 1 60)) '(:vibrato-depth -5) 'type-error)
     ((notes '(0 1 60)) '(:seed 0) 'type-error)
     ((notes '(0 1 60)) '(:duration -1) 'type-error)
     ((list (note 1 2 60) (note 0 1 62)) nil 'invalid-notes))
  (ok (signals (apply #'synthesize-notes notes keys) type)))
