;;;; T0 reference backends: an oracle for consumers' tests, and a plain YIN
;;;; baseline. Real backends live in their own repos (transcribe-backend-pyin).
;;;;
;;;; ORACLE-TRANSCRIBER (:notes notes)
;;;;   Returns NOTES whatever the audio, so a consumer can test its own code
;;;;   (scores, review screens) on a transcription with known notes. NOTES is
;;;;   one line (CHECK-NOTES at MAKE-INSTANCE, INVALID-NOTES), kept as a fresh
;;;;   SIMPLE-VECTOR (ORACLE-TRANSCRIBER-NOTES); default none.
;;;;   TRANSCRIBER-SAMPLE-RATE is NIL: the audio keeps its rate (no resampling).
;;;;   DETECT-NOTES returns a fresh copy of the notes and (notes-contour notes
;;;;   :sample-rate r :hop 256 :frames (frame-count n 256 256)) for a buffer of
;;;;   r Hz and n frames: one frame per 256 samples, ceiling(n / 256) frames
;;;;   (audio-protocol/dsp's FRAME-COUNT, :CENTER). The notes still pass through
;;;;   FINISH-TRANSCRIPTION, so detuned notes come back retuned.
;;;;
;;;; YIN-TRANSCRIBER — a baseline, not a product: YIN (de Cheveigné and
;;;; Kawahara 2002, steps 1-5) frame by frame through audio-protocol/dsp's
;;;; YIN-CMNDF, a level gate, and run-length segmentation. No probabilistic
;;;; thresholds, no Viterbi smoothing, no note model: that is pYIN, slice T1.
;;;; It shows the protocol end to end and gives later backends a floor to beat.
;;;;   Initargs and readers (YIN-TRANSCRIBER-...), checked at MAKE-INSTANCE with
;;;;   CHECK-TYPE:
;;;;     :FRAME-SIZE    N, an even integer >= 4, default 1024 (64 ms at 16 kHz)
;;;;     :HOP           an integer >= 1, default 128 (8 ms; 125 frames a second)
;;;;     :THRESHOLD     YIN's absolute threshold on d', a REAL in (0, 1], default 1/5
;;;;     :GATE-DB       a REAL <= 0, default -40: frames quieter than this, in dB
;;;;                    below the loudest frame, are unvoiced
;;;;     :JUMP          semitones, a positive REAL, default 3/4: a frame further
;;;;                    than this from its note's median so far starts a new note
;;;;     :MIN-DURATION  seconds, a non-negative REAL, default 3/50 (60 ms): shorter
;;;;                    runs are dropped
;;;;   TRANSCRIBER-SAMPLE-RATE is 16000 (the default).
;;;;   DETECT-NOTES on a mono buffer of L samples x_0 .. x_{L-1} at rate s, with
;;;;   W = N / 2 and (low high) = (hint-pitch-range hints):
;;;;   1. Frames. F = ceiling(L / HOP) frames (0 for L = 0). Frame i is the N
;;;;      samples x_{c - W/2} .. x_{c - W/2 + N - 1}, c = i * HOP, as doubles,
;;;;      zero outside 0 .. L-1: YIN's integration window, its first W samples,
;;;;      is centred on sample c, so frame i is at time i * HOP / s.
;;;;   2. d' = (yin-cmndf frame) (window W, lags 0 .. W).
;;;;   3. Lags. tau_lo = max(2, floor(s / (midi-to-hz high))) and tau_hi =
;;;;      min(W - 1, ceiling(s / (midi-to-hz low))). (C2 .. C6 at 16 kHz: 15 .. 245.)
;;;;   4. Absolute threshold (YIN step 4): tau = the first lag in tau_lo ..
;;;;      tau_hi with d'(tau) < THRESHOLD; then, while tau < tau_hi and
;;;;      d'(tau + 1) < d'(tau), tau = tau + 1 (the bottom of that dip). No such
;;;;      lag: the frame is unvoiced.
;;;;   5. Gate. The frame's RMS over its N samples; a frame whose RMS is below
;;;;      (db-to-gain GATE-DB) times the largest frame RMS is unvoiced, and so is
;;;;      every frame when that largest RMS is 0.
;;;;   6. Parabolic interpolation (YIN step 5) of d' at tau - 1, tau, tau + 1
;;;;      (a, b, c): tau* = tau + (a - c) / (2 (a - 2b + c)), or tau when a - 2b
;;;;      + c is not positive. f0 = s / tau* Hz; voicing = 1 - d'(tau), clamped
;;;;      to 0 .. 1. Unvoiced frames have f0 NIL and voicing 0.
;;;;   7. Notes. With m_i = (hz-to-midi f0_i), frames are cut into runs, left
;;;;      to right: an unvoiced frame ends the current run; a voiced frame
;;;;      starts a run when none is open, or when |m_i - M| > JUMP, M the median
;;;;      of m over the open run's frames so far (the mean of the two middle
;;;;      values for an even count), and otherwise joins the open run. (A glide
;;;;      between two notes moves a little each frame, so a step between
;;;;      consecutive frames would miss it. Early in a note the median sits off
;;;;      the centre of a vibrato, about 0.38 of its depth: with JUMP 3/4 a
;;;;      vibrato of up to about 50 cents keeps one note, and deeper vibrato
;;;;      splits it. Smoothing vibrato is pYIN's job, slice T1.) A run a .. b with (b - a + 1) * HOP / s < MIN-DURATION is
;;;;      dropped. Otherwise
;;;;      it is a NOTE-EVENT: onset a * HOP / s, offset (b + 1) * HOP / s, pitch
;;;;      the median of m_a .. m_b (the mean of the two middle values for an
;;;;      even count), confidence the mean voicing of the run.
;;;;   The contour is the F0-CONTOUR of rate s, HOP, and the frames' f0 and
;;;;   voicing. Runs never overlap, so the notes are one line.

(in-package #:transcribe-protocol)

;;; Oracle ---------------------------------------------------------------------

(defclass oracle-transcriber (transcriber)
  ((notes :initarg :notes :initform #() :reader oracle-transcriber-notes
          :documentation "The notes DETECT-NOTES returns, a SIMPLE-VECTOR."))
  (:documentation "A transcriber that returns given notes, whatever the audio."))

(defmethod initialize-instance :after ((oracle oracle-transcriber) &key)
  ;; T0: (check-notes notes), kept as a fresh SIMPLE-VECTOR (see the header).
  (%t0-pending 'oracle-transcriber))

(defmethod transcriber-sample-rate ((oracle oracle-transcriber))
  nil)

(defmethod detect-notes ((oracle oracle-transcriber) audio hints)
  (declare (ignorable audio hints))
  (%t0-pending 'oracle-transcriber))

;;; YIN baseline ---------------------------------------------------------------

(defclass yin-transcriber (transcriber)
  ((frame-size :initarg :frame-size :initform 1024 :reader yin-transcriber-frame-size)
   (hop :initarg :hop :initform 128 :reader yin-transcriber-hop)
   (threshold :initarg :threshold :initform 1/5 :reader yin-transcriber-threshold)
   (gate-db :initarg :gate-db :initform -40 :reader yin-transcriber-gate-db)
   (jump :initarg :jump :initform 3/4 :reader yin-transcriber-jump)
   (min-duration :initarg :min-duration :initform 3/50 :reader yin-transcriber-min-duration))
  (:documentation "A plain YIN baseline: frame-wise f0 by YIN's absolute
threshold, a level gate, run-length notes. See backends.lisp."))

(defmethod initialize-instance :after ((yin yin-transcriber) &key)
  (with-slots (frame-size hop threshold gate-db jump min-duration) yin
    (check-type frame-size (and (integer 4) (satisfies evenp)) "an even integer >= 4")
    (check-type hop (integer 1))
    (check-type threshold (real (0) 1))
    (check-type gate-db (real * 0))
    (check-type jump (real (0)))
    (check-type min-duration (real 0))))

(defmethod detect-notes ((yin yin-transcriber) audio hints)
  (declare (ignorable audio hints))
  (%t0-pending 'yin-transcriber))
