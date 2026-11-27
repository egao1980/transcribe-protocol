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
  ;; One line, kept as a fresh SIMPLE-VECTOR.
  (setf (slot-value oracle 'notes) (check-notes (slot-value oracle 'notes))))

(defmethod transcriber-sample-rate ((oracle oracle-transcriber))
  nil)

(defmethod detect-notes ((oracle oracle-transcriber) audio hints)
  (declare (ignore hints))
  (let ((notes (oracle-transcriber-notes oracle)))
    (values (copy-seq notes)
            (notes-contour notes
                           :sample-rate (audio:pcm-buffer-sample-rate audio)
                           :hop 256
                           :frames (dsp:frame-count (audio:pcm-buffer-frames audio) 256 256)))))

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

(defun %yin-frame (samples start size)
  "SIZE samples of SAMPLES from index START on, as a fresh DOUBLE-FLOAT vector,
0 outside SAMPLES (step 1)."
  (declare (type audio:sample-vector samples)
           (type fixnum start size))
  (let ((frame (make-array size :element-type 'double-float :initial-element 0d0))
        (length (length samples)))
    (loop for j of-type fixnum from (max 0 (- start)) below (min size (- length start))
          do (setf (aref frame j) (float (aref samples (+ start j)) 1d0)))
    frame))

(defun %yin-lag (d tau-lo tau-hi threshold)
  "YIN step 4 on the d' vector D: the first lag in TAU-LO .. TAU-HI with d'
below THRESHOLD, then the bottom of that dip; NIL when there is none."
  (declare (type %double-vector d))
  (loop for tau from tau-lo to tau-hi
        when (< (aref d tau) threshold)
          do (let ((k tau))
               (loop while (and (< k tau-hi) (< (aref d (1+ k)) (aref d k)))
                     do (incf k))
               (return k))))

(defun %yin-period (d tau)
  "YIN step 5: the parabolic vertex tau* of d' at TAU - 1, TAU, TAU + 1, or TAU
when a - 2b + c is not positive."
  (declare (type %double-vector d))
  (let* ((a (aref d (1- tau)))
         (b (aref d tau))
         (c (aref d (1+ tau)))
         (den (+ (- a (* 2d0 b)) c))
         (vertex (if (plusp den)
                     (+ tau (/ (- a c) (* 2d0 den)))
                     (float tau 1d0))))
    ;; The one guard, outside the header's formula. Where a >= b <= c the
    ;; vertex is within 1/2 of TAU. Only at the end of the lag range (TAU =
    ;; tau_lo with a < b) can it stray, and only there can it be 0 or less,
    ;; which would give no valid f0. Then TAU is used.
    (if (plusp vertex) vertex (float tau 1d0))))

(defun %median (sorted count)
  "The median of the COUNT numbers of the ascending list SORTED: the middle
value, or the mean of the two middle values for an even COUNT."
  (multiple-value-bind (half odd) (floor count 2)
    (if (= odd 1)
        (nth half sorted)
        (let ((tail (nthcdr (1- half) sorted)))
          (/ (+ (first tail) (second tail)) 2)))))

(defmethod detect-notes ((yin yin-transcriber) audio hints)
  (let* ((samples (audio:pcm-buffer-samples audio))
         (s (audio:pcm-buffer-sample-rate audio))
         (length (length samples))
         (n (yin-transcriber-frame-size yin))
         (w (floor n 2))
         (hop (yin-transcriber-hop yin))
         (threshold (yin-transcriber-threshold yin))
         (jump (yin-transcriber-jump yin))
         (min-duration (yin-transcriber-min-duration yin))
         (range (hint-pitch-range hints))
         (tau-lo (max 2 (floor s (midi-to-hz (second range)))))
         (tau-hi (min (1- w) (ceiling s (midi-to-hz (first range)))))
         ;; F = ceiling(L / HOP). Frame i starts where the W-sample integration
         ;; window centred on c = i HOP starts under audio-protocol/dsp's
         ;; :CENTER convention, c - floor(W/2), and runs on for N samples.
         (count (dsp:frame-count length w hop))
         (rms (make-array count :element-type 'double-float :initial-element 0d0))
         (f0 (make-array count :initial-element nil))
         (voicing (make-array count :element-type 'double-float :initial-element 0d0)))
    (flet ((frame (i)
             (%yin-frame samples (dsp:frame-start i w hop) n)))
      ;; Step 5's gate needs the loudest frame first: RMS of every frame, then
      ;; YIN (steps 2-4, 6) only where the gate lets the frame in.
      (dotimes (i count)
        (setf (aref rms i) (dsp:rms (frame i))))
      (let* ((loudest (reduce #'max rms :initial-value 0d0))
             (gate (* (audio:db-to-gain (yin-transcriber-gate-db yin)) loudest)))
        (when (plusp loudest)
          (dotimes (i count)
            (unless (< (aref rms i) gate)
              (let* ((d (dsp:yin-cmndf (frame i) :window w))   ; lags 0 .. W
                     (tau (%yin-lag d tau-lo tau-hi threshold)))
                (when tau
                  (setf (svref f0 i) (/ s (%yin-period d tau))
                        (aref voicing i) (max 0d0 (min 1d0 (- 1d0 (aref d tau))))))))))))
    ;; Step 7: runs of voiced frames, each within JUMP of its running median.
    (let ((notes '())
          (start nil)                     ; the open run's first frame, or NIL
          (sorted '())                    ; its m values, ascending
          (size 0)
          (voicing-sum 0d0))
      (flet ((close-run (end)
               ;; The open run is frames START .. END.
               (when start
                 (when (>= (/ (* (1+ (- end start)) hop) s) min-duration)
                   (push (make-note-event :onset (/ (* start hop) s)
                                          :offset (/ (* (1+ end) hop) s)
                                          :pitch (%median sorted size)
                                          :confidence (min 1d0 (/ voicing-sum size)))
                         notes))
                 (setf start nil
                       sorted '()
                       size 0
                       voicing-sum 0d0))))
        (dotimes (i count)
          (let ((f (svref f0 i)))
            (if (null f)
                (close-run (1- i))
                (let ((m (hz-to-midi f)))
                  (when (and start (> (abs (- m (%median sorted size))) jump))
                    (close-run (1- i)))
                  (unless start
                    (setf start i))
                  (setf sorted (merge 'list (list m) sorted #'<))
                  (incf size)
                  (incf voicing-sum (aref voicing i))))))
        (close-run (1- count)))
      (values (nreverse notes)
              (make-f0-contour :sample-rate s :hop hop :f0 f0 :voicing voicing)))))
