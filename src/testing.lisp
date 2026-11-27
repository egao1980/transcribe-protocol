;;;; T0 test support (the system transcribe-protocol/testing): takes with known
;;;; notes, for this protocol's tests and for every backend's.
;;;;
;;;; (MAKE-PARK-MILLER seed) -> a function of no arguments returning the next
;;;;   value of the Park-Miller "minimal standard" generator (Park and Miller
;;;;   1988): x_{k+1} = 16807 x_k mod (2^31 - 1), x_0 = SEED, an integer
;;;;   1 .. 2^31 - 2 (CHECK-TYPE). Each call returns x_{k+1} / (2^31 - 1) as a
;;;;   DOUBLE-FLOAT in (0, 1). Integer arithmetic, so every implementation draws
;;;;   the same values; seed 1 gives x_1 = 16807 and x_10000 = 1043618065, the
;;;;   published check.
;;;;
;;;; (MELODY-NOTES spec &key (start 0) (confidence 1)) -> a fresh SIMPLE-VECTOR
;;;;   of NOTE-EVENTs, one after another from START seconds. SPEC is a list of
;;;;   (pitch duration) entries: PITCH a MIDI number (a REAL 0 .. 128) or NIL
;;;;   for a rest, DURATION a positive REAL in seconds (taken as its RATIONAL).
;;;;   A note runs from the current time t to t + duration; a rest only
;;;;   advances t. Every note gets CONFIDENCE. A malformed entry or argument is
;;;;   a TYPE-ERROR.
;;;;
;;;; (DETUNE-NOTES notes &key (offset 0) (drift-rate 0) (jitter 0) (seed 1)) ->
;;;;   fresh NOTE-EVENTs as a singer would give them: note k (one line,
;;;;   CHECK-NOTES) keeps its times and confidence and gets the pitch
;;;;     p_k + (OFFSET + DRIFT-RATE * mid_k + JITTER * (2 r_k - 1)) / 100,
;;;;   mid_k = (onset + offset) / 2 in seconds, r_k the k-th value of
;;;;   (make-park-miller SEED), computed exactly from the RATIONALs of every
;;;;   term and rounded once to a DOUBLE-FLOAT. OFFSET (cents), DRIFT-RATE
;;;;   (cents per second) and JITTER (cents, the half-width of a uniform
;;;;   detuning) are REALs. The drift is the pitch at the note's midpoint, which
;;;;   is what a tracker's median reports for SYNTHESIZE-NOTES with the same
;;;;   drift.
;;;;
;;;; (SYNTHESIZE-NOTES notes &key (sample-rate 16000) (waveform :sine)
;;;;                   (amplitude 1/2) (offset 0) (drift-rate 0)
;;;;                   (vibrato-rate 0) (vibrato-depth 0) (attack 1/100)
;;;;                   (release 1/100) (noise 0) (seed 1) duration)
;;;;   -> a fresh mono PCM-BUFFER at SAMPLE-RATE of NOTES (one line,
;;;;   CHECK-NOTES), sung as described. Arguments (CHECK-TYPE): SAMPLE-RATE an
;;;;   integer >= 1; WAVEFORM one of +SYNTHESIS-WAVEFORMS+; AMPLITUDE, ATTACK,
;;;;   RELEASE, NOISE, VIBRATO-RATE, VIBRATO-DEPTH non-negative REALs; OFFSET and
;;;;   DRIFT-RATE REALs; SEED as for MAKE-PARK-MILLER; DURATION NIL or a
;;;;   non-negative REAL.
;;;;   Length. DURATION, by default the last offset + 1/4 s (1/4 s without
;;;;   notes); F = (seconds-to-frame duration sample-rate) frames.
;;;;   Samples. Every computation is in DOUBLE-FLOAT; each sample is rounded once
;;;;   to a SINGLE-FLOAT at the end. Silence is 0 outside the notes. Note k
;;;;   (onset o, offset e, pitch p, D = e - o) covers the samples n from n0 =
;;;;   (seconds-to-frame o rate) below min(F, (seconds-to-frame e rate)). For
;;;;   each, t_n = n / rate and u_n = t_n - o (seconds) and
;;;;     pitch(n) = p + (OFFSET + DRIFT-RATE * t_n
;;;;                     + VIBRATO-DEPTH * sin(2 pi VIBRATO-RATE u_n)) / 100
;;;;     f(n)     = (midi-to-hz pitch(n))
;;;;     phase    phi_{n0} = 0, phi_{n+1} = phi_n + 2 pi f(n) / rate
;;;;     envelope e(n) = max(0, min(1, u_n / ATTACK, (D - u_n) / RELEASE)), a term
;;;;              left out when its ATTACK or RELEASE is 0
;;;;     sample   AMPLITUDE * e(n) * wave(phi_n)
;;;;   So OFFSET is a global tuning offset in cents, DRIFT-RATE a linear drift in
;;;;   cents per second from time 0, and VIBRATO-DEPTH the peak deviation in
;;;;   cents of a sinusoidal vibrato starting at phase 0 at each onset. Each
;;;;   note is its own oscillator, starting at phase 0.
;;;;   Waves. :SINE is sin(phi). :SAWTOOTH is band-limited, the first K terms of
;;;;   the rising sawtooth's Fourier series,
;;;;     (2 / pi) * sum_{h=1}^{K} (-1)^(h+1) sin(h phi) / h,
;;;;   with K = max(1, floor(0.45 * rate / f_k)) for the note's frequency at its
;;;;   onset without vibrato, f_k = (midi-to-hz (+ p (/ (+ OFFSET (* DRIFT-RATE
;;;;   o)) 100))): every partial stays below 0.45 of the rate, which leaves
;;;;   room for vibrato and drift under the Nyquist frequency. Its peak is about
;;;;   1.09 (Gibbs).
;;;;   Noise. When NOISE > 0, every sample n = 0 .. F-1 of the buffer, in order,
;;;;   gets NOISE * (2 r_n - 1) added, r_n the n-th value of (make-park-miller
;;;;   SEED): uniform noise in (-NOISE, NOISE), the same for the same seed.

(defpackage #:transcribe-protocol/testing
  (:use #:cl)
  (:nicknames #:stack-transcribe/testing)
  (:local-nicknames (#:tp #:transcribe-protocol)
                    (#:audio #:audio-protocol))
  (:documentation "Test support for transcribe-protocol and its backends:
MELODY-NOTES and DETUNE-NOTES build takes as note events, SYNTHESIZE-NOTES
renders them to a PCM-BUFFER (sine or band-limited sawtooth, a global offset, a
linear drift, vibrato, an envelope, seeded noise), and MAKE-PARK-MILLER is the
seeded generator they use.")
  (:export #:make-park-miller #:melody-notes #:detune-notes
           #:+synthesis-waveforms+ #:synthesize-notes))

(in-package #:transcribe-protocol/testing)

(defparameter +synthesis-waveforms+ '(:sine :sawtooth)
  "The waveforms SYNTHESIZE-NOTES renders. Data.")

(defconstant +park-miller-modulus+ 2147483647)

(defun make-park-miller (seed)
  "A generator of uniform doubles in (0, 1): the Park-Miller minimal standard
from SEED (1 .. 2^31 - 2). See testing.lisp."
  (check-type seed (integer 1 2147483646))
  (let ((x seed)
        (modulus (float +park-miller-modulus+ 1d0)))
    (lambda ()
      (setf x (mod (* 16807 x) +park-miller-modulus+))
      (/ (float x 1d0) modulus))))

(defun %melody-entry-p (entry)
  "True for (pitch duration): PITCH NIL or a REAL 0 .. 128 (exclusive), DURATION
a positive REAL."
  (and (tp::%proper-list-p entry)
       (= 2 (length entry))
       (typep (first entry) '(or null (real 0 (128))))
       (typep (second entry) '(real (0)))))

(defun melody-notes (spec &key (start 0) (confidence 1))
  "NOTE-EVENTs from SPEC, a list of (pitch duration), pitch NIL for a rest, one
after another from START seconds. See testing.lisp."
  (check-type spec (satisfies tp::%proper-list-p) "a list of (pitch duration) entries")
  (check-type start (real 0))
  (check-type confidence (real 0 1))
  (let ((time (rational start))
        (out '()))
    (dolist (entry spec)
      (check-type entry (satisfies %melody-entry-p)
                  "an entry (pitch duration): pitch NIL or a MIDI number 0 .. 128, duration positive")
      (destructuring-bind (pitch duration) entry
        (let ((end (+ time (rational duration))))
          (when pitch
            (push (tp:make-note-event :onset time :offset end :pitch pitch
                                      :confidence confidence)
                  out))
          (setf time end))))
    (coerce (nreverse out) 'simple-vector)))

(defun detune-notes (notes &key (offset 0) (drift-rate 0) (jitter 0) (seed 1))
  "NOTES sung OFFSET cents off, drifting DRIFT-RATE cents per second, each
detuned by up to JITTER cents. See testing.lisp."
  (let ((v (tp:check-notes notes)))
    (check-type offset real)
    (check-type drift-rate real)
    (check-type jitter real)
    (let ((random (make-park-miller seed))
          (out (make-array (length v))))
      ;; One draw per note, in order.
      (dotimes (k (length v) out)
        (let* ((note (svref v k))
               (r (funcall random))
               (onset (tp:note-event-onset note))
               (offset-time (tp:note-event-offset note))
               (mid (/ (+ onset offset-time) 2))
               (cents (+ (rational offset)
                         (* (rational drift-rate) mid)
                         (* (rational jitter) (- (* 2 (rational r)) 1)))))
          (setf (svref out k)
                (tp:make-note-event :onset onset :offset offset-time
                                    :pitch (float (+ (rational (tp:note-event-pitch note))
                                                     (/ cents 100))
                                                  1d0)
                                    :confidence (tp:note-event-confidence note))))))))

(defun %waveform-p (x)
  (member x +synthesis-waveforms+))

(defun %sawtooth (phi partials)
  "(2 / pi) * sum_{h=1}^{PARTIALS} (-1)^(h+1) sin(h PHI) / h, PHI the phase."
  (declare (type double-float phi) (type fixnum partials))
  (let ((sum 0d0))
    (declare (type double-float sum))
    (loop for h of-type fixnum from 1 to partials
          do (let ((term (/ (sin (* h phi)) h)))
               (if (oddp h) (incf sum term) (decf sum term))))
    (* (/ 2d0 (float pi 1d0)) sum)))

(defun %render-note (out note rate frames sawtooth amplitude offset drift-rate
                     vibrato-rate vibrato-depth attack release)
  "Write NOTE, its own oscillator from phase 0, into the DOUBLE-FLOAT vector OUT
(see the file header). Every parameter is a DOUBLE-FLOAT but RATE and FRAMES."
  (declare (type (simple-array double-float (*)) out)
           (type (integer 1) rate) (type fixnum frames)
           (type double-float amplitude offset drift-rate vibrato-rate vibrato-depth
                 attack release))
  (let* ((onset (tp:note-event-onset note))
         (end (tp:note-event-offset note))
         (pitch (tp:note-event-pitch note))
         (n0 (audio:seconds-to-frame onset rate))
         (n1 (min frames (audio:seconds-to-frame end rate)))
         (o (float onset 1d0))
         (d (float (- end onset) 1d0))
         (rate-d (float rate 1d0))
         (two-pi (* 2d0 (float pi 1d0)))
         ;; Partials below 0.45 of the rate at the onset frequency, without vibrato.
         (partials (if sawtooth
                       (max 1 (floor (* 9/20 rate)
                                     (tp:midi-to-hz (+ pitch (/ (+ offset (* drift-rate o)) 100d0)))))
                       0))
         (phi 0d0))
    (declare (type double-float pitch o d rate-d two-pi phi)
             (type fixnum n0 n1 partials))
    (loop for n of-type fixnum from n0 below n1
          do (let* ((tn (/ (float n 1d0) rate-d))
                    (u (- tn o))
                    (f (tp:midi-to-hz
                        (+ pitch (/ (+ offset (* drift-rate tn)
                                       (* vibrato-depth (sin (* two-pi vibrato-rate u))))
                                    100d0))))
                    (envelope 1d0))
               (declare (type double-float tn u f envelope))
               ;; A term is left out when its ATTACK or RELEASE is 0.
               (when (plusp attack)
                 (setf envelope (min envelope (/ u attack))))
               (when (plusp release)
                 (setf envelope (min envelope (/ (- d u) release))))
               (setf envelope (max 0d0 envelope))
               (setf (aref out n)
                     (* amplitude envelope (if sawtooth (%sawtooth phi partials) (sin phi))))
               ;; phi_{n+1} = phi_n + 2 pi f(n) / rate
               (incf phi (/ (* two-pi f) rate-d))))
    out))

(defun synthesize-notes (notes &key (sample-rate 16000) (waveform :sine) (amplitude 1/2)
                                    (offset 0) (drift-rate 0) (vibrato-rate 0)
                                    (vibrato-depth 0) (attack 1/100) (release 1/100)
                                    (noise 0) (seed 1) duration)
  "A mono PCM-BUFFER of NOTES as sung: sine or band-limited sawtooth, a tuning
OFFSET and DRIFT-RATE in cents, vibrato, an envelope, seeded noise. See testing.lisp."
  (let ((v (tp:check-notes notes)))
    (check-type sample-rate (integer 1))
    (check-type waveform (satisfies %waveform-p) "one of +SYNTHESIS-WAVEFORMS+")
    (check-type amplitude (real 0))
    (check-type attack (real 0))
    (check-type release (real 0))
    (check-type noise (real 0))
    (check-type vibrato-rate (real 0))
    (check-type vibrato-depth (real 0))
    (check-type offset real)
    (check-type drift-rate real)
    (check-type seed (integer 1 2147483646))
    (check-type duration (or null (real 0)))
    (let* ((count (length v))
           (seconds (or duration
                        (+ (if (plusp count) (tp:note-event-offset (svref v (1- count))) 0) 1/4)))
           (frames (audio:seconds-to-frame seconds sample-rate))
           (out (make-array frames :element-type 'double-float :initial-element 0d0)))
      (loop for note across v
            do (%render-note out note sample-rate frames (eq waveform :sawtooth)
                             (float amplitude 1d0) (float offset 1d0) (float drift-rate 1d0)
                             (float vibrato-rate 1d0) (float vibrato-depth 1d0)
                             (float attack 1d0) (float release 1d0)))
      ;; Noise last, sample by sample in buffer order, from a fresh generator.
      (when (plusp noise)
        (let ((random (make-park-miller seed))
              (level (float noise 1d0)))
          (dotimes (n frames)
            (incf (aref out n) (* level (- (* 2 (funcall random)) 1))))))
      ;; MAKE-PCM-BUFFER rounds each sample once to a SINGLE-FLOAT.
      (audio:make-pcm-buffer :sample-rate sample-rate :channels 1 :samples out))))
