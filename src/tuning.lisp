;;;; T0 tuning and drift: where the singer is relative to A4 = 440 Hz, phrase
;;;; by phrase, and the notes with that taken out.
;;;;
;;;; Singers are rarely at A440, and unaccompanied singers drift, usually flat,
;;;; a few cents per phrase. Backends report pitches as sung. These functions
;;;; measure the take's tuning and its drift from the notes alone, so every
;;;; backend gets the same correction, and FINISH-TRANSCRIPTION applies it.
;;;;
;;;; Arithmetic. Every pitch q is taken as its exact RATIONAL and every weight
;;;; is the note's duration in seconds, exact. So every result below is exact
;;;; and the same on every implementation; only REMOVE-DRIFT rounds, once.
;;;;
;;;; Residual. For a pitch q (MIDI) and an offset m (integer cents), the
;;;; residual is how far q lies from the nearest semitone of a tuning m cents
;;;; above A440:
;;;;     x = 100 q - m,   r(q, m) = x - 100 * floor(x / 100 + 1/2),   in [-50, 50).
;;;; r is periodic in m with period 100: a tuning and the same tuning a
;;;; semitone away fit every pitch equally, so the key absorbs whole semitones,
;;;; as music-theory's FIT-KEY does.
;;;; Cost. For notes N and an offset m, L(N, m) = sum over N of duration * |r(q, m)|.
;;;; The fitted offset is the m of least cost in a window of 100 integers, the
;;;; ties going to the first in +TUNING-OFFSETS+ order around the window's
;;;; centre: 0, -1, 1, -2, 2, ..., -49, 49, -50 (the smallest correction, then
;;;; the flatter one; music-theory's offset order). On a circle of 100 cents
;;;; this is the duration-weighted median of the residuals: robust to a few
;;;; notes sung out of tune, and free of the wrap-around a plain mean or median
;;;; of residuals suffers near a half semitone.
;;;;
;;;; (ESTIMATE-TUNING notes) -> g, an integer in -50 .. 49: the take's tuning
;;;;   offset in cents above A440 (negative = flat). g = the m of least
;;;;   L(notes, m) for m in +TUNING-OFFSETS+, first in that order among ties.
;;;;   No notes: every cost is 0, so g = 0. NOTES is any note sequence (order
;;;;   and overlaps don't matter; TYPE-ERROR or INVALID-NOTES "not a note
;;;;   event" otherwise). A take more than 50 cents from A440 reads as one a
;;;;   semitone away (-60 is read as +40), as in FIT-KEY.
;;;;
;;;; (SEGMENT-PHRASES notes &key (gap +phrase-gap+) (min-notes +min-phrase-notes+))
;;;;   -> a fresh list of (first . end) conses, half-open index ranges into
;;;;   NOTES, in order, covering every note once. NOTES is one line (CHECK-NOTES,
;;;;   first); GAP a non-negative REAL (seconds), MIN-NOTES an integer >= 1
;;;;   (CHECK-TYPE). Two steps:
;;;;   1. Split: a phrase ends before note i when the rest before it, onset(i)
;;;;      - offset(i-1), is at least GAP (a breath; 1/4 s by default).
;;;;   2. Merge: going left to right, a phrase of fewer than MIN-NOTES notes is
;;;;      joined to the phrase before it; a short phrase with none before it (at
;;;;      the start) is joined to the one after it, and so on until the
;;;;      collected notes reach MIN-NOTES. If the whole take has fewer notes it
;;;;      is one phrase. MIN-NOTES 1 keeps every split.
;;;;   No notes give NIL.
;;;;
;;;; (ESTIMATE-DRIFT notes &key tuning (gap +phrase-gap+) (min-notes +min-phrase-notes+))
;;;;   -> a fresh SIMPLE-VECTOR of PHRASE-DRIFT, one per phrase of
;;;;   (segment-phrases notes :gap gap :min-notes min-notes), in order. NOTES is
;;;;   one line (checked first), TUNING NIL or an integer (CHECK-TYPE); NIL
;;;;   means (estimate-tuning notes). For the phrase P of notes first ..
;;;;   end-1, the PHRASE-DRIFT has START = onset of note first, END = offset of
;;;;   note end-1, FIRST, COUNT = end - first, and CENTS = the d of least
;;;;   L(P, TUNING + d) for d in +TUNING-OFFSETS+, first in that order among
;;;;   ties. CENTS (-50 .. 49) is the phrase's drift relative to the take's
;;;;   tuning; TUNING + CENTS is the phrase's own offset from A440. The window
;;;;   is centred on the take's tuning, so a phrase's offset may lie beyond
;;;;   -50 .. 49 (a take at -40 that sinks to -70 keeps reading -70, not +30):
;;;;   a drift of up to a half semitone either side of the take's tuning is
;;;;   followed, and that window is the drift's clamp.
;;;;
;;;; (REMOVE-DRIFT notes drift &key (tuning 0)) -> a fresh SIMPLE-VECTOR of new
;;;;   NOTE-EVENTs: note i of every PHRASE-DRIFT D with FIRST <= i < FIRST +
;;;;   COUNT gets the pitch (rational pitch) - (TUNING + (phrase-drift-cents D))
;;;;   / 100, computed exactly and rounded once to a DOUBLE-FLOAT. Onset,
;;;;   offset and confidence stay. A note in no phrase keeps its pitch; a note
;;;;   in several takes the last. NOTES is one line (checked first); DRIFT a
;;;;   list or vector of PHRASE-DRIFTs and TUNING an integer (CHECK-TYPE); a
;;;;   phrase that reaches past the last note signals TRANSCRIBE-ERROR. With
;;;;   (estimate-drift notes :tuning g) and :TUNING g the notes come back at
;;;;   A440 (FINISH-TRANSCRIPTION); with the default :TUNING 0 only the drift
;;;;   relative to g is removed and the take keeps its tuning. A result pitch
;;;;   outside 0 .. 128 is MAKE-NOTE-EVENT's TYPE-ERROR.
;;;;
;;;; Consequences (the tests check them). A take with every pitch shifted by c
;;;; cents, c in -50 .. 49, gives g = c and every drift 0. A take that drifts
;;;; slowly by a few tens of cents, with phrases of a few seconds, comes back
;;;; from REMOVE-DRIFT with every note within 20 cents of its semitone.

(in-package #:transcribe-protocol)

(defparameter +tuning-offsets+
  (cons 0 (loop for d from 1 to 49 append (list (- d) d) into tail
                finally (return (append tail (list -50)))))
  "The 100 candidate offsets in cents, in tie order: 0 -1 1 -2 2 ... -49 49 -50.
The same order as music-theory's offsets for FIT-KEY. Data.")

(defparameter +phrase-gap+ 1/4
  "The shortest rest, in seconds, that ends a phrase (SEGMENT-PHRASES). Data.")

(defparameter +min-phrase-notes+ 3
  "The fewest notes a phrase keeps on its own; shorter phrases are merged
(SEGMENT-PHRASES). Data.")

(defstruct (phrase-drift (:constructor %make-phrase-drift (start end first count cents))
                         (:copier nil))
  "One phrase of a take: its time span, its notes (FIRST, COUNT) and its drift
CENTS relative to the take's tuning offset."
  (start 0 :type rational :read-only t)
  (end 0 :type rational :read-only t)
  (first 0 :type (integer 0) :read-only t)
  (count 1 :type (integer 1) :read-only t)
  (cents 0 :type (integer -50 49) :read-only t))

(defun make-phrase-drift (&key (start 0) (end 0) (first 0) (count 1) (cents 0))
  "A PHRASE-DRIFT. START and END are seconds (reals, stored as rationals, START
<= END), FIRST an index, COUNT >= 1 notes, CENTS -50 .. 49 (CHECK-TYPE)."
  (check-type start (real 0))
  (check-type end (real 0))
  (check-type first (integer 0))
  (check-type count (integer 1))
  (check-type cents (integer -50 49))
  (unless (<= start end)
    (error 'transcribe-error :reason (format nil "end ~S is before start ~S" end start)))
  (%make-phrase-drift (rational start) (rational end) first count cents))

;;; API -----------------------------------------------------------------------

(defun estimate-tuning (notes)
  "The take's tuning offset in cents, -50 .. 49: the duration-weighted circular
median of the notes' residuals. See tuning.lisp."
  (declare (ignorable notes))
  (%t0-pending 'estimate-tuning))

(defun segment-phrases (notes &key (gap +phrase-gap+) (min-notes +min-phrase-notes+))
  "Phrases of NOTES as half-open index ranges (first . end), split at rests of
at least GAP seconds, phrases of fewer than MIN-NOTES notes merged. See tuning.lisp."
  (declare (ignorable notes gap min-notes))
  (%t0-pending 'segment-phrases))

(defun estimate-drift (notes &key tuning (gap +phrase-gap+) (min-notes +min-phrase-notes+))
  "One PHRASE-DRIFT per phrase of NOTES: its drift in cents relative to TUNING
(default: ESTIMATE-TUNING). See tuning.lisp."
  (declare (ignorable notes tuning gap min-notes))
  (%t0-pending 'estimate-drift))

(defun remove-drift (notes drift &key (tuning 0))
  "NOTES with each phrase's offset TUNING + CENTS removed from its pitches, as
fresh NOTE-EVENTs. See tuning.lisp."
  (declare (ignorable notes drift tuning))
  (%t0-pending 'remove-drift))
