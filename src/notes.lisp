;;;; T0 notes: note events, MIDI numbers and Hz, note sequences.
;;;;
;;;; A NOTE-EVENT is one sung note, immutable, compared with EQUALP:
;;;;   ONSET, OFFSET  seconds from the start of the audio, exact RATIONALs,
;;;;                  0 <= ONSET < OFFSET. Frame-based backends give exact
;;;;                  times (frame i is at i * hop / rate), and the consumers
;;;;                  (score-backend-midi's NOTES-TO-SCORE, NOTE-F-MEASURE)
;;;;                  compute with rationals, so nothing drifts or depends on
;;;;                  float rounding.
;;;;   PITCH          the pitch centre as a MIDI number with cents in its
;;;;                  fraction (61.37 is C#4 and 37 cents), a DOUBLE-FLOAT,
;;;;                  0 <= PITCH < 128. A pitch from an f0 is a logarithm, so
;;;;                  not rational; music-theory's FIT-KEY takes it as its
;;;;                  exact RATIONAL.
;;;;   CONFIDENCE     the backend's confidence in the note, a DOUBLE-FLOAT 0..1.
;;;;
;;;; (MAKE-NOTE-EVENT &key onset offset pitch (confidence 1)) checks each
;;;; argument with CHECK-TYPE (TYPE-ERROR, STORE-VALUE) in that order: ONSET a
;;;; (REAL 0), OFFSET a REAL, PITCH a (REAL 0 (128)), CONFIDENCE a (REAL 0 1).
;;;; ONSET and OFFSET are stored as their exact RATIONAL (of a float, its exact
;;;; value: 0.5d0 is 1/2), PITCH and CONFIDENCE as DOUBLE-FLOATs. Then OFFSET
;;;; must be above ONSET, else TRANSCRIBE-ERROR (no restart).
;;;; (NOTE-EVENT-DURATION note) is OFFSET - ONSET, exact.
;;;;
;;;; MIDI and Hz, equal temperament at A4 = 440 Hz, in DOUBLE-FLOAT:
;;;;   (MIDI-TO-HZ m) = 440 * 2^((m - 69) / 12)   M a REAL (else TYPE-ERROR)
;;;;   (HZ-TO-MIDI f) = 69 + 12 * log2(f / 440)   F a positive REAL
;;;; MIDI-TO-HZ of 69 is exactly 440d0, HZ-TO-MIDI of 440 exactly 69d0, and
;;;; either undoes the other within 1e-9.
;;;;
;;;; A note sequence is a list or a vector of NOTE-EVENTs.
;;;;
;;;; (CHECK-NOTES notes) -> a fresh SIMPLE-VECTOR of the notes, in order. It is
;;;;   the check for one melodic line, used by every function here that needs
;;;;   one (and by FINISH-TRANSCRIPTION on a backend's notes). NOTES is a list
;;;;   or a vector (else TYPE-ERROR). Then, in order of position i, the first
;;;;   failure signals INVALID-NOTES with INDEX i and REASON:
;;;;     "not a note event"   element i isn't a NOTE-EVENT;
;;;;     "out of order"       its onset is before the onset of element i-1;
;;;;     "overlapping notes"  its onset is before the offset of element i-1
;;;;                          (so equal onsets overlap). A note may start
;;;;                          exactly where the one before ends.
;;;;   Nothing is sorted or cut: a backend gives one line in time order.
;;;;
;;;; (NOTE-PLISTS notes) -> a fresh list with one property list
;;;;   (:onset o :offset f :pitch p :confidence c) per note, in order, the
;;;;   event shape of score-backend-midi's NOTES-TO-SCORE (SC6). NOTES is any
;;;;   note sequence, in any order and overlapping or not (TYPE-ERROR for
;;;;   another object; INVALID-NOTES "not a note event" for an element that
;;;;   isn't a NOTE-EVENT).

(in-package #:transcribe-protocol)

(defstruct (note-event (:constructor %make-note-event (onset offset pitch confidence))
                       (:copier nil))
  "One sung note: onset and offset in seconds (rationals), a MIDI pitch with
cents (a double), a confidence 0..1. Make one with MAKE-NOTE-EVENT."
  (onset 0 :type rational :read-only t)
  (offset 1 :type rational :read-only t)
  (pitch 60d0 :type double-float :read-only t)
  (confidence 1d0 :type double-float :read-only t))

(defun make-note-event (&key onset offset pitch (confidence 1))
  "A NOTE-EVENT from ONSET and OFFSET (seconds, stored as exact rationals),
PITCH (a MIDI number with cents, stored as a double) and CONFIDENCE (0..1)."
  (check-type onset (real 0))
  (check-type offset real)
  (check-type pitch (real 0 (128)))
  (check-type confidence (real 0 1))
  (let ((on (rational onset))
        (off (rational offset)))
    (unless (< on off)
      (error 'transcribe-error
             :reason (format nil "offset ~S is not after onset ~S" offset onset)))
    (%make-note-event on off (float pitch 1d0) (float confidence 1d0))))

(defun note-event-duration (note)
  "OFFSET - ONSET of NOTE, in seconds, exact."
  (check-type note note-event)
  (- (note-event-offset note) (note-event-onset note)))

(defun midi-to-hz (midi)
  "440 * 2^((MIDI - 69) / 12) Hz, a DOUBLE-FLOAT."
  (check-type midi real)
  (* 440d0 (expt 2d0 (/ (- (float midi 1d0) 69d0) 12d0))))

(defun hz-to-midi (hz)
  "69 + 12 * log2(HZ / 440), a DOUBLE-FLOAT MIDI number with cents."
  (check-type hz (real (0)))
  (+ 69d0 (* 12d0 (log (/ (float hz 1d0) 440d0) 2d0))))

(defun %proper-list-p (x)
  (handler-case (list-length x) (type-error () nil)))

(defun %note-vector (notes &optional line)
  "NOTES, a list or vector of NOTE-EVENTs (else TYPE-ERROR), as a fresh
SIMPLE-VECTOR. Position by position, an element that isn't a NOTE-EVENT is
INVALID-NOTES; with LINE true, so is a note out of order or overlapping the
one before it (the CHECK-NOTES rules)."
  (check-type notes (or vector (satisfies %proper-list-p)) "a list or vector of NOTE-EVENTs")
  ;; MAP, not COERCE: COERCE returns a SIMPLE-VECTOR argument itself.
  (let ((v (map 'simple-vector #'identity notes)))
    (dotimes (i (length v) v)
      (flet ((fail (reason)
               (error 'invalid-notes :index i :reason reason)))
        (let ((note (svref v i)))
          (unless (note-event-p note)
            (fail "not a note event"))
          (when (and line (plusp i))
            (let ((before (svref v (1- i))))
              (cond ((< (note-event-onset note) (note-event-onset before))
                     (fail "out of order"))
                    ((< (note-event-onset note) (note-event-offset before))
                     (fail "overlapping notes"))))))))))

(defun check-notes (notes)
  "NOTES, one melodic line in time order, as a fresh SIMPLE-VECTOR; else
INVALID-NOTES at the first offending index. See notes.lisp."
  (%note-vector notes t))

(defun note-plists (notes)
  "One (:onset :offset :pitch :confidence) property list per note of NOTES,
the event shape of score-backend-midi's NOTES-TO-SCORE."
  (map 'list (lambda (note)
               (list :onset (note-event-onset note)
                     :offset (note-event-offset note)
                     :pitch (note-event-pitch note)
                     :confidence (note-event-confidence note)))
       (%note-vector notes)))
