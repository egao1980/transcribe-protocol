;;;; T0 transcription: the result of TRANSCRIBE, and the step that finishes it.
;;;;
;;;; A TRANSCRIPTION is immutable (read-only slots), made by
;;;; FINISH-TRANSCRIPTION only:
;;;;   NOTES          a SIMPLE-VECTOR of NOTE-EVENTs, one line in time order, with
;;;;                  the take's tuning and drift removed: pitches at A440, ready
;;;;                  for spelling and quantisation (NOTE-PLISTS, then
;;;;                  score-backend-midi's NOTES-TO-SCORE)
;;;;   RAW-NOTES      the backend's notes as sung, same length and times
;;;;   CONTOUR        the backend's F0-CONTOUR, as sung
;;;;   TUNING-OFFSET  g, integer cents -50 .. 49 above A440 (ESTIMATE-TUNING)
;;;;   DRIFT          a SIMPLE-VECTOR of PHRASE-DRIFT, each phrase's drift
;;;;                  relative to g (ESTIMATE-DRIFT), possibly empty
;;;;   KEY-FIT        music-theory's KEY-FIT over NOTES, or NIL (below)
;;;;   HINTS          the normalized hints (NORMALIZE-HINTS)
;;;;   BACKEND        a property list describing the backend (TRANSCRIBER-INFO), or NIL
;;;;   DURATION       the transcribed audio's duration in seconds, a rational, or NIL
;;;;
;;;; (FINISH-TRANSCRIPTION notes contour &key hints backend duration
;;;;                       (phrase-gap +phrase-gap+) (min-phrase-notes +min-phrase-notes+))
;;;;   -> a TRANSCRIPTION. TRANSCRIBE calls it on what DETECT-NOTES returned; a
;;;;   backend never computes the tuning, the drift or the key fit itself.
;;;;   Checks, in order: NOTES with CHECK-NOTES (INVALID-NOTES); CONTOUR an
;;;;   F0-CONTOUR, BACKEND a property list with keyword keys (or NIL),
;;;;   DURATION NIL or a non-negative REAL (stored as its RATIONAL), PHRASE-GAP
;;;;   a non-negative REAL and MIN-PHRASE-NOTES an integer >= 1, with
;;;;   CHECK-TYPE; then HINTS with NORMALIZE-HINTS (INVALID-HINT, restarts).
;;;;   Then:
;;;;     raw     = (check-notes notes)
;;;;     g       = (estimate-tuning raw)
;;;;     drift   = (estimate-drift raw :tuning g :gap phrase-gap :min-notes min-phrase-notes)
;;;;     notes   = (remove-drift raw drift :tuning g)
;;;;     key-fit = (fit-key events :hint (getf hints :key)), music-theory MT5,
;;;;               EVENTS the corrected notes in order as (pitch . duration)
;;;;               conses, pitch the double and duration in seconds.
;;;;   FIT-KEY's NO-KEY-EVIDENCE (no notes, or a histogram constant at every
;;;;   offset, such as the twelve chromatic notes at equal durations) is
;;;;   answered here with its USE-VALUE restart and NIL: KEY-FIT is NIL, and the
;;;;   condition never reaches the caller. Other music-theory conditions pass.
;;;;   So with a :KEY hint, (key-fit-hint-distance (transcription-key-fit tr))
;;;;   says how the take relates to it: 0 in the key, -1 sung a semitone below
;;;;   it, +1 above. Because the fit runs on the corrected notes, its offset is
;;;;   what remains after g and the drift (about 0), and a take sung 30 cents
;;;;   flat in its key still reads distance 0.

(in-package #:transcribe-protocol)

(defstruct (transcription (:constructor %make-transcription
                              (notes raw-notes contour tuning-offset drift key-fit
                               hints backend duration))
                          (:copier nil))
  "The result of TRANSCRIBE (see transcription.lisp). Made by FINISH-TRANSCRIPTION."
  (notes #() :type simple-vector :read-only t)
  (raw-notes #() :type simple-vector :read-only t)
  (contour nil :type (or null f0-contour) :read-only t)
  (tuning-offset 0 :type (integer -50 49) :read-only t)
  (drift #() :type simple-vector :read-only t)
  (key-fit nil :type (or null mt:key-fit) :read-only t)
  (hints nil :type list :read-only t)
  (backend nil :type list :read-only t)
  (duration nil :type (or null rational) :read-only t))

(defmethod print-object ((tr transcription) stream)
  (print-unreadable-object (tr stream :type t :identity t)
    (format stream "~D note~:P, tuning ~@D cents"
            (length (transcription-notes tr)) (transcription-tuning-offset tr))))

(defun finish-transcription (notes contour &key hints backend duration
                                                (phrase-gap +phrase-gap+)
                                                (min-phrase-notes +min-phrase-notes+))
  "A TRANSCRIPTION from a backend's NOTES and CONTOUR: tuning, drift and key
fit computed by the protocol. See transcription.lisp."
  (declare (ignorable notes contour hints backend duration phrase-gap min-phrase-notes))
  (%t0-pending 'finish-transcription))
