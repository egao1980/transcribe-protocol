;;;; T0 hints: what the caller knows about the take.
;;;;
;;;; HINTS is a property list with keyword keys, any of +HINT-KEYS+:
;;;;   :KEY          a music-theory KEY (any KEY), or a string PARSE-KEY reads
;;;;                 ("F#m", "Eb major"). The key the take is expected in:
;;;;                 FINISH-TRANSCRIPTION passes it to FIT-KEY as its HINT.
;;;;   :METER        a music-theory METER, or a string PARSE-METER reads ("3/4").
;;;;   :TEMPO        a music-theory TEMPO, or a positive REAL: beats per minute
;;;;                 of a quarter note, (make-tempo :bpm x).
;;;;   :PITCH-RANGE  (low high): MIDI numbers, REALs with 0 <= low < high < 128,
;;;;                 the range of the sung line. Backends search f0 there
;;;;                 (HINT-PITCH-RANGE).
;;;; Meter and tempo are not used by T0's own computations. They are checked
;;;; and echoed (TRANSCRIPTION-HINTS) in the types score-backend-midi's
;;;; NOTES-TO-SCORE takes for its :KEY, :METER and :TEMPO, so a caller passes
;;;; them on, and backends may use them.
;;;;
;;;; (NORMALIZE-HINTS hints) -> a fresh property list of the hints given, each
;;;;   value normalized: a KEY, a METER, a TEMPO (a KEY, METER or TEMPO given is
;;;;   kept as it is, EQ), and a fresh list (low high) of the reals given. The
;;;;   keys come in +HINT-KEYS+ order, each once. NIL gives NIL. Normalizing a
;;;;   normalized list gives an EQUAL list.
;;;;   HINTS must be a proper list of even length whose every other element,
;;;;   from the first, is a keyword; else TYPE-ERROR (CHECK-TYPE). Of a key
;;;;   given twice the first counts, as for GETF; later ones are not checked.
;;;;   Each hint is checked in the order given. A key outside +HINT-KEYS+, or
;;;;   a value of the wrong type, signals INVALID-HINT (KEY, VALUE, REASON) with
;;;;   ERROR inside two restarts:
;;;;     USE-VALUE (value)  check VALUE in its place, again (so it may signal
;;;;                        again); not offered for an unknown key;
;;;;     CONTINUE           drop the hint.
;;;;   A string that PARSE-KEY or PARSE-METER rejects gives INVALID-HINT with
;;;;   that music-theory condition as its CAUSE (the parse is unwound; its own
;;;;   restarts are not offered).
;;;;   REASONs: "unknown hint", "not a key", "not a meter",
;;;;   "not a tempo or a positive bpm", "not (low high) with 0 <= low < high < 128".
;;;;
;;;; (HINT-PITCH-RANGE hints) -> the :PITCH-RANGE of HINTS as a fresh list
;;;;   (low high), else a fresh copy of +DEFAULT-PITCH-RANGE+, (36 84): C2 to
;;;;   C6, about 65 to 1047 Hz, most singing voices. HINTS is normalized first (so
;;;;   it may signal as NORMALIZE-HINTS does).

(in-package #:transcribe-protocol)

(defparameter +hint-keys+ '(:key :meter :tempo :pitch-range)
  "The hints TRANSCRIBE accepts, in the order NORMALIZE-HINTS returns them. Data.")

(defparameter +default-pitch-range+ '(36 84)
  "The pitch range (low high) in MIDI numbers when no :PITCH-RANGE is given:
C2 to C6, about 65 to 1047 Hz. Data.")

(defun normalize-hints (hints)
  "HINTS checked and normalized: a fresh plist in +HINT-KEYS+ order. See hints.lisp."
  (declare (ignorable hints))
  (%t0-pending 'normalize-hints))

(defun hint-pitch-range (hints)
  "The :PITCH-RANGE of HINTS, else +DEFAULT-PITCH-RANGE+, as a fresh list (low high)."
  (declare (ignorable hints))
  (%t0-pending 'hint-pitch-range))
