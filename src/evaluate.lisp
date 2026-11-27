;;;; T0 evaluation: note-level precision, recall and F-measure, as mir_eval's
;;;; transcription metrics compute them (mir_eval 0.8,
;;;; mir_eval.transcription.precision_recall_f1_overlap and match_notes; MIT;
;;;; Raffel et al., ISMIR 2014). The MIREX note-tracking criteria: an
;;;; estimated note is correct when its onset is within 50 ms of a reference
;;;; note's and its pitch within a quarter tone (50 cents), and, when offsets
;;;; count, its offset within 20 % of the reference note's duration or 50 ms,
;;;; whichever is larger.
;;;;
;;;; Notes here are any note sequences (lists or vectors of NOTE-EVENTs), in
;;;; any order, overlapping or not: reference and estimate may be polyphonic
;;;; (TYPE-ERROR for another object, INVALID-NOTES "not a note event" for an
;;;; element that isn't a NOTE-EVENT; REFERENCE is checked first).
;;;;
;;;; Keywords (CHECK-TYPE, in this order):
;;;;   ONSET-TOLERANCE       seconds, a non-negative REAL, default 1/20
;;;;   PITCH-TOLERANCE-CENTS cents, a non-negative REAL, default 50
;;;;   OFFSET-RATIO          NIL (default: offsets are ignored, "onset only"), or
;;;;                         a non-negative REAL; mir_eval's default with offsets is 1/5
;;;;   OFFSET-MIN-TOLERANCE  seconds, a non-negative REAL, default 1/20
;;;;   STRICT                NIL (default) compares with <=, true with <
;;;;
;;;; Candidates. Reference note i (onset a_i, offset b_i, pitch p_i) and
;;;; estimated note j (a'_j, b'_j, p'_j) may match when, with <= (or < when
;;;; STRICT) for every comparison:
;;;;   |a_i - a'_j| <= ONSET-TOLERANCE,
;;;;   100 |p_i - p'_j| <= PITCH-TOLERANCE-CENTS   (MIDI with cents, so this is
;;;;                       mir_eval's 1200 |log2(f_i / f'_j)| exactly),
;;;;   and, when OFFSET-RATIO is a number,
;;;;   |b_i - b'_j| <= max(OFFSET-RATIO * (b_i - a_i), OFFSET-MIN-TOLERANCE).
;;;; Every quantity is exact: times are rationals and pitches are taken as their
;;;; exact RATIONAL, so a distance equal to its tolerance compares exactly.
;;;; (mir_eval rounds time distances to 4 decimals for the same purpose.)
;;;;
;;;; Matching. Each note matches at most one note of the other side. The
;;;; matching is a MAXIMUM cardinality bipartite matching of the candidate
;;;; graph, as mir_eval's (Hopcroft-Karp, through util._bipartite_match): never
;;;; a greedy one, which can match a note to a neighbour and leave a note
;;;; unmatched that a maximum matching pairs. The maximum's size is unique;
;;;; which maximum matching is returned is not specified.
;;;;
;;;; (MATCH-NOTES reference estimate &key ...) -> a fresh list of (i . j)
;;;;   conses, a maximum matching, sorted by i: reference index i, estimate
;;;;   index j (0-based positions in the sequences given).
;;;;
;;;; (NOTE-F-MEASURE reference estimate &key ...) -> (values f precision recall
;;;;   matched), with m = the size of a maximum matching:
;;;;     precision = m / (number of estimated notes)
;;;;     recall    = m / (number of reference notes)
;;;;     f         = 2 precision recall / (precision + recall), 0 when both are 0
;;;;   all exact RATIONALs (1, 2/3, 0), and MATCHED = m. When either side has no
;;;;   notes, mir_eval's rule: 0 0 0 0.

(in-package #:transcribe-protocol)

(defun match-notes (reference estimate &key (onset-tolerance 1/20) (pitch-tolerance-cents 50)
                                            offset-ratio (offset-min-tolerance 1/20) strict)
  "A maximum matching of REFERENCE and ESTIMATE notes under mir_eval's
criteria, as (reference-index . estimate-index) conses. See evaluate.lisp."
  (declare (ignorable reference estimate onset-tolerance pitch-tolerance-cents
                      offset-ratio offset-min-tolerance strict))
  (%t0-pending 'match-notes))

(defun note-f-measure (reference estimate &key (onset-tolerance 1/20) (pitch-tolerance-cents 50)
                                               offset-ratio (offset-min-tolerance 1/20) strict)
  "(values f precision recall matched) of ESTIMATE against REFERENCE, mir_eval's
note metrics, exact. See evaluate.lisp."
  (declare (ignorable reference estimate onset-tolerance pitch-tolerance-cents
                      offset-ratio offset-min-tolerance strict))
  (%t0-pending 'note-f-measure))
