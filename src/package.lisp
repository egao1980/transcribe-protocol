;;;; transcribe-protocol: monophonic audio-to-notes transcription for cl-stack.
;;;; Brief: cl-stack/docs/capabilities/transcribe.md, slice T0 (0.1.0).
;;;;
;;;; The first consumer is the monophonic transcription of sung or hummed
;;;; melodies: a recorded take, 16 kHz mono, in; note events out, with onsets
;;;; and offsets in seconds and pitches as MIDI numbers with cents, plus the f0
;;;; contour, the take's tuning offset, its drift per phrase, and a key fit.
;;;; Batch only: whole buffers in, fresh values out.
;;;;
;;;; Division of work. A backend (a TRANSCRIBER) only detects: DETECT-NOTES
;;;; gives the f0 contour and the note segments as sung. The protocol does
;;;; everything every backend would repeat: hints are checked and normalized,
;;;; audio is downmixed, checked for length and silence and resampled to the
;;;; backend's rate (PREPARE-AUDIO), and FINISH-TRANSCRIPTION estimates the
;;;; tuning offset and the per-phrase drift, removes them, and fits the key
;;;; over the corrected notes with music-theory's FIT-KEY.
;;;;
;;;; Files (each header is the exact spec of its part):
;;;;   conditions.lisp     the condition tree; restarts are CLHS USE-VALUE and CONTINUE
;;;;   notes.lisp          NOTE-EVENT, MIDI <-> Hz, CHECK-NOTES, NOTE-PLISTS
;;;;   contour.lisp        F0-CONTOUR (frame times, f0 or NIL, voicing), NOTES-CONTOUR
;;;;   hints.lisp          NORMALIZE-HINTS (:key :meter :tempo :pitch-range), HINT-PITCH-RANGE
;;;;   tuning.lisp         ESTIMATE-TUNING, SEGMENT-PHRASES, ESTIMATE-DRIFT, REMOVE-DRIFT,
;;;;                       PHRASE-DRIFT
;;;;   transcription.lisp  TRANSCRIPTION, FINISH-TRANSCRIPTION (tuning, drift, key fit)
;;;;   protocol.lisp       TRANSCRIBER, TRANSCRIBE, DETECT-NOTES, PREPARE-AUDIO,
;;;;                       TRANSCRIBER-SAMPLE-RATE, TRANSCRIBER-INFO
;;;;   evaluate.lisp       MATCH-NOTES, NOTE-F-MEASURE (mir_eval's transcription metrics)
;;;;   backends.lisp       ORACLE-TRANSCRIBER (returns given notes) and
;;;;                       YIN-TRANSCRIBER (a plain YIN baseline)
;;;;   testing.lisp        the system transcribe-protocol/testing: melodies, detuned
;;;;                       takes, PCM synthesis of note events
;;;;
;;;; Dependencies: audio-protocol 0.2.0 (PCM-BUFFER, RESAMPLE, the DSP package's
;;;; DOWNMIX, PEAK, FRAME-COUNT, YIN-CMNDF, RMS) and music-theory 0.6.0 (KEY,
;;;; METER, TEMPO, PARSE-KEY, PARSE-METER, FIT-KEY, KEY-FIT).

(defpackage #:transcribe-protocol
  (:use #:cl)
  (:nicknames #:stack-transcribe)
  (:local-nicknames (#:audio #:audio-protocol)
                    (#:dsp #:audio-protocol/dsp)
                    (#:mt #:music-theory))
  (:documentation "Monophonic audio-to-notes transcription for cl-stack (slice T0).

(TRANSCRIBE transcriber audio &key hints) turns an audio-protocol PCM-BUFFER of
one sung or hummed line into a TRANSCRIPTION: NOTE-EVENTs (onset and offset in
seconds as exact rationals, a MIDI pitch with cents as a double, a confidence),
the F0-CONTOUR (frame times, f0 in Hz or NIL, voicing probability), the take's
tuning offset in cents, its drift per phrase (PHRASE-DRIFT), and music-theory's
KEY-FIT over the corrected notes. HINTS is a plist of :KEY :METER :TEMPO and
:PITCH-RANGE.

Backends subclass TRANSCRIBER and write DETECT-NOTES only: the contour and the
notes as sung. The protocol prepares the audio (PREPARE-AUDIO: downmix, length
and silence checks, resampling) and finishes the result
(FINISH-TRANSCRIPTION: ESTIMATE-TUNING, ESTIMATE-DRIFT, REMOVE-DRIFT, FIT-KEY).

NOTE-F-MEASURE and MATCH-NOTES are mir_eval's note transcription metrics.
ORACLE-TRANSCRIBER returns given notes (for consumers' tests); YIN-TRANSCRIBER
is a plain YIN baseline. Test synthesis lives in transcribe-protocol/testing.")
  (:export
   ;; Conditions (conditions.lisp)
   #:transcribe-error #:transcribe-error-reason
   #:unusable-audio #:unusable-audio-duration
   #:audio-too-short #:audio-too-short-minimum
   #:silent-audio #:silent-audio-peak
   #:invalid-hint #:invalid-hint-key #:invalid-hint-value #:invalid-hint-cause
   #:invalid-notes #:invalid-notes-index
   #:transcriber-error #:transcriber-error-transcriber
   ;; Notes (notes.lisp)
   #:note-event #:make-note-event #:note-event-p
   #:note-event-onset #:note-event-offset #:note-event-pitch #:note-event-confidence
   #:note-event-duration #:midi-to-hz #:hz-to-midi
   #:check-notes #:note-plists
   ;; Contour (contour.lisp)
   #:f0-contour #:make-f0-contour #:f0-contour-p
   #:f0-contour-sample-rate #:f0-contour-hop #:f0-contour-f0 #:f0-contour-voicing
   #:contour-length #:contour-frame-time #:contour-frame-rate #:notes-contour
   ;; Hints (hints.lisp)
   #:+hint-keys+ #:+default-pitch-range+ #:normalize-hints #:hint-pitch-range
   ;; Tuning and drift (tuning.lisp)
   #:+tuning-offsets+ #:+phrase-gap+ #:+min-phrase-notes+
   #:phrase-drift #:make-phrase-drift #:phrase-drift-p
   #:phrase-drift-start #:phrase-drift-end #:phrase-drift-first #:phrase-drift-count
   #:phrase-drift-cents
   #:estimate-tuning #:segment-phrases #:estimate-drift #:remove-drift
   ;; Transcription (transcription.lisp)
   #:transcription #:transcription-p
   #:transcription-notes #:transcription-raw-notes #:transcription-contour
   #:transcription-tuning-offset #:transcription-drift #:transcription-key-fit
   #:transcription-hints #:transcription-backend #:transcription-duration
   #:finish-transcription
   ;; Protocol (protocol.lisp)
   #:+default-sample-rate+ #:*minimum-duration* #:*silence-peak-db*
   #:transcriber #:transcribe #:detect-notes #:prepare-audio
   #:transcriber-sample-rate #:transcriber-info
   ;; Evaluation (evaluate.lisp)
   #:match-notes #:note-f-measure
   ;; Backends (backends.lisp)
   #:oracle-transcriber #:oracle-transcriber-notes
   #:yin-transcriber #:yin-transcriber-frame-size #:yin-transcriber-hop
   #:yin-transcriber-threshold #:yin-transcriber-gate-db #:yin-transcriber-jump
   #:yin-transcriber-min-duration))

(in-package #:transcribe-protocol)
