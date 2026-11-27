;;;; T0 protocol: transcribers, TRANSCRIBE, and the audio they are given.
;;;;
;;;; A backend is a subclass of TRANSCRIBER with a method on DETECT-NOTES, and
;;;; optionally on TRANSCRIBER-SAMPLE-RATE and TRANSCRIBER-INFO. Options of a
;;;; backend (frame sizes, thresholds, model paths) are its initargs; hints are
;;;; what the caller knows about the music.
;;;;
;;;; (TRANSCRIBE transcriber audio &key hints) -> a TRANSCRIPTION. A generic
;;;;   function, so callers may add :AROUND methods (caching, timing); its
;;;;   method on TRANSCRIBER does, in order:
;;;;   1. AUDIO must be an audio-protocol PCM-BUFFER (CHECK-TYPE). A
;;;;      TRANSCRIBER of no method is NO-APPLICABLE-METHOD, as for any generic
;;;;      function.
;;;;   2. hints = (normalize-hints HINTS): INVALID-HINT with its restarts.
;;;;   3. (prepare-audio transcriber audio) -> buffer, duration: UNUSABLE-AUDIO
;;;;      with its restarts.
;;;;   4. (detect-notes transcriber buffer hints) -> notes, contour.
;;;;   5. CONTOUR must be an F0-CONTOUR, else TRANSCRIBER-ERROR (REASON "not an
;;;;      f0 contour").
;;;;   6. (finish-transcription notes contour :hints hints :backend
;;;;      (transcriber-info transcriber) :duration duration). Notes that aren't
;;;;      one line in time order are CHECK-NOTES's INVALID-NOTES.
;;;;
;;;; (DETECT-NOTES transcriber audio hints) -> (values notes contour). The one
;;;;   generic function a backend must specialize. AUDIO is what PREPARE-AUDIO
;;;;   returned: a one-channel PCM-BUFFER at (transcriber-sample-rate
;;;;   transcriber), or at its own rate when that is NIL. It may be short or
;;;;   silent when a caller chose CONTINUE, so a method copes with any length,
;;;;   including 0 frames. HINTS is a normalized hint list. NOTES is a list or
;;;;   vector of NOTE-EVENTs, one line in time order, pitches as sung (MIDI
;;;;   with cents); CONTOUR an F0-CONTOUR. The method on TRANSCRIBER signals
;;;;   TRANSCRIBER-ERROR, REASON "no DETECT-NOTES method".
;;;;
;;;; (TRANSCRIBER-SAMPLE-RATE transcriber) -> the rate DETECT-NOTES wants, an
;;;;   integer >= 1, or NIL for the audio's own rate. Default +DEFAULT-SAMPLE-RATE+,
;;;;   16000.
;;;; (TRANSCRIBER-INFO transcriber) -> a property list describing the backend,
;;;;   echoed as TRANSCRIPTION-BACKEND. Default (:name "<class name in lower
;;;;   case>"), e.g. (:name "yin-transcriber").
;;;;
;;;; (PREPARE-AUDIO transcriber audio) -> (values buffer duration). TRANSCRIBER
;;;;   a TRANSCRIBER and AUDIO a PCM-BUFFER (CHECK-TYPE). Steps:
;;;;   1. mono: AUDIO itself when it has one channel, else (downmix audio), the
;;;;      channel mean (audio-protocol/dsp).
;;;;   2. too short: when its duration (PCM-BUFFER-DURATION, exact) is below
;;;;      *MINIMUM-DURATION* (1/10 s), signal AUDIO-TOO-SHORT (DURATION,
;;;;      MINIMUM) with ERROR.
;;;;   3. silent: when its PEAK (audio-protocol/dsp) is below (db-to-gain
;;;;      *SILENCE-PEAK-DB*), 0.001 at -60 dBFS, signal SILENT-AUDIO (DURATION,
;;;;      PEAK) with ERROR. A peak of exactly that gain is not silent.
;;;;   Each signal is inside two restarts, CLHS names:
;;;;      CONTINUE           waive this check and go on to the next step;
;;;;      USE-VALUE (audio)  start again from step 1 with AUDIO (CHECK-TYPE
;;;;                         first), which is checked like the first.
;;;;   4. rate: with R = (transcriber-sample-rate transcriber), the mono buffer
;;;;      when R is NIL or its own rate, else (resample mono R). R must be NIL
;;;;      or an integer >= 1, else TRANSCRIBER-ERROR.
;;;;   BUFFER may be AUDIO itself (nothing is modified); DURATION is the exact
;;;;   duration of the buffer that was prepared (before resampling), so after
;;;;   USE-VALUE it is the replacement's.
;;;;
;;;; *MINIMUM-DURATION* and *SILENCE-PEAK-DB* are special variables a caller may
;;;; bind: simple numeric thresholds, not rules.

(in-package #:transcribe-protocol)

(defconstant +default-sample-rate+ 16000
  "The rate a transcriber analyses unless it says otherwise: 16 kHz mono.")

(defvar *minimum-duration* 1/10
  "The shortest audio, in seconds, PREPARE-AUDIO accepts without AUDIO-TOO-SHORT.")

(defvar *silence-peak-db* -60
  "Audio whose peak is below this level in dBFS is SILENT-AUDIO (PREPARE-AUDIO).")

(defclass transcriber ()
  ()
  (:documentation "A monophonic transcription backend. Subclasses specialize
DETECT-NOTES, and optionally TRANSCRIBER-SAMPLE-RATE and TRANSCRIBER-INFO."))

(defgeneric transcriber-sample-rate (transcriber)
  (:documentation "The sample rate DETECT-NOTES wants, or NIL for the audio's own.")
  (:method ((transcriber transcriber))
    +default-sample-rate+))

(defgeneric transcriber-info (transcriber)
  (:documentation "A property list describing TRANSCRIBER, echoed in the TRANSCRIPTION.")
  (:method ((transcriber transcriber))
    (list :name (string-downcase (princ-to-string (class-name (class-of transcriber)))))))

(defgeneric detect-notes (transcriber audio hints)
  (:documentation "The backend's job: (values notes contour) for AUDIO, a mono
PCM-BUFFER from PREPARE-AUDIO, and normalized HINTS. See protocol.lisp.")
  (:method ((transcriber transcriber) audio hints)
    (declare (ignorable audio hints))
    (%t0-pending 'detect-notes)))

(defgeneric transcribe (transcriber audio &key hints)
  (:documentation "A TRANSCRIPTION of AUDIO, a PCM-BUFFER of one sung line, with
HINTS (:key :meter :tempo :pitch-range). See protocol.lisp."))

(defun prepare-audio (transcriber audio)
  "AUDIO as DETECT-NOTES gets it: mono, checked for length and silence,
resampled to the transcriber's rate; and its duration. See protocol.lisp."
  (declare (ignorable transcriber audio))
  (%t0-pending 'prepare-audio))

(defmethod transcribe ((transcriber transcriber) audio &key hints)
  (declare (ignorable audio hints))
  (%t0-pending 'transcribe))
