;;;; T0 conditions. One root, TRANSCRIBE-ERROR, and subclasses for the cases a
;;;; handler distinguishes. Backends signal these types; they define no tree of
;;;; their own.
;;;;
;;;; transcribe-error (error)          REASON, a short lower-case string or NIL
;;;; ├── unusable-audio                DURATION: the audio's length in seconds
;;;; │   ├── audio-too-short           MINIMUM: *MINIMUM-DURATION* when signalled
;;;; │   └── silent-audio              PEAK: the downmixed audio's peak, a DOUBLE-FLOAT
;;;; ├── invalid-hint                  KEY (the hint's keyword), VALUE, CAUSE (a
;;;; │                                 music-theory condition from PARSE-KEY or
;;;; │                                 PARSE-METER, or NIL)
;;;; ├── invalid-notes                 INDEX: 0-based position in the sequence given
;;;; └── transcriber-error             TRANSCRIBER: the backend that failed
;;;;
;;;; Restarts are the CLHS ones, established where the recovery happens:
;;;;   UNUSABLE-AUDIO  PREPARE-AUDIO (and so TRANSCRIBE): CONTINUE (waive this
;;;;                   check and go on) and USE-VALUE (another PCM-BUFFER,
;;;;                   checked again from the start). protocol.lisp.
;;;;   INVALID-HINT    NORMALIZE-HINTS (and so TRANSCRIBE, FINISH-TRANSCRIPTION):
;;;;                   USE-VALUE (another value for that hint, checked again;
;;;;                   not offered for an unknown key) and CONTINUE (drop the
;;;;                   hint). hints.lisp.
;;;;   INVALID-NOTES and TRANSCRIBER-ERROR are bugs in the caller or the
;;;;   backend; no restart.
;;;; Argument type checks are CHECK-TYPE, so a TYPE-ERROR offers STORE-VALUE.
;;;; music-theory's NO-KEY-EVIDENCE never reaches the caller: FINISH-TRANSCRIPTION
;;;; answers it with USE-VALUE NIL (transcription.lisp).

(in-package #:transcribe-protocol)

(define-condition transcribe-error (error)
  ((reason :initarg :reason :initform nil :reader transcribe-error-reason
           :documentation "A short lower-case explanation, or NIL."))
  (:documentation "Root of the errors transcribe-protocol and its backends signal.")
  (:report (lambda (c s)
             (format s "Transcription error~@[: ~A~]" (transcribe-error-reason c)))))

(define-condition unusable-audio (transcribe-error)
  ((duration :initarg :duration :initform nil :reader unusable-audio-duration
             :documentation "The audio's duration in seconds, an exact rational."))
  (:documentation "PREPARE-AUDIO can't use the audio as it is: too short or silent.
Restarts CONTINUE (use it anyway) and USE-VALUE (another buffer).")
  (:report (lambda (c s)
             (format s "Unusable audio~@[ (~A s)~]~@[: ~A~]"
                     (let ((d (unusable-audio-duration c))) (and d (float d 1d0)))
                     (transcribe-error-reason c)))))

(define-condition audio-too-short (unusable-audio)
  ((minimum :initarg :minimum :initform nil :reader audio-too-short-minimum
            :documentation "The shortest duration accepted, in seconds (*MINIMUM-DURATION*)."))
  (:documentation "The audio is shorter than *MINIMUM-DURATION*.")
  (:report (lambda (c s)
             (format s "Audio too short to transcribe: ~A s, below ~A s"
                     (let ((d (unusable-audio-duration c))) (and d (float d 1d0)))
                     (let ((m (audio-too-short-minimum c))) (and m (float m 1d0)))))))

(define-condition silent-audio (unusable-audio)
  ((peak :initarg :peak :initform nil :reader silent-audio-peak
         :documentation "The peak magnitude of the downmixed audio, a DOUBLE-FLOAT."))
  (:documentation "The audio's peak is below *SILENCE-PEAK-DB* dBFS.")
  (:report (lambda (c s)
             (format s "Silent audio: peak ~A~@[ (~A)~]"
                     (silent-audio-peak c) (transcribe-error-reason c)))))

(define-condition invalid-hint (transcribe-error)
  ((key :initarg :key :initform nil :reader invalid-hint-key
        :documentation "The hint's keyword.")
   (value :initarg :value :initform nil :reader invalid-hint-value
          :documentation "The value given for it.")
   (cause :initarg :cause :initform nil :reader invalid-hint-cause
          :documentation "The music-theory condition PARSE-KEY or PARSE-METER signalled, or NIL."))
  (:documentation "A hint NORMALIZE-HINTS can't accept: an unknown key, or a value
of the wrong type. Restarts USE-VALUE (known keys) and CONTINUE (drop it).")
  (:report (lambda (c s)
             (format s "Invalid hint ~S ~S~@[: ~A~]"
                     (invalid-hint-key c) (invalid-hint-value c)
                     (transcribe-error-reason c)))))

(define-condition invalid-notes (transcribe-error)
  ((index :initarg :index :initform nil :reader invalid-notes-index
          :documentation "0-based position of the offending element in the sequence given."))
  (:documentation "A sequence of notes the protocol can't accept: an element that
isn't a NOTE-EVENT, or (where one line is required) notes out of order or
overlapping.")
  (:report (lambda (c s)
             (format s "Invalid notes~@[ at index ~D~]~@[: ~A~]"
                     (invalid-notes-index c) (transcribe-error-reason c)))))

(define-condition transcriber-error (transcribe-error)
  ((transcriber :initarg :transcriber :initform nil :reader transcriber-error-transcriber
                :documentation "The TRANSCRIBER."))
  (:documentation "A backend failed, has no DETECT-NOTES method, or returned
something the protocol doesn't allow.")
  (:report (lambda (c s)
             (format s "Transcriber ~S failed~@[: ~A~]"
                     (transcriber-error-transcriber c) (transcribe-error-reason c)))))

(defun %t0-pending (name)
  "Contract stub for slice T0. Every call is replaced by the implementation,
then this function is deleted."
  (error "transcribe-protocol T0 pending: ~A" name))
