;;;; T0 contour: the f0 track a backend computed, frame by frame.
;;;;
;;;; An F0-CONTOUR is immutable and compared with EQUALP:
;;;;   SAMPLE-RATE  the rate of the audio the backend analysed (after
;;;;                PREPARE-AUDIO), an integer >= 1
;;;;   HOP          samples between frames, an integer >= 1
;;;;   F0           a SIMPLE-VECTOR, one entry per frame: the fundamental
;;;;                frequency in Hz (a positive DOUBLE-FLOAT), or NIL where the
;;;;                frame is unvoiced
;;;;   VOICING      a (SIMPLE-ARRAY DOUBLE-FLOAT (*)) of the same length: each
;;;;                frame's voicing probability, 0..1
;;;; Frame i sits at time (CONTOUR-FRAME-TIME contour i) = i * HOP / SAMPLE-RATE
;;;; seconds, an exact rational: frames are centred on their sample, audio-
;;;; protocol's :CENTER convention (FRAME-SIGNAL). The frame rate is not fixed
;;;; by the protocol; each contour reports it: (CONTOUR-FRAME-RATE contour) =
;;;; SAMPLE-RATE / HOP frames per second, exact (16000 / 256 = 125/2).
;;;; (CONTOUR-LENGTH contour) is the number of frames.
;;;;
;;;; (MAKE-F0-CONTOUR &key (sample-rate 16000) (hop 256) f0 voicing) checks with
;;;; CHECK-TYPE (TYPE-ERROR, STORE-VALUE), in order: SAMPLE-RATE and HOP
;;;; integers >= 1; F0 a list or vector (default empty) whose elements are NIL
;;;; or positive REALs; VOICING NIL or a list or vector of REALs 0..1. Then a
;;;; VOICING of another length than F0 is a TRANSCRIBE-ERROR. F0 values are
;;;; stored as DOUBLE-FLOATs, VOICING as a fresh double vector; without VOICING
;;;; each frame gets 1d0 where its f0 is a number and 0d0 where it is NIL.
;;;;
;;;; (NOTES-CONTOUR notes &key (sample-rate 16000) (hop 256) frames) -> the
;;;;   F0-CONTOUR a perfect tracker would report for NOTES, one line (checked
;;;;   first, with CHECK-NOTES). SAMPLE-RATE and HOP are integers >= 1, FRAMES
;;;;   NIL or an integer >= 0 (CHECK-TYPE, in that order). With t_i = i * HOP / SAMPLE-RATE, frame i
;;;;   is voiced when some note has ONSET <= t_i < OFFSET: its f0 is
;;;;   (MIDI-TO-HZ pitch) of that note and its voicing the note's CONFIDENCE.
;;;;   Every other frame has f0 NIL and voicing 0d0. FRAMES defaults to the
;;;;   number of frame times before the last offset, ceiling(last offset *
;;;;   SAMPLE-RATE / HOP), and to 0 without notes. ORACLE-TRANSCRIBER reports
;;;;   this contour.

(in-package #:transcribe-protocol)

(deftype %double-vector () '(simple-array double-float (*)))

(defstruct (f0-contour (:constructor %make-f0-contour (sample-rate hop f0 voicing))
                       (:copier nil))
  "Frame-wise f0 (Hz or NIL) and voicing probability; frame i at i * HOP /
SAMPLE-RATE seconds. Make one with MAKE-F0-CONTOUR."
  (sample-rate 16000 :type (integer 1) :read-only t)
  (hop 256 :type (integer 1) :read-only t)
  (f0 #() :type simple-vector :read-only t)
  (voicing (make-array 0 :element-type 'double-float) :type %double-vector :read-only t))

(defun %f0-values-p (x)
  (and (or (vectorp x) (%proper-list-p x))
       (every (lambda (v) (or (null v) (typep v '(real (0))))) x)))

(defun %voicing-values-p (x)
  (and (or (vectorp x) (%proper-list-p x))
       (every (lambda (v) (typep v '(real 0 1))) x)))

(defun make-f0-contour (&key (sample-rate 16000) (hop 256) (f0 #()) voicing)
  "An F0-CONTOUR. F0: NIL (unvoiced) or Hz per frame; VOICING: 0..1 per frame,
by default 1 where F0 is a number and 0 where it is NIL. See contour.lisp."
  (check-type sample-rate (integer 1))
  (check-type hop (integer 1))
  (check-type f0 (satisfies %f0-values-p) "a list or vector of NIL or positive reals")
  (check-type voicing (or null (satisfies %voicing-values-p))
              "NIL or a list or vector of reals 0..1")
  (let* ((f (map 'simple-vector (lambda (v) (and v (float v 1d0))) f0))
         (n (length f)))
    (when (and voicing (/= (length voicing) n))
      (error 'transcribe-error
             :reason (format nil "~D f0 value~:P but ~D voicing value~:P"
                             n (length voicing))))
    (let ((v (make-array n :element-type 'double-float :initial-element 0d0)))
      (if voicing
          (map-into v (lambda (x) (float x 1d0)) voicing)
          (dotimes (i n) (when (svref f i) (setf (aref v i) 1d0))))
      (%make-f0-contour sample-rate hop f v))))

(defun contour-length (contour)
  "The number of frames of CONTOUR."
  (check-type contour f0-contour)
  (length (f0-contour-f0 contour)))

(defun contour-frame-time (contour index)
  "INDEX * HOP / SAMPLE-RATE: the time of frame INDEX in seconds, exact."
  (check-type contour f0-contour)
  (check-type index (integer 0))
  (/ (* index (f0-contour-hop contour)) (f0-contour-sample-rate contour)))

(defun contour-frame-rate (contour)
  "SAMPLE-RATE / HOP: frames per second of CONTOUR, exact."
  (check-type contour f0-contour)
  (/ (f0-contour-sample-rate contour) (f0-contour-hop contour)))

(defun notes-contour (notes &key (sample-rate 16000) (hop 256) frames)
  "The F0-CONTOUR of NOTES: f0 and voicing of the note sounding at each frame
time, NIL and 0 elsewhere. See contour.lisp."
  (let ((v (check-notes notes)))
    (check-type sample-rate (integer 1))
    (check-type hop (integer 1))
    (check-type frames (or null (integer 0)))
    (let* ((n (length v))
           (count (or frames
                      (if (zerop n)
                          0
                          (ceiling (* (note-event-offset (svref v (1- n))) sample-rate) hop))))
           (f0 (make-array count :initial-element nil))
           (voicing (make-array count :element-type 'double-float :initial-element 0d0))
           (k 0))
      ;; One line in time order, so one pointer walks the notes as the frame
      ;; times rise: skip the notes that ended at or before t_i.
      (dotimes (i count)
        (let ((time (/ (* i hop) sample-rate)))
          (loop while (and (< k n) (<= (note-event-offset (svref v k)) time))
                do (incf k))
          (when (and (< k n) (<= (note-event-onset (svref v k)) time))
            (let ((note (svref v k)))
              (setf (svref f0 i) (midi-to-hz (note-event-pitch note))
                    (aref voicing i) (note-event-confidence note))))))
      (make-f0-contour :sample-rate sample-rate :hop hop :f0 f0 :voicing voicing))))
