;;;; Helpers and test melodies shared by the suite.

(in-package #:transcribe-protocol/tests)

(defun note (onset offset pitch &optional (confidence 1))
  (make-note-event :onset onset :offset offset :pitch pitch :confidence confidence))

(defun notes (&rest specs)
  "A SIMPLE-VECTOR of NOTE-EVENTs from (onset offset pitch [confidence]) lists."
  (map 'simple-vector (lambda (spec) (apply #'note spec)) specs))

(defun close-to (a b tolerance)
  (<= (abs (- a b)) tolerance))

(defun pitches (notes)
  (map 'list #'note-event-pitch notes))

(defun cents-from-semitone (pitch)
  "How far PITCH lies from its nearest MIDI number, in cents (0 .. 50)."
  (* 100 (abs (- pitch (round pitch)))))

(defun buffer (samples &key (rate 16000) (channels 1))
  (audio:make-pcm-buffer :sample-rate rate :channels channels :samples samples))

(defun sine-samples (frequency seconds &key (rate 16000) (amplitude 0.5d0))
  (let ((n (round (* seconds rate))))
    (let ((v (make-array n :element-type 'double-float)))
      (dotimes (i n v)
        (setf (aref v i) (* amplitude (sin (/ (* 2 (float pi 1d0) frequency i) rate))))))))

(defun sine-buffer (frequency seconds &key (rate 16000) (amplitude 0.5d0))
  (buffer (sine-samples frequency seconds :rate rate :amplitude amplitude) :rate rate))

(defun silence-buffer (seconds &key (rate 16000) (channels 1))
  (audio:make-pcm-buffer :sample-rate rate :channels channels
                         :frames (round (* seconds rate))))

(defun caught (thunk type)
  "The condition of TYPE that THUNK signals with ERROR, unwound; NIL if none."
  (handler-case (progn (funcall thunk) nil)
    (error (c) (and (typep c type) c))))

(defun crossing-times (samples start end rate)
  "Times (seconds) of the rising zero crossings of SAMPLES between indices
START and END, linearly interpolated."
  (loop for i from (max 1 start) below (min end (length samples))
        for a = (float (aref samples (1- i)) 1d0)
        for b = (float (aref samples i) 1d0)
        when (and (< a 0d0) (>= b 0d0))
          collect (/ (+ (1- i) (/ (- a) (- b a))) rate)))

(defun crossing-frequency (samples start end rate)
  "The mean frequency in Hz of SAMPLES between START and END by rising zero crossings."
  (let ((times (crossing-times samples start end rate)))
    (/ (1- (length times)) (- (car (last times)) (first times)))))

(defun crossing-frequencies (samples start end rate)
  "The frequency of each period between consecutive rising zero crossings."
  (let ((times (crossing-times samples start end rate)))
    (loop for (a b) on times while b collect (/ 1 (- b a)))))

;;; Melodies --------------------------------------------------------------------
;;; Pitch lists written for these tests (no source tune): steps, leaps and two
;;; breaths, no note repeated straight after itself (a plain tracker can't split
;;; those without a rest).

(defparameter *melody-c*
  '((60 2/5) (62 2/5) (64 2/5) (65 2/5) (67 4/5) (nil 3/10)
    (69 2/5) (67 2/5) (65 2/5) (64 2/5) (62 4/5) (nil 3/10)
    (64 2/5) (60 2/5) (62 2/5) (59 2/5) (60 1))
  "Fifteen notes in C major, three phrases, 8 seconds.")

(defparameter *melody-d*
  '((62 2/5) (64 2/5) (66 2/5) (67 2/5) (69 2/5) (71 2/5) (73 2/5) (74 4/5) (nil 3/10)
    (74 2/5) (69 2/5) (66 2/5) (62 2/5) (69 2/5) (74 2/5) (73 2/5) (74 1))
  "Sixteen notes in D major: the scale up, then the tonic triad.")

(defun melody (spec &rest keys)
  (apply #'melody-notes spec keys))
