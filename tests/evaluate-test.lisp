;;;; Evaluation: MATCH-NOTES and NOTE-F-MEASURE against hand-computed cases.

(in-package #:transcribe-protocol/tests)

(defun scores (reference estimate &rest keys)
  (multiple-value-list (apply #'note-f-measure reference estimate keys)))

(deftest-parametrize f-measure-cases
    ((reference estimate keys expected)
     ;; identical: (f precision recall matched)
     ((notes '(0 1/2 60) '(1/2 1 62) '(1 2 64)) (notes '(0 1/2 60) '(1/2 1 62) '(1 2 64))
      nil '(1 1 1 3))
     ;; mir_eval: no notes on either side gives zeros
     (#() (notes '(0 1 60)) nil '(0 0 0 0))
     ((notes '(0 1 60)) #() nil '(0 0 0 0))
     (#() #() nil '(0 0 0 0))
     ;; onsets: exactly 50 ms matches with <=, not with STRICT; 51 ms never
     ((notes '(0 1 60)) (notes '(1/20 1 60)) nil '(1 1 1 1))
     ((notes '(0 1 60)) (notes '(1/20 1 60)) '(:strict t) '(0 0 0 0))
     ((notes '(1/20 1 60)) (notes '(0 1 60)) nil '(1 1 1 1))
     ((notes '(0 1 60)) (notes '(51/1000 1 60)) nil '(0 0 0 0))
     ;; pitch: exactly 50 cents matches, 51 cents doesn't
     ((notes '(0 1 60)) (notes '(0 1 60.5d0)) nil '(1 1 1 1))
     ((notes '(0 1 60)) (notes '(0 1 59.5d0)) nil '(1 1 1 1))
     ((notes '(0 1 60)) (notes '(0 1 60.5d0)) '(:strict t) '(0 0 0 0))
     ((notes '(0 1 60)) (notes '(0 1 6051/100)) nil '(0 0 0 0))
     ;; other tolerances
     ((notes '(0 1 60)) (notes '(1/10 1 61)) '(:onset-tolerance 1/10 :pitch-tolerance-cents 100)
      '(1 1 1 1))
     ;; precision 1, recall 2/3: f = 2 (2/3) / (5/3) = 4/5
     ((notes '(0 1/2 60) '(1/2 1 62) '(1 2 64)) (notes '(0 1/2 60) '(1 2 64)) nil
      '(4/5 1 2/3 2))
     ;; precision 1/2, recall 1: f = 2/3
     ((notes '(0 1/2 60) '(1 2 64)) (notes '(0 1/4 60) '(1/4 1/2 67) '(1 3/2 64) '(3/2 2 66))
      nil '(2/3 1/2 1 2))
     ;; one estimate can't match two reference notes
     ((list (note 0 1 60) (note 0 1 60)) (notes '(0 1 60)) nil '(2/3 1 1/2 1))
     ;; a chord in the reference: notes may overlap on either side
     ((list (note 0 1 60) (note 0 1 64)) (list (note 1/100 1 63.8d0) (note 0 1 60.2d0)) nil
      '(1 1 1 2))
     ;; offsets ignored by default
     ((notes '(0 1 60)) (notes '(0 3 60)) nil '(1 1 1 1))
     ;; offsets: within max(1/5 * 1 s, 50 ms) = 200 ms of a 1 s note
     ((notes '(0 1 60)) (notes '(0 6/5 60)) '(:offset-ratio 1/5) '(1 1 1 1))
     ((notes '(0 1 60)) (notes '(0 1201/1000 60)) '(:offset-ratio 1/5) '(0 0 0 0))
     ((notes '(0 1 60)) (notes '(0 4/5 60)) '(:offset-ratio 1/5) '(1 1 1 1))
     ;; ... and within 50 ms of a 100 ms note (1/5 of it is only 20 ms)
     ((notes '(0 1/10 60)) (notes '(0 3/20 60)) '(:offset-ratio 1/5) '(1 1 1 1))
     ((notes '(0 1/10 60)) (notes '(0 4/25 60)) '(:offset-ratio 1/5) '(0 0 0 0))
     ((notes '(0 1/10 60)) (notes '(0 4/25 60)) '(:offset-ratio 1/5 :offset-min-tolerance 1/10)
      '(1 1 1 1))
     ((notes '(0 1 60)) (notes '(0 6/5 60)) '(:offset-ratio 1/5 :strict t) '(0 0 0 0)))
  (ok (equal expected (apply #'scores reference estimate keys))))

(deftest f-measure-maximum-matching
  ;; Reference A at 0 and B at 30 ms; estimate X at 25 ms (5 ms from B, its
  ;; nearest) and Y at 70 ms (only within 50 ms of B). Greedy nearest-first
  ;; pairs X with B and leaves Y alone: 1 match. The maximum is 2: A-X, B-Y.
  (let ((reference (notes '(0 1/2 60) '(3/100 1/2 60)))
        (estimate (notes '(1/40 1/2 60) '(7/100 1/2 60))))
    (ok (equal '(1 1 1 2) (scores reference estimate)))
    (ok (equal '((0 . 0) (1 . 1)) (match-notes reference estimate)))
    (ok (equal '((0 . 1) (1 . 0)) (match-notes reference (reverse estimate))))
    (ok (equal '((0 . 1) (1 . 0)) (match-notes (reverse reference) estimate))))
  ;; A tie: X is 20 ms from both A and B; Y only within reach of B.
  (let ((reference (notes '(0 1/2 60) '(1/25 1/2 60)))
        (estimate (notes '(1/50 1/2 60) '(2/25 1/2 60))))
    (ok (equal '(1 1 1 2) (scores reference estimate)))
    (ok (equal '((0 . 0) (1 . 1)) (match-notes reference estimate)))))

(defun random-notes (random count)
  "COUNT notes with onsets on a 10 ms grid in 0 .. 200 ms and pitches 60, 60.3, 60.6."
  (loop repeat count
        collect (let ((onset (/ (floor (* 21 (funcall random))) 100)))
                  (note onset (+ onset 1) (+ 60 (* 3/10 (floor (* 3 (funcall random)))))))))

(defun brute-force-maximum (reference estimate)
  "The size of a maximum matching under the default criteria, by search."
  (let ((candidates
          (map 'vector
               (lambda (r)
                 (loop for e in estimate
                       for j from 0
                       when (and (<= (abs (- (note-event-onset r) (note-event-onset e))) 1/20)
                                 (<= (* 100 (abs (- (rational (note-event-pitch r))
                                                    (rational (note-event-pitch e)))))
                                     50))
                         collect j))
               reference)))
    (labels ((best (i used)
               (if (= i (length candidates))
                   0
                   (max (best (1+ i) used)
                        (or (loop for j in (aref candidates i)
                                  unless (member j used)
                                    maximize (1+ (best (1+ i) (cons j used))))
                            0)))))
      (best 0 '()))))

(deftest match-notes-random-instances
  (let ((random (make-park-miller 4242)))
    (dotimes (trial 25)
      (let* ((reference (random-notes random (1+ (floor (* 6 (funcall random))))))
             (estimate (random-notes random (1+ (floor (* 6 (funcall random))))))
             (matching (match-notes reference estimate)))
        (ok (equal matching (sort (copy-list matching) #'< :key #'car)) "sorted by reference index")
        (ok (and (= (length matching) (length (remove-duplicates (mapcar #'car matching))))
                 (= (length matching) (length (remove-duplicates (mapcar #'cdr matching)))))
            "each note matched at most once")
        (ok (every (lambda (pair)
                     (let ((r (nth (car pair) reference)) (e (nth (cdr pair) estimate)))
                       (and (<= (abs (- (note-event-onset r) (note-event-onset e))) 1/20)
                            (<= (* 100 (abs (- (rational (note-event-pitch r))
                                               (rational (note-event-pitch e)))))
                                50))))
                   matching)
            "every pair meets the criteria")
        (ok (= (brute-force-maximum reference estimate) (length matching)) "maximum")
        (ok (= (length matching) (fourth (scores reference estimate))))))))

(deftest f-measure-arguments
  (ok (signals (note-f-measure 5 #()) 'type-error))
  (ok (signals (note-f-measure #() 5) 'type-error))
  (ok (signals (note-f-measure (list (note 0 1 60) 60) #()) 'invalid-notes))
  (ok (signals (note-f-measure #() #() :onset-tolerance -1/20) 'type-error))
  (ok (signals (note-f-measure #() #() :pitch-tolerance-cents "50") 'type-error))
  (ok (signals (note-f-measure #() #() :offset-ratio -1) 'type-error))
  (ok (signals (note-f-measure #() #() :offset-min-tolerance nil) 'type-error))
  (ok (signals (match-notes (list 1) #()) 'invalid-notes))
  (ok (null (match-notes #() (notes '(0 1 60))))))
