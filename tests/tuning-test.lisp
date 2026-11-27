;;;; Tuning and drift: ESTIMATE-TUNING, SEGMENT-PHRASES, ESTIMATE-DRIFT,
;;;; REMOVE-DRIFT. Costs are hand-computed in the comments: L(m) = sum of
;;;; duration * |residual in cents|.

(in-package #:transcribe-protocol/tests)

(defun take (&rest cents-and-durations)
  "Legato notes on C4, D4, E4, ... each CENTS off, of the given durations:
arguments alternate cents and duration."
  (let ((time 0) (out '()) (k 0))
    (loop for (cents duration) on cents-and-durations by #'cddr
          do (push (note time (+ time duration) (+ 60 (* 2 k) (/ cents 100))) out)
             (incf time duration)
             (incf k))
    (coerce (nreverse out) 'simple-vector)))

(deftest-parametrize estimate-tuning-cases
    ((notes expected)
     ;; in tune, and whole semitones away: 0
     ((take 0 1 0 1 0 1) 0)
     ((notes '(0 1 61) '(1 2 63) '(2 3 66)) 0)
     ;; every pitch shifted by c cents: c
     ((take 23 1 23 1/2 23 2) 23)
     ((take -37 1 -37 1) -37)
     ((take 12 1) 12)
     ((take 49 1 49 1) 49)
     ((take -50 1 -50 1) -50)
     ;; +50 is not a candidate: 50 sharp of C is 50 flat of C#
     ((take 50 1 50 1) -50)
     ;; floats: the exact rationals of 60.2d0 and 64.2d0 are within a hair of 20 cents
     ((notes '(0 1 60.2d0) '(1 2 64.2d0)) 20)
     ;; weights: L(m) = 3 |10 - m| + |30 - m|, least at 10 (20); at 11 it is 22
     ((take 10 3 30 1) 10)
     ;; a weighted median, not a mean: four notes at +20 outvote one at -30
     ((take 20 1 20 1 -30 1 20 1 20 1) 20)
     ;; ties: L = 20 on -10 .. 10, the smallest correction wins
     ((take 10 1 -10 1) 0)
     ;; L = 25 on 25 .. 49 and -50 (+50 wraps): the smallest |m|. (Ties need
     ;; pitches a double holds exactly: quarters and eighths of a semitone.)
     ((take 25 1 50 1) 25)
     ;; around the half semitone: +37.5 and -37.5 are 25 apart through 50.
     ;; L = 25 on 38 .. 49 and -50 .. -38; |38| = |-38|, the flatter first
     ((take 75/2 1 -75/2 1) -38)
     ;; L = 6.25 on 47 .. 49, -50 .. -47: -47 comes before 47
     ((take 375/8 1 -375/8 1) -47)
     ;; no notes: every cost is 0
     (#() 0))
  (ok (eql expected (estimate-tuning notes))))

(deftest estimate-tuning-arguments
  (testing "any order, overlapping or not"
    (ok (eql 23 (estimate-tuning (list (note 2 3 64.23d0) (note 0 5/2 60.23d0))))))
  (ok (signals (estimate-tuning (list (note 0 1 60) 7)) 'invalid-notes))
  (ok (signals (estimate-tuning 7) 'type-error)))

(defun phrased (counts &key (gap 1/2) (note-length 1/4))
  "Phrases of COUNTS legato notes NOTE-LENGTH long, GAP seconds of rest between."
  (let ((time 0) (out '()) (k 0))
    (dolist (count counts)
      (dotimes (i count)
        (push (note time (+ time note-length) (+ 60 (mod k 7))) out)
        (incf time note-length)
        (incf k))
      (incf time gap))
    (coerce (nreverse out) 'simple-vector)))

(deftest-parametrize segment-phrases-cases
    ((notes keys expected)
     ;; a rest of exactly 1/4 s splits
     ((phrased '(3 3) :gap 1/4) nil '((0 . 3) (3 . 6)))
     ;; a shorter rest does not
     ((phrased '(3 3) :gap 6/25) nil '((0 . 6)))
     ((phrased '(4 3 5)) nil '((0 . 4) (4 . 7) (7 . 12)))
     ;; merging: [1 5 2 4 1] -> the first joins the next, the 2 and the last 1
     ;; join the phrase before them
     ((phrased '(1 5 2 4 1)) nil '((0 . 8) (8 . 13)))
     ;; leading short phrases collect until they reach three notes
     ((phrased '(1 1 1 5)) nil '((0 . 3) (3 . 8)))
     ((phrased '(2 2)) nil '((0 . 4)))
     ;; a take shorter than MIN-NOTES is one phrase
     ((phrased '(1 1)) nil '((0 . 2)))
     ;; MIN-NOTES 1 keeps every split
     ((phrased '(1 5 2 4 1)) '(:min-notes 1) '((0 . 1) (1 . 6) (6 . 8) (8 . 12) (12 . 13)))
     ((phrased '(2 2)) '(:min-notes 2) '((0 . 2) (2 . 4)))
     ;; GAP 0: every note starts a phrase
     ((phrased '(4)) '(:gap 0 :min-notes 1) '((0 . 1) (1 . 2) (2 . 3) (3 . 4)))
     ((phrased '(3 3) :gap 1) '(:gap 2) '((0 . 6)))
     (#() nil nil))
  (ok (equal expected (apply #'segment-phrases notes keys))))

(deftest segment-phrases-arguments
  (ok (signals (segment-phrases (list (note 1 2 60) (note 0 1 60))) 'invalid-notes))
  (ok (signals (segment-phrases (phrased '(3)) :gap -1) 'type-error))
  (ok (signals (segment-phrases (phrased '(3)) :min-notes 0) 'type-error)))

(defun two-phrases (cents-a cents-b)
  "Three notes CENTS-A off, a rest of 1/2 s, three notes CENTS-B off; 1 s each."
  (concatenate 'simple-vector
               (map 'vector (lambda (k) (note k (1+ k) (+ 60 k (/ cents-a 100)))) '(0 1 2))
               (map 'vector (lambda (k) (note (+ 7/2 k) (+ 9/2 k) (+ 62 k (/ cents-b 100))))
                    '(0 1 2))))

(deftest estimate-drift-phrases
  (let* ((ns (two-phrases 10 -20))
         (drift (estimate-drift ns)))
    ;; take: L = 3 |10 - m| + 3 |-20 - m| = 90 on -20 .. 10, so g = 0
    (ok (eql 0 (estimate-tuning ns)))
    (ok (typep drift 'simple-vector))
    (ok (= 2 (length drift)))
    (ok (equalp (vector (make-phrase-drift :start 0 :end 3 :first 0 :count 3 :cents 10)
                        (make-phrase-drift :start 7/2 :end 13/2 :first 3 :count 3 :cents -20))
                drift)))
  (testing "relative to a given tuning"
    (ok (equal '(5 -25) (map 'list #'phrase-drift-cents
                             (estimate-drift (two-phrases 10 -20) :tuning 5)))))
  (testing "the default tuning is ESTIMATE-TUNING's"
    (let ((ns (two-phrases 30 22)))
      (ok (eql 22 (estimate-tuning ns)))
      (ok (equalp (estimate-drift ns :tuning 22) (estimate-drift ns)))
      (ok (equal '(8 0) (map 'list #'phrase-drift-cents (estimate-drift ns))))))
  (testing "phrase keywords"
    (ok (= 1 (length (estimate-drift (two-phrases 10 -20) :gap 1))))
    (ok (= 1 (length (estimate-drift (two-phrases 10 -20) :min-notes 4)))))
  (ok (equalp #() (estimate-drift '()))))

(deftest estimate-drift-window
  ;; The window is centred on the take's tuning: a phrase 70 cents flat in a
  ;; take at -40 reads -30 relative to it (offset -70), not +30.
  (let ((drift (estimate-drift (two-phrases -40 -70) :tuning -40)))
    (ok (equal '(0 -30) (map 'list #'phrase-drift-cents drift))))
  ;; ... and a phrase 95 cents flat reads +5 (a semitone down, 5 sharp).
  (let ((drift (estimate-drift (two-phrases -40 -95) :tuning -40)))
    (ok (equal '(0 45) (map 'list #'phrase-drift-cents drift))))
  ;; ties inside a phrase: L = 20 on -10 .. 10 around the tuning
  (let ((ns (vector (note 0 1 60.1d0) (note 1 2 61.9d0))))
    (ok (equal '(0) (map 'list #'phrase-drift-cents (estimate-drift ns :tuning 0 :min-notes 1))))))

(deftest estimate-drift-arguments
  (ok (signals (estimate-drift (list (note 0 2 60) (note 1 3 62))) 'invalid-notes))
  (ok (signals (estimate-drift (two-phrases 0 0) :tuning 1/2) 'type-error)))

(deftest remove-drift-pitches
  (let* ((ns (two-phrases 10 -20))
         (drift (estimate-drift ns))
         (fixed (remove-drift ns drift)))
    (ok (typep fixed 'simple-vector))
    (ok (= 6 (length fixed)))
    (ok (equal '(60d0 61d0 62d0 62d0 63d0 64d0) (pitches fixed)))
    (ok (every (lambda (a b)
                 (and (eql (note-event-onset a) (note-event-onset b))
                      (eql (note-event-offset a) (note-event-offset b))
                      (eql (note-event-confidence a) (note-event-confidence b))))
               ns fixed)))
  (testing "exact subtraction, rounded once"
    (let* ((ns (vector (note 0 1 60.37d0 0.5d0)))
           (fixed (remove-drift ns (vector (make-phrase-drift :start 0 :end 1 :count 1 :cents 7))
                                :tuning 30)))
      (ok (eql (float (- (rational 60.37d0) 37/100) 1d0) (note-event-pitch (svref fixed 0))))
      (ok (eql 0.5d0 (note-event-confidence (svref fixed 0))))))
  (testing "TUNING: with the take's own tuning the notes come back at A440"
    (let* ((ns (two-phrases 35 25))
           (g (estimate-tuning ns))
           (drift (estimate-drift ns :tuning g)))
      (ok (eql 25 g))
      (ok (equal '(10 0) (map 'list #'phrase-drift-cents drift)))
      (ok (equal '(60d0 61d0 62d0 62d0 63d0 64d0) (pitches (remove-drift ns drift :tuning g))))
      (ok (every (lambda (p) (close-to (cents-from-semitone p) 25 1d-9))
                 (pitches (remove-drift ns drift)))
          "only the drift: the take keeps its 25 cents")))
  (testing "notes in no phrase keep their pitch; a later phrase wins"
    (let* ((ns (notes '(0 1 60.2d0) '(1 2 62.2d0) '(2 3 64.2d0)))
           (fixed (remove-drift ns (list (make-phrase-drift :first 0 :count 2 :cents 20)
                                         (make-phrase-drift :first 1 :count 1 :cents 10)))))
      (ok (equal (list (float (- (rational 60.2d0) 1/5) 1d0)
                       (float (- (rational 62.2d0) 1/10) 1d0)
                       64.2d0)
                 (pitches fixed)))))
  (ok (equalp #() (remove-drift #() #()))))

(deftest remove-drift-arguments
  (let ((ns (two-phrases 0 0)))
    (ok (signals (remove-drift ns (vector (make-phrase-drift :first 4 :count 3))) 'transcribe-error))
    (ok (signals (remove-drift ns (vector 7)) 'type-error))
    (ok (signals (remove-drift ns 7) 'type-error))
    (ok (signals (remove-drift ns #() :tuning 0.5) 'type-error))
    (ok (signals (remove-drift (list (note 1 2 60) (note 0 1 60)) #()) 'invalid-notes))))

(deftest-parametrize drifting-takes-come-back-in-tune
    ((offset drift-rate seed)
     (15 -8 7)
     (-15 -6 11)
     (25 5 3)
     (0 -8 19)
     (-45 4 23))
  ;; *MELODY-C*: 8 s, three phrases. Sung OFFSET cents off, drifting
  ;; DRIFT-RATE cents a second (up to 64 cents over the take), each note
  ;; detuned by up to 5 cents.
  (let* ((truth (melody *melody-c*))
         (sung (detune-notes truth :offset offset :drift-rate drift-rate :jitter 5 :seed seed))
         (g (estimate-tuning sung))
         (fixed (remove-drift sung (estimate-drift sung :tuning g) :tuning g)))
    (ok (some (lambda (a b) (> (* 100 (abs (- (note-event-pitch a) (note-event-pitch b)))) 20))
              truth sung)
        "as sung, some note is more than 20 cents off")
    (ok (every (lambda (a b) (<= (* 100 (abs (- (note-event-pitch a) (note-event-pitch b)))) 20))
               truth fixed)
        "every note within 20 cents of its written pitch")))
