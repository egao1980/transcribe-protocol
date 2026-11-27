;;;; Hints: NORMALIZE-HINTS, its condition and restarts, HINT-PITCH-RANGE.

(in-package #:transcribe-protocol/tests)

(deftest-parametrize normalize-hints-values
    ((hints expected)
     (nil nil)
     ('(:key "G") (list :key (mt:parse-key "G")))
     ('(:key "f#") (list :key (mt:parse-key "F# minor")))
     ('(:key "Eb major") (list :key (mt:make-key :tonic (mt:parse-pitch-class "Eb") :mode :major)))
     ('(:meter "3/4") (list :meter (mt:parse-meter "3/4")))
     ('(:meter "6/8") (list :meter (mt:make-meter :numerators '(6) :denominator 8)))
     ('(:tempo 90) (list :tempo (mt:make-tempo :bpm 90)))
     ('(:tempo 72.5d0) (list :tempo (mt:make-tempo :bpm 72.5d0)))
     ('(:pitch-range (48 72.5)) '(:pitch-range (48 72.5)))
     ('(:pitch-range (0 127)) '(:pitch-range (0 127))))
  (ok (equalp expected (normalize-hints hints))))

(deftest normalize-hints-order-and-identity
  (let* ((key (mt:parse-key "D"))
         (meter (mt:parse-meter "2/4"))
         (tempo (mt:make-tempo :bpm 60 :beat-unit 1/8))
         (range (list 50 70))
         (out (normalize-hints (list :pitch-range range :tempo tempo :meter meter :key key))))
    (ok (equal '(:key :meter :tempo :pitch-range)
               (loop for (k) on out by #'cddr collect k)))
    (ok (eq key (getf out :key)))
    (ok (eq meter (getf out :meter)))
    (ok (eq tempo (getf out :tempo)))
    (ok (equal range (getf out :pitch-range)))
    (ok (not (eq range (getf out :pitch-range))) "a fresh range")
    (ok (equal out (normalize-hints out)) "idempotent"))
  (testing "a key given twice: the first counts, the second isn't checked"
    (ok (equalp (list :tempo (mt:make-tempo :bpm 100))
                (normalize-hints '(:tempo 100 :tempo "fast"))))))

(deftest-parametrize normalize-hints-invalid
    ((hints key reason)
     ('(:key 5) :key "not a key")
     ('(:key "H") :key "not a key")
     ('(:key "G#") :key "not a key")
     ('(:meter 3/4) :meter "not a meter")
     ('(:meter "3/") :meter "not a meter")
     ('(:tempo 0) :tempo "not a tempo or a positive bpm")
     ('(:tempo -60) :tempo "not a tempo or a positive bpm")
     ('(:tempo "fast") :tempo "not a tempo or a positive bpm")
     ('(:pitch-range (72 48)) :pitch-range "not (low high) with 0 <= low < high < 128")
     ('(:pitch-range (48 48)) :pitch-range "not (low high) with 0 <= low < high < 128")
     ('(:pitch-range (48)) :pitch-range "not (low high) with 0 <= low < high < 128")
     ('(:pitch-range (40 60 80)) :pitch-range "not (low high) with 0 <= low < high < 128")
     ('(:pitch-range (-1 60)) :pitch-range "not (low high) with 0 <= low < high < 128")
     ('(:pitch-range (40 128)) :pitch-range "not (low high) with 0 <= low < high < 128")
     ('(:pitch-range 60) :pitch-range "not (low high) with 0 <= low < high < 128")
     ('(:tempi 90) :tempi "unknown hint"))
  (let ((c (caught (lambda () (normalize-hints hints)) 'invalid-hint)))
    (ok c)
    (ok (and c (eq key (invalid-hint-key c))))
    (ok (and c (equal (getf hints key) (invalid-hint-value c))))
    (ok (and c (equal reason (transcribe-error-reason c))))))

(deftest normalize-hints-cause
  (let ((c (caught (lambda () (normalize-hints '(:key "H"))) 'invalid-hint)))
    (ok (typep (and c (invalid-hint-cause c)) 'mt:invalid-key)))
  (let ((c (caught (lambda () (normalize-hints '(:meter "x/4"))) 'invalid-hint)))
    (ok (typep (and c (invalid-hint-cause c)) 'mt:invalid-meter)))
  (let ((c (caught (lambda () (normalize-hints '(:tempo -1))) 'invalid-hint)))
    (ok (and c (null (invalid-hint-cause c))))))

(deftest-parametrize normalize-hints-type-errors
    ((hints)
     (:key)
     ('(:key))
     ('("key" "G"))
     ('(:key "G" . :tempo)))
  (ok (signals (normalize-hints hints) 'type-error)))

(deftest normalize-hints-use-value
  (let ((out (handler-bind ((invalid-hint (lambda (c) (use-value "D" c))))
               (normalize-hints '(:tempo 80 :key 5)))))
    (ok (equalp (list :key (mt:parse-key "D") :tempo (mt:make-tempo :bpm 80)) out)))
  (testing "the replacement is checked again"
    (let* ((seen '())
           (out (handler-bind ((invalid-hint
                                 (lambda (c)
                                   (push (invalid-hint-value c) seen)
                                   (use-value (if (eql (invalid-hint-value c) 0) -5 120) c))))
                  (normalize-hints '(:tempo 0)))))
      (ok (equal '(-5 0) seen))
      (ok (equalp (list :tempo (mt:make-tempo :bpm 120)) out))))
  (testing "no USE-VALUE for an unknown key"
    (let ((restarts (handler-case (normalize-hints '(:colour "blue"))
                      (invalid-hint (c) (mapcar #'restart-name (compute-restarts c))))))
      (ok (member 'continue restarts))
      (ok (not (member 'use-value restarts))))))

(deftest normalize-hints-continue
  (let ((out (handler-bind ((invalid-hint #'continue))
               (normalize-hints '(:colour "blue" :key "A" :tempo "slow" :meter "3/4")))))
    (ok (equalp (list :key (mt:parse-key "A") :meter (mt:parse-meter "3/4")) out)))
  (ok (equalp (list :tempo (mt:make-tempo :bpm 3/2))
              (handler-bind ((invalid-hint #'continue))
                (normalize-hints '(:key 1 :meter 2 :tempo 3/2 :pitch-range 4))))
      "only the tempo survives"))

(deftest hint-pitch-range-default-and-given
  (ok (equal '(36 84) (hint-pitch-range nil)))
  (ok (not (eq +default-pitch-range+ (hint-pitch-range nil))))
  (ok (equal '(45 69) (hint-pitch-range '(:key "C" :pitch-range (45 69)))))
  (ok (signals (hint-pitch-range '(:pitch-range (70 60))) 'invalid-hint)))
