;;;; NOTES-CONTOUR: the contour a perfect tracker reports.

(in-package #:transcribe-protocol/tests)

(deftest notes-contour-frames
  ;; 100 Hz frames (rate 1600, hop 16): frame i at i/100 s.
  (let* ((ns (notes '(1/50 1/20 69 1/2) '(1/20 3/25 57)))
         (c (notes-contour ns :sample-rate 1600 :hop 16)))
    (ok (= 1600 (f0-contour-sample-rate c)))
    (ok (= 16 (f0-contour-hop c)))
    ;; ceiling(12/100 * 1600 / 16) = 12 frames: times 0 .. 11/100
    (ok (= 12 (contour-length c)))
    (ok (equalp (vector nil nil 440d0 440d0 440d0 220d0 220d0 220d0 220d0 220d0 220d0 220d0)
                (map 'vector (lambda (f) (and f (/ (round (* f 1000)) 1000d0)))
                     (f0-contour-f0 c))))
    (ok (eql (midi-to-hz 69d0) (aref (f0-contour-f0 c) 2)))
    (ok (equalp #(0d0 0d0 0.5d0 0.5d0 0.5d0 1d0 1d0 1d0 1d0 1d0 1d0 1d0)
                (f0-contour-voicing c)))))

(deftest notes-contour-rests-and-length
  (let ((c (notes-contour (notes '(0 1/100 60) '(3/100 1/25 62)) :sample-rate 100 :hop 1)))
    ;; frames at 0, 1/100, 2/100, 3/100: the second and third fall in the rest
    (ok (= 4 (contour-length c)))
    (ok (equalp (list t nil nil t) (map 'list (lambda (f) (and f t)) (f0-contour-f0 c)))))
  (testing "explicit frame counts"
    (let ((c (notes-contour (notes '(0 1/100 60)) :sample-rate 100 :hop 1 :frames 3)))
      (ok (equalp (list t nil nil) (map 'list (lambda (f) (and f t)) (f0-contour-f0 c)))))
    (let ((c (notes-contour (notes '(0 1 60)) :sample-rate 100 :hop 1 :frames 2)))
      (ok (= 2 (contour-length c)))))
  (testing "no notes"
    (ok (= 0 (contour-length (notes-contour '()))))
    (let ((c (notes-contour '() :frames 5)))
      (ok (= 5 (contour-length c)))
      (ok (every #'null (f0-contour-f0 c)))
      (ok (every #'zerop (f0-contour-voicing c)))))
  (testing "defaults: 16 kHz, hop 256"
    (let ((c (notes-contour (notes '(0 1 60)))))
      (ok (= 16000 (f0-contour-sample-rate c)))
      (ok (= 256 (f0-contour-hop c)))
      (ok (= 63 (contour-length c))))))

(deftest notes-contour-errors
  (ok (signals (notes-contour (list (note 1 2 60) (note 0 1 60))) 'invalid-notes))
  (ok (signals (notes-contour (notes '(0 1 60)) :hop 0) 'type-error))
  (ok (signals (notes-contour (notes '(0 1 60)) :sample-rate 1.5) 'type-error))
  (ok (signals (notes-contour (notes '(0 1 60)) :frames -1) 'type-error)))
