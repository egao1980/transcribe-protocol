;;;; Note sequences: CHECK-NOTES (one line in time order) and NOTE-PLISTS.

(in-package #:transcribe-protocol/tests)

(deftest check-notes-accepts-one-line
  (let* ((v (notes '(0 1/2 60) '(1/2 1 62) '(5/4 2 64)))
         (checked (check-notes v)))
    (ok (typep checked 'simple-vector))
    (ok (equalp v checked))
    (ok (not (eq v checked)) "a fresh vector")
    (ok (equalp v (check-notes (coerce v 'list))))
    (ok (equalp v (check-notes (make-array 3 :initial-contents v :adjustable t)))))
  (ok (equalp #() (check-notes '())))
  (ok (equalp #() (check-notes #())))
  (ok (= 1 (length (check-notes (list (note 3 4 70)))))))

(deftest-parametrize check-notes-rejects
    ((sequence index reason)
     ((list (note 0 1 60) 'x (note 2 3 60)) 1 "not a note event")
     ((list (note 0 1 60) (note 1 2 62) 64) 2 "not a note event")
     ((list (note 1 2 60) (note 0 1/2 62)) 1 "out of order")
     ((list (note 0 1 60) (note 2 3 62) (note 3/2 5/2 64)) 2 "out of order")
     ((list (note 0 1 60) (note 1/2 2 62)) 1 "overlapping notes")
     ((list (note 0 1 60) (note 0 1/2 62)) 1 "overlapping notes")
     ((vector (note 0 1 60) (note 1 2 62) (note 1999/1000 3 64)) 2 "overlapping notes"))
  (let ((c (caught (lambda () (check-notes sequence)) 'invalid-notes)))
    (ok c)
    (ok (and c (eql index (invalid-notes-index c))))
    (ok (and c (equal reason (transcribe-error-reason c))))))

(deftest check-notes-arguments
  (ok (signals (check-notes 5) 'type-error))
  (ok (signals (check-notes (list* (note 0 1 60) (note 1 2 60))) 'type-error))
  (ok (signals (check-notes "abc") 'invalid-notes)))

(deftest note-plists-shape
  (let ((plists (note-plists (notes '(0 1/2 60.25d0 0.75d0) '(1/2 3/2 62)))))
    (ok (equal (list (list :onset 0 :offset 1/2 :pitch 60.25d0 :confidence 0.75d0)
                     (list :onset 1/2 :offset 3/2 :pitch 62d0 :confidence 1d0))
               plists)))
  (ok (null (note-plists #())))
  (testing "any order, overlapping or not"
    (let ((plists (note-plists (list (note 1 2 60) (note 0 3 64)))))
      (ok (equal '(1 0) (mapcar (lambda (p) (getf p :onset)) plists)))))
  (ok (signals (note-plists (list (note 0 1 60) :x)) 'invalid-notes))
  (ok (signals (note-plists 'x) 'type-error)))
