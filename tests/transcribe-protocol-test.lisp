(defpackage #:transcribe-protocol/tests
  (:use #:cl #:rove))

(in-package #:transcribe-protocol/tests)

(deftest skeleton-loads
  (ok (find-package '#:transcribe-protocol)))
