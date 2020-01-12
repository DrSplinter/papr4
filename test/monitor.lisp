(defpackage #:papr4-test/monitor
  (:use :cl :rove :check-it :papr4-test/utils
	:papr4/monitor))
(in-package #:papr4-test/monitor)

(defclass shared-counter (monitor)
  ((count
    :initarg :count
    :accessor shared-counter-count)))

(defclass counter ()
  ((count
    :initarg :count
    :accessor counter-count)))

(defop op-incf
  (value (integer)))

(defop op-decf
  (value (integer)))

(defmethod execute ((model counter) (op op-incf))
  (incf (counter-count model) (op-incf-value op))
  model)

(defmethod execute ((model counter) (op op-decf))
  (decf (counter-count model) (op-decf-value op))
  model)

(define-monitor-method execute ((model shared-counter) (op op-incf))
  (incf (shared-counter-count model) (op-incf-value op))
  model)

(define-monitor-method execute ((model shared-counter) (op op-decf))
  (decf (shared-counter-count model) (op-decf-value op))
  model)

(defmethod equivp ((structure shared-counter) (model counter))
  (= (shared-counter-count structure)
     (counter-count model)))

(deftest monitor-properties
  (testing "Updating monitor counter does not corrupt it."
    (ok
     (check-it
      (generator
       (tuple (integer)
	      (list (thread (op-incf) (op-decf)))))
      (lambda (x)
	(destructuring-bind (initial-value seq-ops) x
	  (check-equivalence
	   (make-instance 'shared-counter :count initial-value)
	   (make-instance 'counter :count initial-value)
	   seq-ops)))))))
