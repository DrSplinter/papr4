(defpackage #:papr4-test/semaphore
  (:use :cl :rove :check-it :papr4-test/utils
	:papr4/semaphore))
(in-package #:papr4-test/semaphore)

(defop op-wait-on)
(defop op-signal)

(defclass counter ()
  ((count
    :initarg :count
    :accessor counter-count)))

(defmethod execute ((model counter) (op op-wait-on))
  (decf (counter-count model))
  model)

(defmethod execute ((model counter) (op op-signal))
  (incf (counter-count model))
  model)


(defmethod execute ((model semaphore) (op op-wait-on))
  (wait-on-semaphore model)
  model)

(defmethod execute ((model semaphore) (op op-signal))
  (signal-semaphore model)
  model)

(defmethod equivp ((m1 semaphore) (m2 counter))
  (= (semaphore-count m1)
     (counter-count m2)))

(check-it:def-generator critical-section ()
  (generator
   (or (tuple (op-wait-on) (op-signal))
       (tuple (op-wait-on) (op-some-work) (op-signal)))))

(deftest semaphore-properties
  (testing "Semaphore with at least one resource has the same number
  of resources after any number of critical sections."
    (ok
     (check-it
      (generator
       (tuple (integer 1 *)
	      (list (thread (critical-section)))))
      (lambda (x)
	(destructuring-bind (resources seq-ops) x
	  (check-equivalence
	   (make-instance 'semaphore :count resources)
	   (make-instance 'counter :count resources)
	   seq-ops)))))))
