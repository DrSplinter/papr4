(defpackage #:papr4/barrier
  (:use :cl :papr4/semaphore)
  (:nicknames :barrier)
  (:export :barrier
	   :counting-barrier
	   :wait-on-barrier))
(in-package #:papr4/barrier)

;;;
;;; ABSTRACT CLASS
;;;

(defclass barrier ()
  ((n
    :initarg :threads)))

(defgeneric wait-on-barrier (barrier &key))

;;;
;;; IMPLEMENTATIONS
;;;

(defclass counting-barrier (barrier)
  ((n
    :initarg :threads)
   (count
    :initform 0)
   (arrival
    :initform (semaphore 1))
   (departure
    :initform (semaphore 0))))

(defmethod wait-on-barrier ((barrier counting-barrier) &key)
  (with-slots (n count arrival departure) barrier
    (wait-on-semaphore arrival)
    (incf count)
    (if (= count n)
	(signal-semaphore departure)
	(signal-semaphore arrival))
    (wait-on-semaphore departure)
    (decf count)
    (if (> count 0)
	(signal-semaphore departure)
	(signal-semaphore arrival))))

(defun counting-barrier (n)
  (make-instance 'counting-barrier :threads n))
