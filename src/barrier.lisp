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
   (mutex
    :initform (semaphore 1))
   (arrival-turnstile
    :initform (semaphore 0))
   (departure-turnstile
    :initform (semaphore 0))))

(defmethod wait-on-barrier ((barrier counting-barrier) &key)
  (with-slots (n count mutex arrival-turnstile departure-turnstile) barrier
    (critical-section mutex
      (incf count)
      (when (= count n)
        (signal-semaphore arrival-turnstile n)))
    (wait-on-semaphore arrival-turnstile)
    
    (critical-section mutex
      (decf count)
      (when (= count 0)
        (signal-semaphore departure-turnstile n)))
    (wait-on-semaphore departure-turnstile)))

(defun counting-barrier (n)
  (make-instance 'counting-barrier :threads n))
