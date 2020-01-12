(defpackage #:papr4/semaphore
  (:use :cl)
  (:nicknames :sem :semaphore)
  (:import-from :bordeaux-threads)
  (:import-from :alexandria)
  (:export :semaphore
	   :signal-semaphore
	   :wait-on-semaphore
	   :try-semaphore
	   :semaphore-count
	   :semaphore-waiters
	   :critical-section)
  (:documentation
   "Fundamentals for semaphore synchronization.

"))
(in-package #:papr4/semaphore)

;; This file contains modified version of bt-semaphore project downloaded from 
;; https://github.com/rmoritz/bt-semaphore/blob/master/src/semaphore.lisp

(defclass semaphore ()
  ((lock
    :initform (bt:make-lock)
    :documentation
    "Implementation primitive securing mutually exclusive access to
    slots of semaphore.")
   (condition
    :initform (bt:make-condition-variable)
    :documentation
    "Implementation primitive securing blocking of threads which
    want to acquire a resource but none is available.")
   (count
    :initarg :count
    :initform 0
    :type fixnum
    :documentation
    "Number of available resources.")
   (waiters
    :initform 0
    :type fixnum
    :documentation
    "Number of blocked threads waiting to acquire a resource."))
  (:documentation
   "Synchronization primitive securing thread-safe access to certain
   number of resources with blocking if no resource is available."))

(defun semaphore (count)
  (make-instance 'semaphore :count count))

(defmethod signal-semaphore ((semaphore semaphore) &optional (n 1))
  "Return N resources to SEMAPHORE. If there are waiting threads then
  wake up N of them."
  (flet ((signal-semaphore ()
           (with-slots (lock condition count waiters) semaphore
             (bt:with-lock-held (lock)
               (incf count n)
               (loop :repeat (min waiters n)
                  :do (bt:condition-notify condition))))))
    #+sbcl (sb-sys:without-interrupts
             (signal-semaphore))
    #+ccl (ccl:without-interrupts
            (signal-semaphore))
    #-(or sbcl ccl) (signal-semaphore)))

(defmethod wait-on-semaphore ((semaphore semaphore) &key timeout)
  "Acquire a resource of SEMAPHORE if available. Otherwise sleep
  current thread until some resource is returned. Return t on
  success. If TIMEOUT is given, wait maximum TIMEOUT seconds for a
  resource and if no resource is returned in that time, return nil."
  (flet ((wait-on ()
           (with-slots (lock condition count waiters) semaphore
             (bt:with-lock-held (lock)
               (unwind-protect
                    (progn
                      (incf waiters)
                      (loop :while (zerop count)
                         :do (bt:condition-wait condition lock))
                      (decf count))
                 (decf waiters))))
           t))
    (if timeout
        (handler-case
            (bt:with-timeout (timeout)
              (wait-on))
          (bt:timeout ()))
        (wait-on))))

(defmethod try-semaphore ((semaphore semaphore) &optional (n 1))
  "Try to acquire N resources of SEMAPHORE. Return nil if there are
  less than N resources available, otherwise return t."
  (with-slots (lock count) semaphore
    (bt:with-lock-held (lock)
      (when (>= count n)
        (decf count n)
	t))))

(defmethod semaphore-count ((semaphore semaphore))
  "Return the number of resources of SEMAPHORE."
  (with-slots (lock count) semaphore
    (bt:with-lock-held (lock)
      count)))

(defmethod semaphore-waiters ((semaphore semaphore))
  "Return the number of threads waiting on SEMAPHORE."
  (with-slots (lock waiters) semaphore
    (bt:with-lock-held (lock)
      waiters)))

(defmethod print-object ((semaphore semaphore) stream)
  (print-unreadable-object (semaphore stream :type t :identity t)
    (format stream ":COUNT ~A" (semaphore-count semaphore))))

(defmacro critical-section (semaphore &body body)
  (alexandria:once-only (semaphore)
    `(progn (wait-on-semaphore ,semaphore)
	    (unwind-protect (progn ,@body)
	      (signal-semaphore ,semaphore)))))

