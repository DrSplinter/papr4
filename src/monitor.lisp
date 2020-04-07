(defpackage #:papr4/monitor
  (:use :cl)
  (:nicknames :monitor)
  (:import-from :alexandria)
  (:import-from :bordeaux-threads)
  (:export :monitor
	   :enter-monitor
	   :exit-monitor
	   :with-locked
	   :define-monitor-method
	   :condition-variable
	   :wait-on-condition
	   :signal-condition
	   :broadcast-condition)
  (:documentation
   "Fundamentals for monitor synchronization.

To make class synchronized simply inherit monitor.

Example:
```
(defclass counter (monitor)
  ((count :initform 0)))
```

Then methods defined with macro DEFINE-MONITOR-METHOD will have
exclusive access to the instance of the class. Note that monitor
class has to be the first argument of method.

Example:
```
(define-monitor-method increment ((counter counter))
  (incf (slot-value counter 'count)))
```

Whenever it is needed to wait for some condition to be true inside
monitor method we need to use conditional variable. So far we have
only non blocking conditional variables. In order to wait for a
condition we use method WAIT-ON-CONDITION. Note that inside monitor
methods it has only one argument. Be aware that non blocking
condition has to be rechecked since it can become false before the 
thread can exclusively access monitor.

On the other hand, when we make the condition true in some monitor
method we should call method SIGNAL-CONDITION to release threads 
waiting for this condition to become true.

Example:
```
(defclass counter (monitor)
  ((count :initform 0)
   (positivep :initform (condition-variable))))

(define-monitor-method decrement ((counter counter))
  (with-slots (count positivep) counter
    (loop :until (plusp count)
       :do (wait-on-condition positivep))
    (decf count)))

(define-monitor-method increment ((counter counter))
  (incf (slot-value counter 'count))
  (signal-condition (slot-value counter 'positivep)))
```
"))
(in-package #:papr4/monitor)

(defclass monitor ()
  ((lock
    :initform (bt:make-recursive-lock)
    :type bt:lock
    :documentation "Reentrant lock for exclusive access to monitor."))
  (:documentation "Class implementing lockable objects."))

(defmethod enter-monitor ((monitor monitor))
  "Lock MONITOR for exclusive access."
  (bt:acquire-lock (slot-value monitor 'lock)))

(defmethod exit-monitor ((monitor monitor))
  "Release MONITOR from exclusive access."
  (bt:release-lock (slot-value monitor 'lock)))

(defclass condition-variable ()
  ((condition-variable
    :initform (bt:make-condition-variable)
    :documentation "Low level condition variable.")
   (lock
    :initform (bt:make-lock)
    :documentation "Lock for exclusive access to counter.")
   (counter
    :initform 0
    :documentation "The number of waiting threads."))
  (:documentation "Class implementing condition variable of monitor."))

(defun condition-variable ()
  "Return a new instance of condition variable class."
  (make-instance 'condition-variable))

(defmethod wait-on-condition ((condvar condition-variable) (monitor monitor))
  "Queues current thread on CONDVAR of MONITOR and blocks until
CONDVAR is signaled."
  (with-slots (condition-variable counter lock) condvar
    (bt:with-lock-held (lock)
      (incf counter)
      (bt:condition-wait condition-variable
			 (slot-value monitor 'lock)))))

(defmethod signal-condition ((condvar condition-variable))
  "Release a thread waiting on CONDVAR."
  (with-slots (condition-variable counter lock) condvar
    (bt:with-lock-held (lock)
      (setf counter (max 0 (1- counter)))
      (bt:condition-notify condition-variable))))

(defmethod broadcast-condition ((condvar condition-variable))
  "Release all threads waiting on CONDVAR."
  (with-slots (condition-variable counter lock) condvar
    (bt:with-lock-held (lock)
      (let ((n counter))
	(setf counter 0)
	(loop :repeat n :do (bt:condition-notify condition-variable))))))

(defmacro with-locked (object &body forms)
  "Macro for safe exclusive access to OBJECT and shadowing method
WAIT-ON-CONDITION while executing FORMS."
  (alexandria:once-only ((object object))
    `(flet ((wait-on-condition (condition)
	      (wait-on-condition condition ,object)))
       (declare (ignorable (function wait-on-condition)))
       (enter-monitor ,object)
       (unwind-protect (progn ,@forms)
	 (exit-monitor ,object)))))

(defmacro define-monitor-method (name (monitor &rest lambda-list) &body body)
  "Definer for monitor method."
  `(defmethod ,name (,monitor ,@lambda-list)
     (with-locked ,(if (consp monitor) (car monitor) monitor)
       ,@body)))


;; TODO:
;; implementovat blocking-condition-variable
;; prejmenovat condition-variable na non-blocking-condition-variable
;; udelat java-monitor
;; pak vse pouzit pri implementaci blocking-stack a udelat testy
