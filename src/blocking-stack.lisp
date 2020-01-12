(defpackage #:papr4/blocking-stack
  (:use :cl
	:papr4/monitor
	:papr4/semaphore
	:papr4/transactional-memory
	;; :papr4/message-passing
	)
  (:nicknames :bs)
  (:export :blocking-stack
	   :blocking-push
	   :blocking-pop
	   :blocking-stack-buffer
	   :sem-blocking-stack
	   :mon-blocking-stack
	   :tm-blocking-stack
	   ;; :mp-blocking-stack
	   ))
(in-package #:papr4/blocking-stack)

;;;
;;; ABSTRACT CLASS
;;;

;; This is needed for macro TRANSACTIONAL.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass blocking-stack ()
    ((buffer
      :initform nil
      :documentation "Container for stack values where first element
      is top of stack."))
    (:documentation
     "Abstract class for thread-safe stack. 

Works exactly like classic stack where values on stack are buffered
and the top of the stack is manipulated with methods BLOCKING-PUSH and
BLOCKING-POP. Moreover stacks buffer can be accessed with method
BLOCKING-STACK-BUFFER.

The implementation of thread-safe stack requires to implement above
mentioned methods in a thread-safe way so it is not possible to
corrupt stacks buffer.")))

(defgeneric blocking-stack-buffer (stack &key)
  (:documentation
   "Return all values of buffer."))
(defgeneric blocking-push (stack value)
  (:documentation
   "Make VALUE the new top of STACK."))
(defgeneric blocking-pop (stack &key)
  (:documentation
   "Remove and return the top of stack. When stack is empty, block
current thread until stack is non-empty."))


;;;
;;; IMPLEMENTATIONS
;;;

;; SEMAPHORE

(defclass sem-blocking-stack (blocking-stack)
  ((mutex
    :initform (make-instance 'semaphore :count 1)
    :type semaphore
    :documentation "Semaphore guarding access to slots.")
   (non-empty-p
    :initform (make-instance 'semaphore :count 0)
    :type semaphore
    :documentation "Semaphore guarding access to empty stack."))
  (:documentation "Blocking stack implementation using semaphores."))

(defmethod blocking-stack-buffer ((stack sem-blocking-stack) &key)
  (with-slots (mutex buffer) stack
    (critical-section mutex
      (copy-list buffer))))

(defmethod blocking-push ((stack sem-blocking-stack) value)
  (with-slots (mutex buffer non-empty-p) stack
    (critical-section mutex
      (push value buffer))
    (signal-semaphore non-empty-p))
  stack)

(defmethod blocking-pop ((stack sem-blocking-stack) &key)
  (with-slots (mutex buffer non-empty-p) stack
    (wait-on-semaphore non-empty-p)
    (critical-section mutex
      (pop buffer))))

;; MONITOR

(defclass mon-blocking-stack (blocking-stack monitor)
  ((non-empty-p
    :initform (make-instance 'condition-variable)
    :type condition-variable
    :documentation "Condition variable guarding access to empty stack."))
  (:documentation "Blocking stack implementation using monitor."))

(define-monitor-method blocking-stack-buffer ((stack mon-blocking-stack) &key)
  (with-slots (buffer) stack
    (copy-list buffer)))

(define-monitor-method blocking-push ((stack mon-blocking-stack) value)
  (with-slots (buffer non-empty-p) stack
    (push value buffer)
    (signal-condition non-empty-p))
  stack)

(define-monitor-method blocking-pop ((stack mon-blocking-stack) &key)
  (with-slots (buffer non-empty-p) stack
    (loop :while (null buffer)
       :do (wait-on-condition non-empty-p))
    (pop buffer)))

;; TRANSACTIONAL MEMORY

(transactional
 (defclass tm-blocking-stack (blocking-stack)
   ((buffer
     :initform nil
     :documentation "Transactional value of stacks buffer."))
   (:documentation "Blocking stack implementation using transactional
memory.")))

(defmethod blocking-stack-buffer ((stack tm-blocking-stack) &key)
  (atomic (copy-list (slot-value stack 'buffer))))

(defmethod blocking-push ((stack tm-blocking-stack) value)
  (atomic (push value (slot-value stack 'buffer)))
  stack)

(defmethod blocking-pop ((stack tm-blocking-stack) &key)
  (atomic
   (with-slots (buffer) stack
     (if (null buffer)
	 (retry)
	 (pop buffer)))))

;; MESSAGE PASSING

;; (defclass mp-blocking-stack (blocking-stack actor) ())

;; (defun stack-actor-function (self)
;;   (with-slots (buffer) self
;;     (loop :while
;; 	 (receive-match self
;; 	   ((list :buffer sender)
;; 	    (copy-list buffer))
;; 	   ((list :push value)
;; 	    (push value buffer))
;; 	   ((list :pop sender)
;; 	    (send sender (pop buffer)))
;; 	   (:exit nil)))))

;; (defmethod initialize-instance ((stack mp-blocking-stack) &key)
;;   (setf (slot-value stack 'function) #'stack-actor-function))

;; (defmethod blocking-stack-buffer ((stack mp-blocking-stack) &key sender)
;;   (send stack (list :buffer sender)))

;; (defmethod blocking-push ((stack mp-blocking-stack) value)
;;   (send stack (list :push value)))

;; (defmethod blocking-pop ((stack mp-blocking-stack) &key sender)
;;   (send stack (list :pop sender))
;;   (receive sender))
