(defpackage #:papr4/message-passing
  (:use :cl :papr4/semaphore)
  (:nicknames :mp :message-passing)
  (:import-from :papr4/thread)
  (:import-from :alexandria)
  (:import-from :trivia)
  (:export :channel
	   :send
	   :receive
	   :receive-match
	   :actor))
(in-package #:papr4/message-passing)

(defmethod send ((receivers list) value)
  (mapcar (lambda (receiver) (send receiver value)) receivers))

(defclass channel ()
  ((lock
    :initform (semaphore 1))
   (non-empty-p
    :initform (semaphore 0))
   (queue
    :initform (semaphore 0))
   (buffer
    :initform nil)))

(defun channel ()
  (make-instance 'channel))

(define-modify-macro appendf (sequence &rest more-sequences) append)

(defmethod send ((channel channel) value)
  (with-slots (lock buffer non-empty-p queue) channel
    (critical-section lock
      (appendf buffer (list value)))
    (signal-semaphore non-empty-p)
    (signal-semaphore queue (semaphore-waiters queue)))
  channel)

(defmacro removef (item sequence &rest args)
  `(setf ,sequence (remove ,item ,sequence ,@args)))

(defmethod receive ((channel channel) &optional match-function)
  (let ((match-function (or match-function (constantly t))))
    (with-slots (lock buffer queue non-empty-p) channel
      (wait-on-semaphore non-empty-p)
      (critical-section lock
	(loop :for value :in buffer
	   :for match = (funcall match-function value)
	   :when match :do
	     (progn (removef value buffer :count 1)
		    (return-from receive
		      (values value match)))))
      (signal-semaphore non-empty-p)
      (wait-on-semaphore queue)
      (receive channel match-function))))

;; (defmacro receive-match (channel &body clauses)
;;   (alexandria:with-unique-names (value)
;;     `(receive ,channel
;; 	      (lambda (,value)
;; 		(trivia:match ,value
;; 		   ,@(loop :for (pattern . forms) :in clauses
;; 		        :collect `(,pattern (lambda () ,@forms))))))))

(defclass actor (thread:thread)
  ((mailbox
    :initform (channel))))

(defmethod send ((actor actor) value)
  (send (slot-value actor 'mailbox) value))

(defmethod initialize-instance :before ((actor actor) &key function)
  (setf (slot-value actor 'function)
	(lambda () (funcall function actor))))

(defmacro actor (&body body)
  `(start (make-instance
	   'actor
	   :function (lambda (self)
		       (declare (ignorable self))
		       (flet ((receive &optional match-function)
			        (receive (slot-value self 'mailbox)
				  match-function))
			 ,@body)))))

