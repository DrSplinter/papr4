(defpackage #:papr4/thread
  (:use :cl)
  (:nicknames :thread)
  (:import-from :bordeaux-threads)
  (:import-from :alexandria)
  (:export :thread
	   :thread-name
	   :thread-status
	   :thread-finished-p
	   :start-thread
	   :join-thread
	   :all-threads
	   :kill-all-threads))
(in-package #:papr4/thread)

(defclass thread ()
  ((function
    :initarg :function
    :type function
    :documentation "Function to be executed.")
   (name
    :initarg :name
    :initform "Anonymous"
    :reader thread-name
    :type string
    :documentation "Name identifier.")
   (executor
    :type bt:thread
    :documentation "Process executing the function."))
  (:documentation "Container for thread."))

(defun arglist (fn)
  "Return the signature of the function."
  #+allegro (excl:arglist fn)
  #+clisp (sys::arglist fn)
  #+(or cmu scl)
  (let ((f (coerce fn 'function)))
    (typecase f
      (STANDARD-GENERIC-FUNCTION (pcl:generic-function-lambda-list f))
      (EVAL:INTERPRETED-FUNCTION (eval:interpreted-function-arglist f))
      (FUNCTION (values (read-from-string (kernel:%function-arglist f))))))
  #+cormanlisp (ccl:function-lambda-list
                (typecase fn (symbol (fdefinition fn)) (t fn)))
  #+gcl (let ((fn (etypecase fn
                    (symbol fn)
                    (function (si:compiled-function-name fn)))))
          (get fn 'si:debug))
  #+lispworks (lw:function-lambda-list fn)
  #+lucid (lcl:arglist fn)
  #+sbcl (sb-introspect:function-lambda-list fn)
  #-(or allegro clisp cmu cormanlisp gcl lispworks lucid sbcl scl)
  (error 'not-implemented :proc (list 'arglist fn)))

(defmethod initialize-instance :after ((thread thread) &key function)
  (unless function
    (error "Thread object need a function to execute."))
  (unless (null (arglist function))
    (error "Thread object function cannot have arguments.")))

(defmethod thread-status ((thread thread))
  (with-slots (executor) thread
    (cond ((not (slot-boundp thread 'executor)) :PREPARED)
	  ((bt:thread-alive-p executor) :RUNNING)
	  (t :FINISHED))))

(defmethod print-object ((thread thread) stream)
  (print-unreadable-object (thread stream :type t :identity t)
    (format stream "~S ~S"
	    (thread-name thread)
	    (thread-status thread))))

(defmethod thread-finished-p ((thread thread))
  (eql (thread-status thread) :finished))

(defclass thread-pool ()
  ((lock
    :initform (bt:make-lock "thread-pool")
    :documentation "Lock for exclusive access to list of threads.")
   (list
    :initform nil
    :type list
    :documentation "List of all running threads in pool."))
  (:documentation "Container for running threads."))

(defvar *global-thread-pool* (make-instance 'thread-pool)
  "Global thread pool for monitoring all instances of thread.")

(defun add-thread (thread &optional (pool *global-thread-pool*))
  "Register running THREAD to POOL."
  (bt:with-lock-held ((slot-value pool 'lock))
    (push thread (slot-value pool 'list)))
  thread)

(defun rem-thread (thread &optional (pool *global-thread-pool*))
  "Unregister running THREAD from POOL."
  (bt:with-lock-held ((slot-value pool 'lock))
    (setf (slot-value pool 'list)
	  (delete thread (slot-value pool 'list))))
  thread)

(defun current-thread (&optional (pool *global-thread-pool*))
  "Get instance of current running thread registered in POOL."
  (find (bt:current-thread) (slot-value pool 'list)
	:key (lambda (x) (slot-value x 'executor))))

(defmethod start-thread ((thread thread) &optional (pool *global-thread-pool*))
  "Run THREADs function in POOL."
  (with-slots (function name executor) thread
    (let ((fn (lambda ()
		(add-thread thread pool)
		(unwind-protect (funcall function)
		  (rem-thread thread pool)))))
      (setf executor
	    (bt:make-thread fn :name name
			    :initial-bindings
			    `((*standard-output* . ,*standard-output*))))))
  thread)

(defmethod join-thread ((thread thread))
  "Get return value of THREAD. If value is not yet available block
current thread."
  (bt:join-thread (slot-value thread 'executor)))

(defun all-threads (&optional (pool *global-thread-pool*))
  "Get list of all running threads in POOL."
  (copy-list (slot-value pool 'list)))

(defun kill-all-threads (&optional (pool *global-thread-pool*))
  "Kill all running threads in POOL. This function is for debugging
purpose only."
  (warn "Dangerous operation which can lead to inconsistent state. Use
  this with caution.")
  (loop :for thread :in (all-threads pool)
     :for executor = (slot-value thread 'executor)
     :when (bt:thread-alive-p executor)
     :do (bt:destroy-thread executor)))

(defmacro thread (&body forms)
  "Start a new thread which executes FORMS."
  `(start-thread (make-instance 'thread :function (lambda () ,@forms))))
