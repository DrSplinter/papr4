(defpackage #:papr4/user
  (:use :cl)
  (:nicknames :papr4-user)
  (:import-from :papr4/thread
		:thread
		:thread-name
		:start-thread
		:join-thread
		:all-threads
		:kill-all-threads)
  (:import-from :papr4/semaphore
		:semaphore
		:wait-on-semaphore
		:signal-semaphore)
  (:import-from :papr4/barrier
		:barrier
		:wait-on-barrier
		:counting-barrier)
  (:import-from :papr4/monitor
		:monitor
		:define-monitor-method
		:condition-variable
		:wait-on-condition
		:signal-condition)
  (:import-from :papr4/transactional-memory
		:transactional
		:atomic
		:retry)
  (:import-from :papr4/pmmp
		:init-pmmp
		:run-pmmp
		:send
		:asend
		:receive
		:probe
		:broadcast
		:scatter
		:getter))
(in-package #:papr4/user)

;; Nevim jak moc je nasledujici uzitecne, proto je to zatim tady

(defmacro pprogn (&body forms)
  "Evaluate each of FORMS in separate thread and wait them to finish."
  `(let ((threads (list ,@(loop :for form :in forms
			     :collect `(thread ,form)))))
     (loop :for (thread . rest) :on threads
	:when rest :do (join-thread thread)
	:unless rest :return (join-thread thread))))

(defmacro pdotimes ((var count &optional result) &body forms)
  `(progn
     (map nil #'join-thread
	  (loop :for ,var :from 0 :below ,count
	     :collect (let ((,var ,var)) (thread ,@forms))))
     ,result))

(defmacro pdolist ((var list &optional result) &body forms)
  `(progn
     (map nil #'join-thread
	  (loop :for ,var :in ,list
	     :collect (let ((,var ,var)) (thread ,@forms))))
     ,result))
