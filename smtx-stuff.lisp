;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PRIKLADY NA SMTX
;; CHANGELOG:
;;     27. 4. 2017 - prvni verze: counter, producent-konzument, horska draha
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COUNTER
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(stmx:transactional
 (defclass foo ()
   ((value1 :type integer :initarg :value1 :initform 0))))


(defparameter *counter* (make-instance 'foo))


(defun increase (instance)
  (stmx:atomic
   (incf (slot-value instance 'value1) 1)))


(stmx:transactional
 (defclass limited-buffer ()
   ((size :type integer :initarg :size :initform 20)
    (full :initform 0)
    (r :initform 0)
    (f :initform 0)
    (buffer :initform nil))))

(defmethod initialize-instance :after ((instance limited-buffer) &key)
  (with-slots (size buffer) instance
    (setf buffer (make-array size))))


(defun put-in (instance what)
  (with-slots (size full f buffer) instance
    (stmx:atomic
     (when (= full size)
       (stmx:retry))
     (setf (aref buffer f) what)
     (setf f (mod (+ f 1) size))
     (incf full))))


(defun take-from (instance)
  (let ((ret nil))
    (with-slots (size full r buffer) instance
      (stmx:atomic
       (when (= full 0)
	 (stmx:retry))
       (setf ret (aref buffer r))
       (setf r (mod (+ r 1) size))
       (decf full)))
    ret))
      

(defparameter *buffer* (make-instance 'limited-buffer :size 10))

(defun produce ()
  (loop
     (put-in *buffer* 1))))

(defun consume ()
  (let ((sum 1))
    (loop while (< sum 1000000)
       do (incf sum (take-from *buffer*)))
    (print sum)))


;; horska draha

(defun customer-load ()
  (print 'load))

(defun run()
  (print 'running))

(defun unload ()
  (print 'unload))

(defun board (id)
  (format t " ~A " id)
  (sleep 0.5))

(defun unboard (id)
  (format t " ~A " id)
  (sleep 0.5))

(stmx:transactional
 (defclass roller-coaster ()
   ((inside :initform 0)
    (get-in :initform nil)
    (get-out :initform nil)
    (run :initform nil)
    (load :initform nil)
    (capacity :initarg :capacity :initform 3))))


(defun coaster-run (instance)
  (with-slots (get-in get-out load) instance
    (loop
       (customer-load)
       (atomic 
	(setf get-in t))
       
       (atomic
	(when (not run)
	  (retry))
	(setf run nil))

       (run)
       (sleep 2)
       (atomic
	(setf get-out t))
       
       (unload)
       (atomic
	(when (not load)
	  (retry))
	(setf load nil)))))


(defun customer-run (instance index)
  (with-slots (get-in get-out instide run load capacity) instance
    (loop
       (atomic
	(when (not get-in)
	  (retry))
	(incf inside)
	(when (= inside capacity)
	  (setf get-in nil)))
       
       (board)
       
       (atomic
	(when (not get-out)
	  (retry))
	(decf inside)
	(when (= inside 0)
	  (setf get-out nil)))

       (unboard))))
	
