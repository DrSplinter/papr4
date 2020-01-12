(defpackage #:papr4-test/utils
  (:use :cl :check-it :papr4/thread)
  (:import-from :bordeaux-threads)
  (:import-from :lparallel)
  (:import-from :alexandria)
  (:import-from :closer-mop)
  (:export :execute
	   :equivp
	   :check-equivalence
	   :defop
	   :thread
	   :op-some-work
	   :+op-time+
	   :+thread-overhead+
	   :+max-sleep-milliseconds+
	   :+check-time+)
  (:documentation
   "Defines an API for testing concurent structures.

    To test a structure we need to specify its model, equivalence of
    structure with its model and operations (ops in short) on them and
    their meaning. Then we just need to call function CHECK-EQUIVALENCE
    with some structure, its model and sequences of operations
    representing a collection of threads where each thread successively
    execute its operations.
    
    Equivalence is defined with generic function EQUIVP. The meaning
    of operations is specified for both structure in question and its
    model with generic function EXECUTE."))
(in-package #:papr4-test/utils)

;; TODO zamyslet se nad tim kam presunout, mozna do PAPR4-USER package?
(setf check-it:*num-trials* 10)

;; TODO
;; preusporadat soubor
;; zkontrolovat vsechny dokumentace

(defgeneric equivp (structure model)
  (:documentation
   "Check if STRUCTURE coresponds to its MODEL."))

(defgeneric execute (model op)
  (:documentation
   "Interpretation of OPERATION for MODEL."))

(defmacro defop (name &body definitions)
  "Defines operation NAME where DEFINITIONS conform to
  ((SLOT-NAME TYPE-GENERATOR)*). 

  Operations corespond to key synchronized procedures on structure
  which is tested. TYPE-GENERATOR has to be an instance of class which
  inherits from CHECK-IT:GENERATOR ... TODO"
  (flet ((symbol->keyword (symbol)
	   (intern (symbol-name symbol) :keyword))
	 (alist->plist (alist varkey)
	   (loop :for (var val) :in alist
	      :appending (list (funcall varkey var) val))))
    `(progn
       (defstruct ,name ,@(mapcar #'car definitions))
       (check-it:def-genex-macro ,name ()
	 `(check-it:struct
	   ,',name
	   ,@',(alist->plist definitions #'symbol->keyword))))))

(defparameter +max-sleep-milliseconds+ 500
  "Maximum number of milliseconds to wait in sleep operation.")

(defop op-some-work
  ;; General operation to imitate any computation.
  (seconds (map (lambda (x) (/ x 1000))
		(integer 1 +max-sleep-milliseconds+))))

(defun prepare-thread (structure ops)
  "Create thread as future which first waits on LATCH and then execute
  OPERATIONS on STRUCTURE."
  (make-instance 'thread
		 :function
		 (lambda ()
		   (reduce #'execute ops :initial-value structure))))

(defvar *expected-time-multiple* 0
  "The multiple of expected time to wait for any thread to finish.")

(defparameter +recheck-time+ 1/2
  "Interval between checking if threads has ended.")

(defun watch-threads (threads wait-time)
  "Wait until THREADS finish their jobs and print warning of potential
  deadlock everytime WAIT-TIME has elapsed."
  (let ((warning-time (+ wait-time (get-universal-time))))
    (loop :until (every #'thread-finished-p threads)
       :do (sleep +recheck-time+)
       :when (>= (get-universal-time) warning-time)
       :do (progn (incf warning-time wait-time)
		  (warn "(~:r time) There is a potential of deadlock."
			(incf *expected-time-multiple*))))))

(defun async-execute (structure seq-ops)
  "Execute each operations in SEQUENCE-OPERATIONS on STRUCTURE in
  separate thread and watch them until they finish."
  (let* ((threads (mapcar (lambda (ops)
			    (prepare-thread structure ops))
			  seq-ops))
	 (wait-time (loop :for ops :in seq-ops
		       :maximize (ops-time ops))))
    (map nil #'start-thread threads)
    (let ((*expected-time-multiple* 0))
      (watch-threads threads wait-time)) 
    structure))

(defun shallow-copy-object (original)
  "Make a shallow copy of ORIGINAL object."
  (let* ((class (class-of original))
         (copy (allocate-instance class)))
    (dolist (slot (mapcar #'closer-mop:slot-definition-name
			  (closer-mop:class-slots class)))
      (when (slot-boundp original slot)
        (setf (slot-value copy slot)
              (slot-value original slot))))
    copy))

(defun map-interleavings (function list-of-lists &optional path)
  (loop :with endp = t
     :for index :from 0
     :for list :in list-of-lists
     :unless (null list)
     :do (progn (setf endp nil)
		(let ((elem (pop (nth index list-of-lists))))
		  (map-interleavings function
				     list-of-lists
				     (cons elem path))
		  (push elem (nth index list-of-lists))))
     :finally (when endp
		(funcall function (reverse path)))))

(defun map-sync-executions (function model seq-ops)
  "Execute each operations in SEQUENCE-OPERATIONS on MODEL for each
  interleaving."
  (map-interleavings
   (lambda (ops)
     (funcall function
	      (reduce #'execute ops
		      ;;FIXME tady bude problem s modifikaci vnorenych
		      ;;struktur
		      :initial-value (shallow-copy-object model))))
   (mapcar (lambda (ops)
	     (remove 'op-some-work ops :key #'type-of))
	   seq-ops)))

(defparameter +op-time+ 60/1000
  "Expected time needed to finish a non-sleep operation. Measured in
  seconds.")
(defparameter +op-overhead+ 60/1000
  "Expected overhead needed to execute an operation. Measured in
  seconds.")

(defun ops-time (ops)
  "Return expected time which OPERATIONS can take when run in thread."
  (loop :for op :in ops
     :sum (+ +op-overhead+
	     (if (typep op 'op-some-work)
		 (op-some-work-seconds op)
		 +op-time+))))

(defmethod execute (model (op op-some-work))
  (sleep (op-some-work-seconds op))
  model)

(check-it:def-genex-macro thread (&rest ops)
  ;; Thread code generator with some work between OPERATIONS.
  `(map #'alexandria:flatten
	(list (or ,@ops ,@(mapcar (lambda (op)
				    `(tuple ,op (op-some-work)))
				  ops)))))

;; (check-it:def-genex-macro thread-list (&rest threads)
;;   ;; Thread code generator with some work between OPERATIONS.
;;   `(map #'alexandria:flatten
;; 	(list (or ,@ops ,@(mapcar (lambda (op)
;; 				    `(tuple ,op (op-some-work)))
;; 				  ops)))))

(defun drop-op-some-work (seq-ops)
  (mapcar (lambda (ops) (remove 'op-some-work ops :key #'type-of)) seq-ops))

(defparameter +recheck-equivalence+ 10
  "The number of times asynchronous execution is runned to ensure we
  are able to repeat race conditions.")

(defun check-equivalence (structure model seq-ops)
  "TODO"
  (loop :repeat +recheck-equivalence+
     :always
       (let* ((structure (shallow-copy-object structure))
	      (svalue (async-execute structure seq-ops)))
	 (map-sync-executions
	  (lambda (mvalue)
	    (when (equivp svalue mvalue)
	      (return-from check-equivalence t)))
	  model
	  (drop-op-some-work seq-ops)))))

;; TODO:
;; udelat funkci for-all
;; asi semka presunout guard interleavings a jeste upravit api

