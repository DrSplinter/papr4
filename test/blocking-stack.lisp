(defpackage #:papr4-test/blocking-stack
  (:use :cl :rove :check-it :papr4-test/utils
	:papr4/blocking-stack))
(in-package #:papr4-test/blocking-stack)

(defclass stack ()
  ((buffer
    :initform nil
    :accessor stack-buffer)))

(defop op-push
  (value (integer)))

(defop op-pop)

(defmethod execute ((model stack) (op op-push))
  (push (op-push-value op) (stack-buffer model))
  model)

(defmethod execute ((model stack) (op op-pop))
  (pop (stack-buffer model))
  model)

(defmethod execute ((model blocking-stack) (op op-push))
  (blocking-push model (op-push-value op))
  model)

(defmethod execute ((model blocking-stack) (op op-pop))
  (blocking-pop model)
  model)

(defmethod equivp ((structure blocking-stack) (model stack))
  (equalp (blocking-stack-buffer structure)
	  (stack-buffer model)))

(defun more-pushes-p (seq-ops)
  (let ((op-types (mapcar #'type-of (apply #'append seq-ops))))
    (>= (count 'op-push op-types)
	(count 'op-pop op-types))))

(defun factorial (x)
  (reduce #'* (loop :for i :from x :downto 1 :collect i)))

(defun interleavings-count (list-of-lists)
  (let ((lengths (mapcar #'length list-of-lists)))
    (/ (factorial (reduce #'+ lengths))
       (reduce #'* (mapcar #'factorial lengths)))))

(deftest blocking-stack-properties
  (testing "Concurrent producers and consumers do not corrupt
  buffer of sem-blocking-stack."
    (ok
     (check-it
      (generator
       (guard
	(lambda (x)
	  (< (interleavings-count x) #.(expt 10 9)))
	(guard
	 #'more-pushes-p
	 (list (or (thread (op-push))
		   (thread (op-pop)))))))
      (lambda (seq-ops)
	(check-equivalence
	 (make-instance 'sem-blocking-stack)
	 (make-instance 'stack)
	 seq-ops)))))
  
  (testing "Concurrent producers and consumers do not corrupt
  buffer of mon-blocking-stack."
    (ok
     (check-it
      (generator
       (guard
	(lambda (x)
	  (< (interleavings-count x) #.(expt 10 9)))
	(guard
	 #'more-pushes-p
	 (list (or (thread (op-push))
		   (thread (op-pop)))))))
      (lambda (seq-ops)
	(check-equivalence
	 (make-instance 'mon-blocking-stack)
	 (make-instance 'stack)
	 seq-ops)))))

  ;; TODO Pro testovani TM je potreba udelat kopii transakcni tridy
  
  ;; (testing "Concurrent producers and consumers do not corrupt
  ;; buffer of tm-blocking-stack."
  ;;   (ok
  ;;    (check-it
  ;;     (generator
  ;;      (guard
  ;; 	(lambda (x)
  ;; 	  (< (interleavings-count x) #.(expt 10 9)))
  ;; 	(guard
  ;; 	 #'more-pushes-p
  ;; 	 (list (or (thread (op-push))
  ;; 		   (thread (op-pop)))))))
  ;;     (lambda (seq-ops)
  ;; 	(check-equivalence
  ;; 	 (make-instance 'tm-blocking-stack)
  ;; 	 (make-instance 'stack)
  ;; 	 seq-ops)))))

  ;; TODO Pro testovani MP je potreba vymyslet stejny koncept dedeni
  ;; abstraktni tridy nebo udelat testovani od podlahy jinak
  
  ;; (testing "Concurrent producers and consumers do not corrupt
  ;; buffer of mp-blocking-stack."
  ;;   (ok
  ;;    (check-it
  ;;     (generator
  ;;      (guard
  ;; 	(lambda (x)
  ;; 	  (< (interleavings-count x) #.(expt 10 9)))
  ;; 	(guard
  ;; 	 #'more-pushes-p
  ;; 	 (list (or (thread (op-push))
  ;; 		   (thread (op-pop)))))))
  ;;     (lambda (seq-ops)
  ;; 	(let ((bstack (start (make-instance 'mp-blocking-stack))))
  ;; 	  (check-equivalence
  ;; 	   bstack
  ;; 	   (make-instance 'stack)
  ;; 	   seq-ops)
  ;; 	  (send bstack :exit))))))
  )

