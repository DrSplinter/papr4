(defpackage #:papr4-test/pmmp
  (:use :cl :rove :papr4/pmmp))
(in-package #:papr4-test/pmmp)

;; TODO integrovat testy do rove frameworku

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; test1 synchroniho posilani inkrementu jednomu procesu
;; UKOL: upravit inkrementy posila vice procesu
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun zero-func (id)
  (with-pmmp (id)
    (loop for i from 0 to 5 do
					;(sleep 2)
	 (format t "sending ~A~%" i)
	 (send 1 'number i))
    (format t "all messages sent~%")
    (send 1 'end nil)))
  
(defun one-func (id)
  (let ((number 0))
    (with-pmmp (id)
      (loop
	 (sleep 1)
	 (multiple-value-bind (data sender type) (receive nil nil)
	   (declare (ignorable sender))
	   (if (equalp type 'number)
	       (incf number data)
	       (return))))
      (format t "number is ~A" number))))

(defun test1 ()
  (init-pmmp 2)
  (set-process-function 0 #'zero-func)
  (set-process-function 1 #'one-func)
  (run-pmmp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; asynchronni posilani inkrementu
;; UKOL: upravit, inkrementy posila vice procesu
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun zero-func-async (id)
  (with-pmmp (id)
    (loop for i from 0 to 5 do
	 ;(sleep 2)
	 (format t "sending ~A~%" i)
	 (asend 1 'number i))
    (format t "all messages sent~%")
    (send 1 'end nil)))

(defun test2 ()
  (init-pmmp 2)
  (set-process-function 0 #'zero-func-async)
  (set-process-function 1 #'one-func)
  (run-pmmp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; test 3:  pocitani pi
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generator (id)
  (with-pmmp (id)
    (loop
       (when (probe nil nil)
	 (receive nil nil)
	 ;(format t "generator sending finished ~%")
	 (asend 1 'finish nil)
	 ;(format t "generator finished ~%")
	 (return-from generator))
       ;(format t "generator: sending data ~%")
       (send 1 'number (list (random 10000000000000) (random 100000000))))))

(defun intermediate-guy (id)
  (with-pmmp (id)
    (loop
       (multiple-value-bind (data sender type) (receive nil nil)
	 (declare (ignorable data sender type))
	 (cond  ((equalp type 'number)
		 ;(format t "itermediate sending data ~%")
		 (send 2 'test (if (> (gcd (first data) (second data)) 1) 0 1)))
		((equalp type 'finish)
		 ;(format t "intermediate sending finished ~%")
		 (asend 2 'finish nil)
		 ;(format t "intermediate finished ~%")
		 (return-from intermediate-guy)))))))

(defun compute-pi (pos all)
  (sqrt (/ 6 (- 1 (/ pos all) ))))


(defun pi-guy (id)
  (let ((iterations 0) (positive 0))
    (with-pmmp (id)
      (loop
	   (multiple-value-bind (data sender type) (receive nil nil)
	     (declare (ignorable data sender type))
	     ;(format t "pi-guy received ~A ~A~%" data type)
	     (cond ((equal type 'test)
		    (incf iterations)
		    (when (= data 0)
		      (incf positive))
		    (when (= iterations 1000)
		      ;(format t "pi-guy sending finished ~%")
		      (asend 0 'finish nil)
		      ;(format t "pi-guy after sending finished ~%")
		      ))
		   (t
		    (format t "iterations: ~A  pi:~A~%" iterations (compute-pi positive iterations))	       
		    (return-from pi-guy))))))))
  
(defun test3 ()
  (init-pmmp 3)
  (set-process-function 0 #'generator)
  (set-process-function 1 #'intermediate-guy)
  (set-process-function 2 #'pi-guy)
  (run-pmmp))
		  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;		    
;; test4 -- broadcast test
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun posilac (id)
  (with-pmmp (id)
    (broadcast 10 'number id)
    (format t "broadcast succesfull!~%")))

(defun prijimac (id)
  (with-pmmp (id)
    (multiple-value-bind (data message-type) (broadcast nil nil 0)
      (declare (ignorable data message-type))
      (sleep (random 2))
      (format t "id ~A received ~A ~%" id data))))

(defun test4 ()
  (init-pmmp 6)
  (set-process-function 0 #'posilac)
  (loop for i from 1 to 5 do
       (set-process-function i #'prijimac))
  (run-pmmp))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; test 5 -- scatter test
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun scatter-posilac (id)
  (let ((array (make-array 23 :initial-element 1)))
    (loop for i from 0 to (1- 23) do
	 (setf (aref array i) i))
    (with-pmmp (id)
      (setf array (scatter array 'kus-pole id))
      (format t "scatter succesfull!~%")
      (format t "proces ~A: ~A ~%" id array))))

(defun scatter-prijimac (id)
  (with-pmmp (id)
    (let ((array (scatter nil 'kus-pole 0)))
      (sleep id) ;; kvuli vypisum
      (format t "proces ~A: ~A ~%" id array))))

(defun test5 ()
  (init-pmmp 4)
  (set-process-function 0 #'scatter-posilac)
  (loop for i from 1 to 3 do
       (set-process-function i #'scatter-prijimac))
  (run-pmmp))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; test 6 -- getter test
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun getter-process (id)
  (let ((array (make-array (1+ id))))
    (loop for i from 0 to id do
	 (setf (aref array i) id))
    (with-pmmp (id)
      (cond
	;; -- receiver --
	((= id 0)
	 (setf array (getter array 0))
	 (format t "proces ~A: ~A ~%" id array))
	;; -- senders --
	(t
	 (getter array 0)
	 (sleep id) ;; at si neprepisuji vypisy
	 (format t "proces ~A getter successfull! ~%" id))))))

(defun test6 ()
  (init-pmmp 6)
  (loop for i from 0 to (1- 6) do
       (set-process-function i #'getter-process))
  (run-pmmp))
       


    
    
    
	     
