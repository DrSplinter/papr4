(defpackage #:papr4-test/thread
  (:use :cl :rove
	:papr4/thread))
(in-package #:papr4-test/thread)

(defun make-test-threads (values)
  (loop :for x :in values
     :collect
       (make-instance 'thread :function
		      (let ((x x))
			(lambda ()
			  (sleep 0.3)
			  x)))))

(deftest thread-properties
  (let ((values (list 1.1 1 "1" #(1) '(1))))
    (testing "Threads does not do anything before start"
      (make-test-threads values)
      (ok (null (all-threads))))
    
    (testing "Threads doing their job on start."
      (map nil #'start-thread (make-test-threads values))
      (ng (null (all-threads))))
    
    (testing "It is possible to kill all threads."
      (map nil #'start-thread (make-test-threads values))
      (kill-all-threads)
      (sleep 0.1)
      (ok (null (all-threads))))
    
    (testing "Threads return values on join."
      (let ((threads (mapcar #'start-thread (make-test-threads values))))
	(ok (equalp values (mapcar #'join-thread threads)))))))
