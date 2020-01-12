(defsystem #:papr4-test
  :author "Jan Tříska <jan.triska@upol.cz>"
  :license "MIT"
  :class :package-inferred-system
  :depends-on (:papr4-test/thread
	       :papr4-test/semaphore
	       :papr4-test/monitor
	       :papr4-test/blocking-stack)
  :pathname "test"
  :perform (test-op :after (op c) (symbol-call :rove '#:run c)))
