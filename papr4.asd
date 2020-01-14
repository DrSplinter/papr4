(setf *load-print* nil)

(defsystem #:papr4
  :author "Jan Tříska <jan.triska@upol.cz>"
  :maintainer "Jan Tříska <jan.triska@upol.cz>"
  :license "MIT"
  :version "1.0"
  :description "Implementation of paralel primitives for solving tasks
  in course KMI/PARA at UPOL\."
  :class :package-inferred-system
  :depends-on (:papr4/user
	       :papr4/blocking-stack)
  :pathname "src"
  :in-order-to ((test-op (test-op #:papr4-test))))
