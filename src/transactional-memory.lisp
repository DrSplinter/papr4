(defpackage #:papr4/transactional-memory
  (:use :cl)
  (:nicknames :tm :transactional-memory)
  (:import-from :stmx
		:transactional
		:atomic
		:retry)
  (:export :transactional
	   :atomic
	   :retry)
  (:documentation
   "Fundamentals for transactional memory synchronization.

Main macro is TRANSACTIONAL which wraps class (struct) definition
and makes its slots aware of transactions.

Example:
```
(transactional
 (defclass counter ()
   ((count :initform 0))))
```

To exclusively access slots of transactional class (struct) use
macro ATOMIC. It is a realization of transaction. According to
stmx low level library it is recommended to use function SLOT-VALUE
to access slots or to use macro WITH-SLOTS since it expands to usage
of function SLOT-VALUE.

Example:
```
(defmethod increment ((counter counter))
  (atomic
    (incf (slot-value counter 'count))))
```

Be aware of transaction retries which are essential for them to work
in multithreaded environment. Hence, do not use IO inside transactions.

Whenever it is needed to wait for some condition to be true inside
transaction use function RETRY.

Example:
```
(defmethod decrement ((counter count))
  (with-slots (count) counter
    (atomic
      (if (plusp count)
          (decf count)
          (retry)))))
```
"))
(in-package #:papr4/transactional-memory)
