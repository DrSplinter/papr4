;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Kod pro cviceni KMI/PARA.
;; zalozeno na bordeax-threads, bt-semaphores, stmx
;;
;; CHANGELOG:
;; ----------
;;  2.3. pridano: wrap-form, a-progn, a-dolist
;; 
;;  9.3. pridano: *big-para-log*, swap
;; 
;;  16.3. pridany semafory: (make-semaphore, sem-wait, sem-signal, sem-count) a
;;        binarni semafory. (make-mutex, mutex-wait, mutex-signal).
;; 
;;  29.3. reimplementace mutexu, aby je nevlastnila vlakna, upraven kod z bt-semaphore
;;        pridana implementace jednoduche pocitaci bariery (make-barrier, barrier-wait)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; nahrani knihoven
(ql:quickload "bordeaux-threads")
(ql:quickload "bt-semaphore")
(ql:quickload "stmx")

	
;; aprogn (asynchronni progn)
;; syntax: (a-progn
;;             forma1
;;             forma2
;;             ...)
;; semantika: asynchronne vola vyhodnoceni forem, kazdou v jednom vlaknu
;;            vraci seznam zavolanych vlaken, ve vlaknech je navazana
;;            promenna pro standardni vystup

(defmacro wrap-form (form)
  (let ((so (gensym)))
  `(let ((,so *standard-output*))
     (lambda ()
       (let ((*standard-output* ,so))
	 ,form)))))

(defmacro a-progn (&body body)
  (let ((ret (list 'let))
	(thds (gensym)))
    (nconc ret `( ((,thds nil)) ))
    (dolist (i body)
	    (nconc ret `( (push (bt:make-thread (wrap-form ,i)) ,thds) )))
    (nconc ret `(,thds) )
    ret))

;; pocka na dokonceni vsech vlaken v seznamu thds
(defun wait-to-finish (thds)
  (dolist (i thds)
    (bt::join-thread i)))

;; zabije vsechna vlakna v seznamu thds/ pozor, je nutno opravit zamky!
(defun kill-threads (thds)
  (dolist (i thds)
    (when (bt::thread-alive-p i)
      (bt::destroy-thread i))))


;; asynchroni dolist
;; syntax:  (a-dolist (var list)
;;            body)
;; semantika: postupne vaze prvky list na promenou var, pro kazde navazani
;;            spusti body ve vlastnim vlakne, ve vlaknech je navazana promenna
;;            pro standardni vystup
;; navratova hodnota: seznam spustenych vlaken, vlakna jsou pojmenovana podle hodnot var

(defmacro a-dolist ((var list) &body body)
  (let ((thds (gensym))
	(unf-body `(progn ,@body)))
    `(let ((,thds nil))
       (dolist (,var ,list)
	 (push (bt:make-thread (wrap-form ,unf-body) :name (princ-to-string ,var)) ,thds))
       ,thds)))


;;; simulace atomickych operaci zamky
(defparameter *para-big-lock* (bt::make-lock "para-big-lock"))

;; atomicka operace swap, pouziva *big-para-lock* 
(defmacro swap (a b)
  (let ((tmp (gensym)))
    `(progn
       (bt:acquire-lock *para-big-lock*)
       (let ((,tmp ,a))
	 (setf ,a ,b ,b ,tmp))
       (bt:release-lock *para-big-lock*))))


    
;; semafory (s pouzitim bt-semaphore)

;; bezny semafor
(defun make-semaphore (count)
  (bt-sem:make-semaphore :count count))

(defun sem-signal (semaphore)
  (bt-sem:signal-semaphore semaphore))

(defun sem-wait (semaphore)
  (bt-sem:wait-on-semaphore semaphore))

(defun sem-value (semaphore)
  (bt-sem:semaphore-count semaphore))


;; binarni semafor (kvuli shode s prednaskou), open = 1, closed = 0
;; vykradena knihovna bt-semaphore

(defclass mutex ()
  ((lock    :initform (bt:make-lock))
   (condvar :initform (bt:make-condition-variable))
   (count   :initarg  :count)
   (waiters :initform 0)))

(defmethod mutex-signal ((object mutex) &key)
  (with-slots (lock condvar count waiters) object
    (bt:with-lock-held (lock)
      (when (= count 0)
	(incf count))
      (when waiters
	(bt:condition-notify condvar)))))
    
(defmethod mutex-wait ((object mutex) &key)
  (with-slots (lock condvar count waiters) object
    (bt:with-lock-held (lock)
      (unwind-protect
	   (progn
	     (incf waiters)
	     (loop until (> count 0)
		do (bt:condition-wait condvar lock))
	     (decf count))
	(decf waiters)))))

(defun make-mutex (open)
  (make-instance 'mutex :count open))


;; jednoducha pocitaci bariera
(defclass barrier ()
  ((n :initarg :threads)
   (count :initform 0)
   (arrival :initform (make-semaphore 1))
   (departure :initform (make-semaphore 0))))


;; cekani na barrieru
(defmethod barrier-wait((object barrier) &key)
  (with-slots (n count arrival departure) object
    (sem-wait arrival)
    (incf count)
    (if (= count n)
	(sem-signal departure)
	(sem-signal arrival))
    (sem-wait departure)
    (decf count)
    (if (> count 0)
	(sem-signal departure)
	(sem-signal arrival))))


;; vytvoreni bariery
(defun make-barrier (n)
  (make-instance 'barrier :threads n))


;;;;;;;;;;;;;;;;;;;;;;;;
;;;; tests
;;;;;;;;;;;;;;;;;;;;;;;;

