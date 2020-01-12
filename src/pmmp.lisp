;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; PMMP - poor mans message passing, version 0.01, for educational purposes only
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; API - dokumentace
;;
;; obecne: ocekava se, ze vlakna jsou cislovana od 0 po pocet-vlaken minus 1
;;
;; Prace s thread poolem:
;; ======================
;;
;; init-pmmp (size)
;; ----------------
;; - funkce inicializuje thread pool pro praci se size vlakny
;;
;; set-process-function (id fn)
;; ----------------------------
;; - nastavi to, ze vlakno id bude provadet funkci fn,
;; - fn musi byt funkce, ktera bere jeden argument, id vlakne, ktere ji bude
;;   provadet
;;
;; run-pmmp ()
;; -----------
;; - spusti vypocet
;; - nespusti vlakna, ktera nemaji prirazenu funkci pomoci set-process-function
;; - pri pokusu poslat takto necinemu vlaknu ovsem dojde k vyjimce
;;   (nebo tak neco), takze nedoporucuji to zkouset
;; - vraci seznam threadu (na ktere je pouzitelne napriklad kill-threads
;;
;; Posilani zprav
;; ==============
;;
;;- pomoci makra (with-pmmp (id) body), kde id je idcko procesu a body kod 
;; (interne je to zatim labels) jsou zpristupneny funkce na posilani zprav
;; priklady pouziti v souboru tests.lisp
;;
;; send (receivers type data)
;; --------------------------   
;; - receivers je bud seznam prijemcu zpravy nebo jenom jeden prijemce (tj. idcko)
;; - type je typ (neco jako tag, predp. ze tak je skalarni hodnota)
;; - data jsou posilana data
;; - volani je blokujici (vlakno ceka, dokud si vsichni adresati nevyzvednou
;;   zpravy
;;
;; asend (receivers type data)
;; ----------------------------
;; - viz send, ale neblokujici
;;
;; receive (senders type) => (values data sender message-type)
;; ---------------------
;; - senders je seznam id procesu nebo jedno id procesu
;;   pokud je nil, bere se to jako jakykoliv process
;; - type je tag zpravy, pokud je nil, bere se to jako
;;   jakykoliv type
;; - funkce vyzvedne zpravu od nektereho z danych odesilatelu
;;   s danym tagem
;; - vraci hodnoty: data, id odesilatele, tag-zpravy
;; - blokujici (process ceka, dokud neni vyhovujici zprava
;;   ve schrance  
;;
;; probe (senders type) => generalized boolean
;; --------------------
;; - senders a type viz receive
;; - otestuje, jestli je zprava vyhovujici senders a type
;;   ve schrance
;;
;; broadcast (data type sender) => (values data message-type)
;; ----------------------------
;; - data jsou odesilana data
;; - type je tag
;; - sender je odesilajici process (ostatni mohou mit data nil)
;; - provede blokujici broadcast
;; - vsem krome sendera vraci volani dvojici hodnot data tag
;; - volani je blokujici, funguje jako bariera
;;
;; scatter (data type sender) -> (values data-part message-type)
;; --------------------------
;; - data musi byt jednoduche pole
;; - type je tag
;; - sender je id odesilajici procesu (ostatni mohou mit data nil)
;; - pro funkce "naseka" pole data procesu sender a kousky posle
;;   vsem procesum (vcetne sendera)
;; - vraci dvojic hodnot: kousek pole (jako samostane pole), tag
;; - volani je pro vsechny procesy blokujici (jako broadcast)
;;
;; getter (data-part receiver) => data
;; ----------------------
;; - data-part musi byt jednoduche pole 
;; - receiver id procesu, ktery dostane vsechny kousky
;; - receiver dostane jako navratovou hodnotu pole,
;;   ktere vznikne poskladanim data-part od jednotlivych
;;   procesu (podle id, vzestupne)
;; - volani je pro vsechny procesy blokujici (jako broadcast)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defpackage #:papr4/pmmp
  (:use :cl)
  (:import-from :closer-mop)
  (:export :init-pmmp
	   :run-pmmp
	   :send
	   :asend
	   :receive
	   :probe
	   :broadcast
	   :scatter
	   :getter))
(in-package #:papr4/pmmp)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; message envelope
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass message-envelope ()
  ((sender-id :initarg :sender-id :initform nil)
   (style :initarg :style :initform nil)	 
   (message-type :initarg :message-type)
   (data :initarg :data :initform nil)
   sync-semaphore))

(defmethod initialize-instance :after ((m message-envelope) &key)
  (with-slots (style sync-semaphore) m
    (when (member 'sync style)
      (setf sync-semaphore (make-semaphore 0)))))

(defun forbidden-styles (style-list)
  (intersection style-list '(broadcast-forward
			     broadcast-back
			     broadcast-confirm
			     scatter-forward
			     scatter-back
			     scatter-confirm)))

(defun impl (a b)
  (or (not a) b))
    
(defun style-check (args style-def)
  (and (subsetp args style-def)
       (impl (forbidden-styles style-def) (forbidden-styles args)))) 
	     

(defmethod message-match ((m message-envelope) senders type &optional (style-arg nil))
  (with-slots (sender-id  message-type style)  m
    (and (or (not senders) (member sender-id senders :test #'equalp))
	 (or (not type) (equalp message-type type))
	 (style-check style-arg style))))
          
(defmethod debug-print ((m message-envelope))
  (with-slots (sender-id message-type style) m
    (format t "sender-id ~A message-type ~A style ~A ~%" sender-id message-type style)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Special queue (written in C style)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass queue-list ()
  ((queue :initform nil)
   (tail :initform nil)))

(defmethod enqueue (element (q queue-list))
  (with-slots (queue tail) q
    (if queue
 	(progn
	  (rplacd tail (list element))
	  (setf tail (rest tail)))
	(setf queue (list element)
	      tail queue))))

(defmethod dequeue-if (fn (q queue-list))
  (with-slots (queue tail) q
    (let ((ret nil) (tmp queue))
      (cond
	;; empty queue
	((not queue)
	 (return-from dequeue-if nil))
	;; first element matches
	((funcall fn (first queue))
	 (progn
	   (setf ret (first queue)
		 queue (rest queue))
	   ;; repair tail if last element was removed
	   (when (not queue)
	     (setf tail nil)))))
      ;;first element was not a match
      (loop while (rest tmp) do
	   (when (funcall fn (second tmp))
	     (setf ret (second tmp))
	     (when (eq (rest tmp) tail)
	       (setf tail tmp))
	     (rplacd tmp (rest (rest tmp)))
	     (return))
	   (setf tmp (rest tmp)))
      ret)))

(defmethod queue-find ((q queue-list) fn)
  (with-slots (queue) q
    (not (null (member-if fn queue)))))

(defmethod debug-print ((q queue-list))
  (with-slots (queue) q
    (dolist (a queue)
      (debug-print a))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; mailbox for one process
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass mailbox()
  ((lock :initform (make-mutex 1))
   (message-queue :initform (make-instance 'queue-list))
   (receive-semaphore :initform (make-semaphore 0))
   (waiting-flags :initform nil)))
	    
(defmethod push-to-mailbox ((box mailbox) (message message-envelope))
  (with-slots (lock message-queue receive-semaphore waiting-flags) box
    (with-slots (sender style sync-semaphore message-type) message
      (mutex-wait lock)
      (enqueue message message-queue)
      (let ((blocking (member 'sync style))) 
	(if waiting-flags
	    (progn
	      (sem-signal receive-semaphore)
	      (when blocking
		(sem-wait sync-semaphore)))
	    (progn
	      (mutex-signal lock)
	      (when blocking
		(sem-wait sync-semaphore))))))))

(defmethod pop-from-mailbox((box mailbox) &key senders type style-arg)
  (with-slots (lock message-queue receive-semaphore waiting-flags) box
    (let (ret)
      (mutex-wait lock)
      (setf ret (dequeue-if (lambda (x) (message-match x senders type style-arg))
			    message-queue))
      (loop
	 while (not ret) do
	   (setf waiting-flags t)
	   (mutex-signal lock)
	   (sem-wait receive-semaphore)
	   (setf ret (dequeue-if (lambda (x) (message-match x senders type style-arg))
				 message-queue)))
      (setf waiting-flags nil)
      
      (with-slots (style sync-semaphore) ret
	(when (member 'sync style)
	  (sem-signal sync-semaphore)))
      (mutex-signal lock)
      ret)))

(defmethod probe-mailbox ((box mailbox) &key senders type)
  (with-slots (lock message-queue) box
    (let ((ret nil))
      (mutex-wait lock)
      (setf ret
	    (queue-find message-queue (lambda (x) (message-match x senders type))))
      (mutex-signal lock)
      ret)))
    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; process pool: 
;;
;; TODO: vhodna syntakticka abstrakce? (zabalit do class? bude potreba vice nez jeden pool?)
;;       
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defparameter *pool-size* nil)
(defparameter *mailboxes* nil)
(defparameter *process-functions* nil) ;; jednorozmerne pole


(defun get-all-ids ()
  (loop for i from 0 to (1- *pool-size*)
       collect i))

;; inicializuje pool
(defun init-pmmp (size)
  (setf *pool-size* size)
  (setf *mailboxes* (make-array size :initial-element nil))
  (setf *process-functions* (make-array size :initial-element nil))
  (loop
     for i from 0 to (1- size) do
       (setf (aref *mailboxes* i) (make-instance 'mailbox))))


;; privatni funkce pro komunikaci
;; budou navazany makrem with-pmmp
(defun pmmp-send (id receivers type data)
  (dolist (to receivers)
    (push-to-mailbox (aref *mailboxes* to)
		     (make-instance 'message-envelope
				    :style '(sync)
				    :message-type type
				    :data data
				    :sender-id id))))

(defun pmmp-asend (id receivers type data)
  (dolist (to receivers)
    (push-to-mailbox (aref *mailboxes* to)
		     (make-instance 'message-envelope
				    :message-type type
				    :data data
				    :sender-id id))))

;; returns (values data sender type)
(defun pmmp-receive (id senders type)
  (let ((message (pop-from-mailbox (aref *mailboxes* id) :senders senders :type type)))
    (with-slots (sender-id message-type data) message
      (values data sender-id message-type))))

;; return t nebo nil
(defun pmmp-probe (id senders type)
  (probe-mailbox (aref *mailboxes* id) :senders senders :type type))

;; pitomy broadcast (ne strom, predelat pomoci funkci na potomky a predky ve stromu)
(defun pmmp-broadcast (id data type sender)
  (cond ((= id sender) ;; odesilatel
	 (let ((recs (delete id (get-all-ids))))
	   (dolist (r recs)
	     (push-to-mailbox (aref *mailboxes* r)
			      (make-instance 'message-envelope
					     :style '(broadcast-forward)
					     :message-type type
					     :data (deep-copy data)
					     :sender-id id)))
	   (dolist (r recs)
	     (pop-from-mailbox (aref *mailboxes* id)
			       :senders (list r)
			       :style-arg '(broadcast-back)))
	   (dolist (r recs)
	     (push-to-mailbox (aref *mailboxes* r)
			      (make-instance 'message-envelope
					     :style '(broadcast-confirm)
					     :message-type nil
					     :data nil
					     :sender-id id)))))
	(t ;; prijemce
	 (let ((envelope (pop-from-mailbox (aref *mailboxes* id)
					   :senders (list sender)
					   :style-arg '(broadcast-forward))))

	   ;;(sleep (random 5)) ;; test chovani jako bariera
	   (push-to-mailbox (aref *mailboxes* sender)
			    (make-instance 'message-envelope
					   :style '(broadcast-back)
					   :sender-id id))
	   
	   (pop-from-mailbox (aref *mailboxes* id)
			     :style-arg '(broadcast-confirm)
			     :senders (list sender))
	   
	   (with-slots (message-type data) envelope
	     (values data message-type))))))

;; pitomy scatter data = simple array (mozna sekvence?)


(defun compute-indices (length p)
  (multiple-value-bind (k remainder) (floor length p)
    (nconc
     (loop for i from 0 to (1- remainder) 
	  collect (list (* i (1+ k)) (* (1+ i) (1+ k))))
     (loop for i from 0 to (1- (- p remainder))
	with start = (* remainder (1+ k))
	collect (list (+ start (* i k))
		       (+ start (* (1+ i) k)))))))


(defun pmmp-scatter (id data type sender)
  (cond
    ;; ---- odesilatel -----
    ((= id sender) 
     (let* ((recs (delete id (get-all-ids)))
	    (indices (compute-indices (length data) *pool-size*))
	    (my-indices (nth id indices)))
       ;; smazu my-indices z indices
       (setf indices (nconc (subseq indices 0 id) (nthcdr (1+ id) indices))) 
       ;; rozeslu kousky poli
       (loop for (to (start end)) in (mapcar #'list recs indices) do ;; pres dvojice (kam indexy)
		(push-to-mailbox (aref *mailboxes* to)
				 (make-instance 'message-envelope
						:sender-id id
						;; TODO: je potreba deep copy?
						:data (deep-copy (subseq data start end)) 
						:message-type type
						:style '(scatter-forward))))    
       ;; pockam si na odpovedi		      
       (loop for s in recs do
	    (pop-from-mailbox (aref *mailboxes* id)
			      :style-arg '(scatter-back)
			      :senders (list s)))
       ;; poslu potvrzeni 
       (loop for s in recs do
	    (push-to-mailbox (aref *mailboxes* s)
			     (make-instance 'message-envelope
					      :sender-id id
					      :data nil
					      :message-type nil
					      :style '(scatter-confirm))))
       ;; vratim svoji vlastni cast
       (values (subseq data (first my-indices) (second my-indices)))))
     ;; ---- prijemce ----
     (t
      (let ((envelope (pop-from-mailbox (aref *mailboxes* id)
					:style-arg '(scatter-forward)
					:senders (list sender))))
	;; potvrzeni o prijeti
	(push-to-mailbox (aref *mailboxes* sender)
			    (make-instance 'message-envelope
					   :style '(scatter-back)
					   :sender-id id))
	;; cekam na zpravu o pokracovani
	(pop-from-mailbox (aref *mailboxes* id)
			     :style-arg '(scatter-confirm)
			     :senders (list sender))
	;; vratim data a type ze zpravy
	(with-slots (message-type data) envelope
	  (values data message-type))))))


;; pitomy getter : data = array (mozna sequence, zjistit typ pro concatenate)
;; vynechany type (nedava smysl, co kdyz odesilatele poslou ruzny type?)

(defun pmmp-getter (id data receiver)
  (cond
    ;; ---- prijemce ----
    ((= id receiver)
     (let ((others (delete id (get-all-ids)))
	   ;; dostanu spojene pole
	   (result (apply
		    #'concatenate
		    (nconc
		     (list 'vector)
		     ;; mensi id
		     (mapcar (lambda (x) (slot-value x 'data))
			     (loop
				for s from 0 upto (1- receiver)
				collect (pop-from-mailbox (aref *mailboxes* id)
							  :style-arg '(getter-forward)
							  :senders (list s))))
		     ;; moje data
		     (list data)
		     ;; vetsi id
		     (mapcar (lambda (x) (slot-value x 'data))
			     (loop
				for s from (1+ receiver) upto (1- *pool-size*)
				collect (pop-from-mailbox (aref *mailboxes* id)
							  :style-arg '(getter-forward)
							  :senders (list s))))))))
       ;; poslu potvrzeni
       (loop for r in others do
	    (push-to-mailbox (aref *mailboxes* r)
			     (make-instance 'message-envelope
					    :style '(getter-confirm)
					    :sender-id id)))
       ;; vratim vysledek
       result))
    ;; ---- odesilatel ----
    (t
     (push-to-mailbox (aref *mailboxes* receiver)
		      (make-instance 'message-envelope
				     :data (deep-copy data)
				     :sender-id id
				     :style '(getter-forward)))
     ;; pockam na potvrzeni prijeti a hotovo
     (pop-from-mailbox (aref *mailboxes* id)
		       :style-arg '(getter-confirm)
		       :senders (list receiver)))))
	   
	       
;; spusti vypocet
(defun run-pmmp ()
  (a-dolist (i (loop for i from 0 to (1- *pool-size*) collect i))
    (when (aref *process-functions* i)
      (funcall (aref *process-functions* i)))))

;; fn = funkce, ktera bere 1 parametr a to je id procesu
(defun set-process-function (id fn)
  (setf (aref *process-functions* id) (lambda () (funcall fn id))))


;;;; MAKRO pro definice posilacich operaci

;; (send receivers type data)
;; (asend receivers type data)
;; (receive senders type) => (values data sender message-type)
;; (probe senders type) 
;; (broadcast data type sender) => (values data message-type)
;; (scatter data type sender) -> (values data-part message-type)


(defmacro with-pmmp ((id) &body body)
  `(labels ((send (receivers type data)
	      (pmmp-send ,id
			 (if (listp receivers) receivers (list receivers))
			 type
			 (deep-copy data)))
	    (asend (receivers type data)
	      (pmmp-asend ,id
			  (if (listp receivers) receivers (list receivers))
			  type
			  (deep-copy data)))
	    (receive (senders type)
	      (pmmp-receive ,id
			    (if (listp senders) senders (list senders))
			    type))
	    (probe (senders type)
	      (pmmp-probe ,id
			  (if (listp senders) senders (list senders))
			  type))
	    (broadcast (data type sender)
	      (pmmp-broadcast ,id
			      data
			      type
			      sender))
	    (scatter (data type sender)
	      (pmmp-scatter ,id
			    data
			    type
			    sender))
	    (getter (data receiver)
	      (pmmp-getter ,id
			   data
			   receiver)))
     (declare (ignorable (function send) (function asend) (function receive)
			 (function probe) (function broadcast) (function scatter)
			 (function getter)))
     ,@body))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Deep COPY
;; credit goes to davypough@stackexchange
;; (https://codereview.stackexchange.com/questions/156392/generic-copy-function)
;; Ken Pitman http://www.nhplace.com/kent/PS/EQUAL.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod deep-copy ((object t))
  object)

(defmethod deep-copy ((object null))
  nil)

;; toto muze zlobit (napriklad s NILL, ale to mozna odstrani deep-copy nahore
;(defmethod deep-copy ((object symbol))
; (copy-symbol object))

(defmethod deep-copy ((seq sequence))
  (map (type-of seq) #'deep-copy seq))

(defmethod deep-copy ((arr array))
  (let ((new-arr (make-array (array-dimensions arr)
                             :element-type (array-element-type arr)
                             :adjustable (adjustable-array-p arr))))
    (dotimes (i (array-total-size arr))
      (setf (row-major-aref new-arr i)
        (deep-copy (row-major-aref arr i))))
    new-arr))  

(defmethod deep-copy ((ht hash-table))
  (loop with new-ht = (make-hash-table
                        :test (hash-table-test ht)
                        :size (hash-table-size ht)
                        :rehash-size (hash-table-rehash-size ht)
                        :rehash-threshold (hash-table-rehash-threshold ht))
     for key being the hash-key in ht using (hash-value value)
     do (setf (gethash (deep-copy key) new-ht) (deep-copy value))
     finally (return new-ht)))


;; v sbcl potrebuju sb-mop package

(defmethod deep-copy ((inst standard-object))
  (let ((new-inst (allocate-instance (class-of inst)))
        (slots (class-direct-slots (class-of inst))))
    (dolist (slot slots)
      (let ((slot-name (closer-mop:slot-definition-name slot)))
        (when (slot-boundp inst slot-name)
          (setf (slot-value new-inst slot-name)
		(deep-copy (slot-value inst slot-name))))))
    new-inst))


