(defpackage #:lispworks-install
  (:use :cl)
  (:export :cleanup))
(in-package #:lispworks-install)

(defparameter *path* 
  (multiple-value-bind (directory successp)
      (capi:prompt-for-directory "Please enter papr4 directory:")
    (if successp
	directory
	(error "Error while trying to install papr4 library."))))
(defparameter *ql-path* (merge-pathnames "quicklisp/" *path*))
(defparameter *papr4-path* (merge-pathnames "local-projects/papr4" *ql-path*))

;; make directory structure
(defun filename (path)
  (let ((name (pathname-name path))
	(type (pathname-type path))
	(dir (pathname-directory path)))
    (if (stringp name)
	(if (stringp type)
	    (concatenate 'string name "." type)
	    name)
	(car (last dir)))))

(defun directory-files (path)
  (directory (make-pathname :defaults path
			    :type :wild
			    :name :wild)))

(ensure-directories-exist *papr4-path*)
(dolist (source (directory-files *path*))
  (unless (or (string= (filename source) "quicklisp")
	      (string= (filename source) "misc"))
    (let ((target (merge-pathnames (filename source) *papr4-path*)))
      (rename-file source target))))

;; install quicklisp
(load (merge-pathnames "misc/quicklisp.lisp" *path*))
(quicklisp-quickstart:install :path *ql-path*)

(defun delete-file* (path)
  (if (or (stringp (pathname-name path))
	  (stringp (pathname-type path)))
      (delete-file path)
      (dolist (f (directory-files path) (lw:delete-directory path))
	(delete-file* f))))

;; upgrade to ASDF 3.3
(let ((cache (merge-pathnames "cache/" *ql-path*))
      (target-asdf (merge-pathnames "asdf.lisp" *ql-path*))
      (target-setup (merge-pathnames "setup.lisp" *ql-path*))
      (source-asdf (merge-pathnames "misc/asdf.lisp" *path*))
      (source-setup (merge-pathnames "misc/setup.lisp" *path*)))
  (delete-file* cache)
  (delete-file target-asdf)
  (delete-file target-setup)
  (rename-file source-asdf target-asdf)
  (rename-file source-setup target-setup)
  (load target-setup))

;; create loading file
(let ((load-file (merge-pathnames "lispworks-load.lisp" *path*)))
  (with-open-file (f load-file
		     :direction :output
		     :if-does-not-exist :create
		     :if-exists :supersede)
    (print `(load ,(merge-pathnames "setup.lisp" *ql-path*)) f)
    (print '(ql:quickload :papr4) f))
  load-file)

(defun cleanup ()
  (delete-directory (merge-pathnames "misc/" *path*) :recursive t))


(format t "
================ papr4 lispworks install complete ==============

 To finish installation, evaluate:  (lispworks-install:cleanup)

----------------------------------------------------------------

 Now everytime you want to work with papr4 functionality load
    ~A
 and then evaluate (in-package :papr4).

================================================================
" (merge-pathnames "lispworks-load.lisp" *path*))
