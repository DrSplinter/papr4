(defparameter *path* 
  (multiple-value-bind (directory successp)
      (capi:prompt-for-directory "Choose papr4 directory:")
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
	(if type
	    (concatenate 'string name "." type)
	    name)
	(car (last dir)))))

(defun directory-files (path)
  (directory (make-pathname :defaults path
			    :type :wild
			    :name :wild)))

(ensure-directories-exist *papr4-path*)
(dolist (source (directory-files *path*))
  (unless (string= (filename source) "quicklisp")
    (let ((target (merge-pathnames (filename source) *papr4-path*)))
      (rename-file source target))))

;; install quicklisp
(load (merge-pathnames "misc/quicklisp.lisp" *papr4-path*))
(quicklisp-quickstart:install :path *ql-path*)

;; upgrade to ASDF 3.3
(let ((cache (merge-pathnames "cache" *ql-path*))
      (target-asdf (merge-pathnames "asdf.lisp" *ql-path*))
      (target-setup (merge-pathnames "setup.lisp" *ql-path*))
      (source-asdf (merge-pathnames "misc/asdf.lisp" *papr4-path*))
      (source-setup (merge-pathnames "misc/setup.lisp" *papr4-path*)))
  (delete-directory cache :recursive t)
  (delete-file target-asdf)
  (delete-file target-setup)
  (rename-file source-asdf target-asdf)
  (rename-file source-setup target-setup)
  (load target-setup))

;; create loading file
(with-open-file (f (merge-pathnames "lispworks-load.lisp" *path*)
		   :direction :output
		   :in-does-not-exist :create
		   :if-exists :supersede)
  (print `(load ,(merge-pathnames "setup.lisp" *ql-path*)) f)
  (print '(ql:quickload :papr4) f)
  (print '(in-package :papr4)))

(delete-file (merge-pathnames "misc/quicklisp.lisp" *papr4-path*))
(delete-file (merge-pathnames "misc/lispworks-install.lisp" *papr4-path*))
