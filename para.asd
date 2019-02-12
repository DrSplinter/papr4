#|
 This file is a part of para
 (c) 2019 Petr Osička (petr.osicka@upol.cz)
 Author: Petr Osička <petr.osicka@upol.cz>
|#

(in-package #:cl-user)
(asdf:defsystem #:para
  :version "0.2"
  :license "MIT"
  :author "Petr Osička <petr.osicka@upol.cz>"
  :maintainer "Jan Tříska <triskaj@centrum.cz>"
  :description "Implementation of primitives for solving tasks in course KMI/PARA at UPOL\."
  :serial T
  :components ((:file "para")
               (:file "stmx-stuff"))
  :depends-on (#:stmx #:bt-semaphore #:bordeaux-threads))
