(defpackage sharing
  (:use :cl))

(in-package :sharing)

(defvar *acceptor*
  (make-instance 'hunchentoot:easy-acceptor
		 :port 3000
		 :document-root #p"frontend/dist/"))

(hunchentoot:start *acceptor*)
(hunchentoot:stop *acceptor*)
