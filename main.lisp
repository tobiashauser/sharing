(defpackage sharing
  (:use :cl))

(in-package :sharing)

(defvar *acceptor*
  (make-instance 'hunchentoot:easy-acceptor
		 :port 3000
		 :document-root #p"frontend/dist/"))

(hunchentoot:define-easy-handler (upload :uri "/upload") (uploaded)
  (format t "==> Received uploaded file: ~a~%" (car uploaded))
  ;; (rename-file (car uploaded)
  ;;              (concatenate 'string "/tmp/"
  ;; 			    (cl-base64:string-to-base64-string (cadr uploaded))))
  "SUCCESS")

(hunchentoot:start *acceptor*)
(hunchentoot:stop *acceptor*)
