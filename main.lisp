(defpackage sharing
  (:use :cl))

(in-package :sharing)

;; Reference 'com.inuoe.jzon' with a shorthand.  
(uiop:add-package-local-nickname '#:jzon '#:com.inuoe.jzon)

(defvar *acceptor*
  (make-instance 'hunchentoot:easy-acceptor
		 :port 5173
		 :document-root #p"frontend/dist/"))

(hunchentoot:define-easy-handler (upload :uri "/upload") ()
  (let ((form-data (hunchentoot:post-parameters hunchentoot:*request*)))
    (format t "==> Received uploaded file: ~a~%" form-data))
  ;; (rename-file (car uploaded)
  ;;              (concatenate 'string "/tmp/"
  ;; 			    (cl-base64:string-to-base64-string (cadr uploaded))))
  ;; Return a JSON object as the response.
  (setf (hunchentoot:content-type*) "application/json")
  (let ((response '(:message "Hello, world!" :status "success")))
    (jzon:stringify (alexandria:plist-hash-table response))))

(hunchentoot:start *acceptor*)
(hunchentoot:stop *acceptor*)
