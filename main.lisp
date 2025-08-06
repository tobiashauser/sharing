(defpackage sharing
  (:use :cl))

(in-package :sharing)

;; Reference 'com.inuoe.jzon' with a shorthand.  
(uiop:add-package-local-nickname '#:jzon '#:com.inuoe.jzon)

(defvar *acceptor*
  (make-instance 'hunchentoot:easy-acceptor
		 :port 5173
		 :document-root #p"frontend/dist/"))

(hunchentoot:define-easy-handler (session-token :uri "/api/session-token") ()
  (let ((response '(:session-token "my-random-session-token")))
    (jzon:stringify (alexandria:plist-hash-table response))))

(hunchentoot:define-easy-handler (upload :uri "/upload") ()
  (let* ((request hunchentoot:*request*)
	 (form-data (hunchentoot:post-parameters request))
	 (auth-header (hunchentoot:header-in :authorization request)))
    (format t "==> Received uploaded file: ~a~% in session ~a~%" form-data auth-header))
  ;; (rename-file (car uploaded)
  ;;              (concatenate 'string "/tmp/"
  ;; 			    (cl-base64:string-to-base64-string (cadr uploaded))))
  ;; Return a JSON object as the response.
  (setf (hunchentoot:content-type*) "application/json")
  (let ((response '(:message "Hello, world!" :status "success")))
    (jzon:stringify (alexandria:plist-hash-table response))))

(hunchentoot:start *acceptor*)
(hunchentoot:stop *acceptor*)
