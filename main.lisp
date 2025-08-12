(defpackage sharing
  (:use :cl))

(in-package :sharing)

;; Reference 'com.inuoe.jzon' with a shorthand.  
(uiop:add-package-local-nickname '#:jzon '#:com.inuoe.jzon)

;; Define the location where my files should actually end up being stored.
(defvar *storage* "/tmp/")

(defvar *acceptor*
  (make-instance 'hunchentoot:easy-acceptor
		 :port 3000
		 :document-root #p"frontend/dist/"))

(hunchentoot:define-easy-handler (upload :uri "/upload" :default-request-type "POST") ()
  (let* ((request hunchentoot:*request*)
	 (session-token (hunchentoot:header-in :authorization request))
	 (form-data (car (hunchentoot:post-parameters request)))
	 (file-name (caddr form-data))
	 (file-path (cadr form-data))
	 (session-directory (concatenate 'string *storage* session-token "/"))
	 (new-file-path (concatenate 'string session-directory file-name)))
    ;; Ensure that a directory for the session exists and copy the
    ;; file in the directory.
    (ensure-directories-exist session-directory)
    (rename-file file-path new-file-path)
    (format t "POST [~a] ~a~%" session-token new-file-path))
  ;; Return a JSON object as the response.
  (setf (hunchentoot:content-type*) "application/json")
  (let ((response '(:status "success")))
    (jzon:stringify (alexandria:plist-hash-table response))))

;; Why is this locked when I try to name it delete instead of 'clean'?
(hunchentoot:define-easy-handler (clean :uri "/delete") (name)
  (let* ((request hunchentoot:*request*)
	 (session-token (hunchentoot:header-in :authorization request))
	 (session-directory (concatenate 'string *storage* session-token "/"))
	 (file-path (concatenate 'string session-directory name)))
    (delete-file file-path)
    (format t "DELETE [~a] ~a~%" session-token file-path))
  ;; Return a JSON object as the response.
  (setf (hunchentoot:content-type*) "application/json")
  (let ((response '(:status "success")))
    (jzon:stringify (alexandria:plist-hash-table response))))

;; This starts the server always whenever the file is loaded. This
;; should be a dedicated function.
(hunchentoot:start *acceptor*)
