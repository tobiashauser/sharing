(defpackage sharing
  (:use :cl))

(in-package :sharing)

;; Reference 'com.inuoe.jzon' with a shorthand.  
(uiop:add-package-local-nickname '#:jzon '#:com.inuoe.jzon)

;; Define the location where my files should actually end up being stored.
(defvar session-directory "/tmp/sharing/")

(defvar *acceptor*
  (make-instance 'hunchentoot:easy-acceptor
		 :port 3000
		 :document-root #p"frontend/dist/"))

;;; Helpers

(defun session-token (request)
  (hunchentoot:header-in :session request))

(defun form-data (request)
  (car (hunchentoot:post-parameters request)))

(defun session-dir (request)
  (concatenate 'string session-directory (session-token request) "/"))

;; This includes all intermediary folders.
(defun file-name (request)
  (caddr (form-data request)))

;; This is the location where hunchentoot stores the file initially.
(defun incoming-location (request)
  (cadr (form-data request)))

;; This is the location where we move the files to before we zip them up.
(defun session-location (request &optional path)
  (concatenate 'string
	       (session-dir request)
	       (or path (file-name request))))

(defun response (&optional body)
  (jzon:stringify (alexandria:plist-hash-table body)))

;;; API

(hunchentoot:define-easy-handler (upload :uri "/upload" :default-request-type "POST") ()
  (let ((request hunchentoot:*request*))
    ;; Ensure the session directory exists.
    (ensure-directories-exist (session-dir request))
    (ensure-directories-exist (session-location request))
    ;; Then we move the file.
    (rename-file (incoming-location request)
		 (session-location request))
    ;; Some logging.
    (hunchentoot:log-message* (session-token request)
			      "POST ~a"
			      (file-name request))))

;; (hunchentoot:define-easy-handler (upload :uri "/upload" :default-request-type "POST") ()
;;   (let* ((request hunchentoot:*request*)
;; 	 (session-token (hunchentoot:header-in :session request))
;; 	 (form-data (car (hunchentoot:post-parameters request)))
;; 	 (file-name (caddr form-data))
;; 	 (file-path (cadr form-data))
;; 	 (session-directory (concatenate 'string *storage* session-token "/"))
;; 	 (new-file-path (concatenate 'string session-directory file-name)))
;;     ;; Ensure that a directory for the session exists and copy the
;;     ;; file in the directory.
;;     (ensure-directories-exist session-directory)
;;     (rename-file file-path new-file-path)
;;     (format t "POST [~a] ~a~%" session-token new-file-path))
;;   ;; Return a JSON object as the response.
;;   (setf (hunchentoot:content-type*) "application/json")
;;   (let ((response '(:status "success")))
;;     (jzon:stringify (alexandria:plist-hash-table response))))

;; Why is this locked when I try to name it delete instead of 'clean'?
(hunchentoot:define-easy-handler (clean :uri "/clean") (path)
  (let* ((request hunchentoot:*request*)
	 (session-location (session-location request path)))
    (if (uiop:file-exists-p session-location)
	(delete-file session-location)
	(uiop:delete-directory-tree (uiop:ensure-directory-pathname session-location)
				    :validate t
				    :if-does-not-exist :ignore))
    ;; Some logging.
    (hunchentoot:log-message* (session-token request)
			      "CLEAN ~a"
			      (uiop:file-exists-p session-location))))

;; (hunchentoot:define-easy-handler (clean :uri "/delete") (name)
;;   (let* ((request hunchentoot:*request*)
;; 	 (session-token (hunchentoot:header-in :session request))
;; 	 (session-directory (concatenate 'string *storage* session-token "/"))
;; 	 (file-path (concatenate 'string session-directory name)))
;;     (delete-file file-path)
;;     (format t "DELETE [~a] ~a~%" session-token file-path))
;;   ;; Return a JSON object as the response.
;;   (setf (hunchentoot:content-type*) "application/json")
;;   (let ((response '(:status "success")))
;;     (jzon:stringify (alexandria:plist-hash-table response))))

;; This starts the server always whenever the file is loaded. This
;; should be a dedicated function.
(hunchentoot:start *acceptor*)
