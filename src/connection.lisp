(in-package :cl-user)
(defpackage cl-freee.connection
  (:use :cl))
(in-package :cl-freee.connection)

(defclass <freee-connection> ()
  ((client-id :initarg :client-id
              :accessor client-id
              :initform NIL)
   (client-secret :initarg :client-secret
                  :accessor client-secret
                  :initform NIL)
   (redirect-uri :initarg :redirect-uri
                 :accessor redirect-uri
                 :initform NIL)
   (access-token :initarg :access-token
                 :accessor access-token
                 :initform NIL)
   (refresh-token :initarg :refresh-token
                  :accessor refresh-token
                  :initform NIL)
   (callback :initarg :callback
             :accessor callback
             :initform #'(lambda (access-token refresh-token) NIL))))

(defparameter *TOKEN-URI*
  "https://api.freee.co.jp/oauth/token")

(defgeneric authorize (connection authorization-code)
  (:documentation ""))

(defgeneric refresh (connection)
  (:documentation ""))


(defun make-connection (&key client-id client-secret redirect-uri access-token refresh-token callback)
  (when (not (and client-id client-secret redirect-uri))
    (error "client-id, client-secret, redirect-uri are required"))
  (let ((callback (if callback
                     #'(lambda (access-token refresh-token)
                         (declare (ignore access-token refresh-token)))
                     callback)))
    (make-instance '<freee-connection>
                   :client-id client-id
                   :client-secret client-secret
                   :redirect-uri redirect-uri
                   :access-token access-token
                   :refresh-token refresh-token
                   :callback callback)))

(defmethod authorize ((connection <freee-connection>) authorization-code)
  (loop for x in (authorize-1 connection authorization-code)
        when (consp x)
          if (string= (car x) "access_token")
            do (setf (access-token connection) (cdr x))
          else if (string= (car x) "refresh_token")
            do (setf (refresh-token connection) (cdr x))
          end)
  (funcall (callback connection) (access-token connection) (refresh-token connection))
  connection)

(defun authorize-1 (connection authorization-code)
  (jsown:parse
   (dex:post *TOKEN-URI*
             :content `(("grant_type" . "authorization_code")
                        ("client_id" . ,(client-id connection))
                        ("client_secret" . ,(client-secret connection))
                        ("redirect_uri" . ,(redirect-uri connection))
                        ("code" . ,authorization-code)))))

(defmethod refresh ((connection <freee-connection>))
  (loop for x in (refresh-1 connection)
        when (consp x)
          if (string= (car x) "access_token")
            do (setf (access-token connection) (cdr x))
          else if (string= (car x) "refresh_token")
            do (setf (refresh-token connection) (cdr x))
          end)
  (funcall (callback connection) (access-token connection) (refresh-token connection))
  connection)


(defun refresh-1 (connection)
  (jsown:parse
   (dex:post "https://api.freee.co.jp/oauth/token"
             :content `(("grant_type" . "refresh_token")
                        ("client_id" . ,(client-id connection))
                        ("client_secret" . ,(client-secret connection))
                        ("refresh_token" . ,(refresh-token connection))))))


