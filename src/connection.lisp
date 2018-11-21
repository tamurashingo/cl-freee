(in-package :cl-user)
(defpackage cl-freee.connection
  (:use :cl)
  (:export :<freee-connection>
           :access-token
           :authorize
           :make-connection
           :refresh
           :*PROXY*
           :*API-DEBUG*))
  
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
  "https://secure.freee.co.jp/oauth/token")

(defparameter *PROXY* NIL)

(defparameter *API-DEBUG* NIL)

(defgeneric authorize (connection authorization-code)
  (:documentation "authorization-codeを受け取り、アクセストークン、リフレッシュトークンを取得する"))

(defgeneric refresh (connection)
  (:documentation "トークンのリフレッシュを行う"))


(defun make-connection (&key client-id client-secret redirect-uri access-token refresh-token callback)
  (when (not (and client-id client-secret redirect-uri))
    (error "client-id, client-secret, redirect-uri are required"))
  (let ((callback (if callback
                      callback
                     #'(lambda (access-token refresh-token)
                         (declare (ignore access-token refresh-token))))))
    (make-instance '<freee-connection>
                   :client-id client-id
                   :client-secret client-secret
                   :redirect-uri redirect-uri
                   :access-token access-token
                   :refresh-token refresh-token
                   :callback callback)))

(defmethod authorize ((connection <freee-connection>) authorization-code)
  (loop for x in (authorize-1 connection authorization-code)
        if (eq (car x) :ACCESS--TOKEN)
          do (setf (access-token connection) (cdr x))
        else if (eq (car x) :REFRESH--TOKEN)
          do (setf (refresh-token connection) (cdr x))
        end)
  (funcall (callback connection) (access-token connection) (refresh-token connection))
  connection)

(defun authorize-1 (connection authorization-code)
  (cl-json:decode-json-from-string
   (dex:post *TOKEN-URI*
             :content `(("grant_type" . "authorization_code")
                        ("client_id" . ,(client-id connection))
                        ("client_secret" . ,(client-secret connection))
                        ("redirect_uri" . ,(redirect-uri connection))
                        ("code" . ,authorization-code))
             :proxy *PROXY*
             :verbose *API-DEBUG*)))

(defmethod refresh ((connection <freee-connection>))
  (loop for x in (refresh-1 connection)
        if (eq (car x) :ACCESS--TOKEN)
          do (setf (access-token connection) (cdr x))
        else if (eq (car x) :REFRESH--TOKEN)
          do (setf (refresh-token connection) (cdr x))
        end)
  (funcall (callback connection) (access-token connection) (refresh-token connection))
  connection)

(defun refresh-1 (connection)
  (cl-json:decode-json-from-string
   (dex:post *TOKEN-URI*
             :content `(("grant_type" . "refresh_token")
                        ("client_id" . ,(client-id connection))
                        ("client_secret" . ,(client-secret connection))
                        ("refresh_token" . ,(refresh-token connection)))
             :proxy *PROXY*
             :verbose *API-DEBUG*)))
