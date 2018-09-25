(in-package :cl-user)
(defpackage cl-freee.api.users
  (:use :cl
        :cl-freee.api)
  (:export :get-users-me
           :get-users-capabilities))
(in-package :cl-freee.api.users)

(defgeneric get-users-me (connection &rest args &key companies)
  (:documentation "ユーザの情報を取得する"))

(defgeneric get-users-capabilities (connection &rest args &key company-id)
  (:documentation "ユーザの権限情報を取得する"))

(defmethod get-users-me ((connection <freee-connection>) &rest args &key companies)
  (declare (ignore args))
  (let ((uri (quri:make-uri :defaults *API-URI*
                            :path *API-PATH-USERS-ME*
                            :query (when companies
                                     `(("companies" . ,companies))))))
    (cl-json:decode-json-from-string
     (request uri connection :method :get))))

(defmethod get-users-capabilities ((connection <freee-connection>) &rest args &key company-id)
  (declare (ignore args))
  (let ((uri (quri:make-uri :defaults *API-URI*
                            :path *API-PATH-USERS-CAPABILITIES*
                            :query `(("company_id" . ,company-id)))))
    (cl-json:decode-json-from-string
     (request uri connection :method :get))))
