(in-package :cl-user)
(defpackage cl-freee.api.account-items
  (:use :cl
        :cl-freee.api)
  (:export :get-account-items
           :get-account-items-detail))
(in-package :cl-freee.api.account-items)

(defgeneric get-account-items (connection &rest args &key company-id base-date)
  (:documentation "指定した事業所の勘定科目一覧を取得する"))

(defgeneric get-account-items-detail (connection id &rest args &key company-id)
  (:documentation "指定した勘定科目の詳細を取得する"))


(defmethod get-account-items ((connection <freee-connection>) &rest args &key company-id base-date)
  (declare (ignore args))
  (let ((uri (quri:make-uri :defaults *API-URI*
                            :path *API-PATH-ACCOUNT-ITEMS*
                            :query `,(loop for (param . value) in `(("company_id" . ,company-id)
                                                                    ("base_date" . ,base-date))
                                          when value
                                            collect `(,param . ,value)))))
    (cl-json:decode-json-from-string
     (request uri connection :method :get))))

(defmethod get-account-items-detail ((connection <freee-connection>) id &rest args &key company-id)
  (declare (ignore args))
  (let ((uri (quri:make-uri :defaults *API-URI*
                            :path (format NIL "~A/~A" *API-PATH-ACCOUNT-ITEMS* id)
                            :query `(("company_id" . ,company-id)))))
    (cl-json:decode-json-from-string
     (request uri connection :method :get))))

