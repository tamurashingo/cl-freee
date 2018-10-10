(in-package :cl-user)
(defpackage cl-freee.api.transfers
  (:use :cl
        :cl-freee.api)
  (:export :get-transfers))
(in-package :cl-freee.api.transfers)

(defgeneric get-transfers (connection &rest args &key company-id start-date end-date offset limit)
  (:documentation "指定した事業所の取引（振替）一覧を取得する"))

(defgeneric post-transfers (connection &rest args &key content)
  (:documentation "指定した事業所の取引（振替）を作成する"))

(defmethod get-transfers ((connection <freee-connection>) &rest args &key company-id start-date end-date offset limit)
  (declare (ignore args))
  (let ((uri (quri:make-uri :defaults *API-URI*
                            :path *API-PATH-TRANSFERS*
                            :query `,(loop for (param . value) in `(("company_id" . ,company-id)
                                                                    ("start_date" . ,start-date)
                                                                    ("end_date" . ,end-date)
                                                                    ("offset" . ,offset)
                                                                    ("limit" . ,limit))
                                           when value
                                             collect `(,param . ,value)))))
    (cl-json:decode-json-from-string
     (request uri connection :method :get))))

(defmethod post-transfers ((connection <freee-connection>) &rest args &key content)
  (declare (ignore args))
  (let ((uri (quri:make-uri :defaults *API-URI*
                            :path *API-PATH-TRANSFERS*)))
    (cl-json:decode-json-from-string
     (request uri connection :method :post :content content))))
