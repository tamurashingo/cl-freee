(in-package :cl-user)
(defpackage cl-freee.api.partners
  (:use :cl
        :cl-freee.api)
  (:export :get-partners
           :post-partners))
(in-package :cl-freee.api.partners)

(defgeneric get-partners (connection &rest args &key company-id offset limit)
  (:documentation "指定した事業所の取引先一覧を取得する"))

(defgeneric post-partners (connection &rest args &key content)
  (:documentation "指定した事業所の取引先を作成する"))

(defmethod get-partners ((connection <freee-connection>) &rest args &key company-id offset limit)
  (declare (ignore args))
  (let ((uri (quri:make-uri :defaults *API-URI*
                            :path *API-PATH-PARTNERS*
                            :query `,(loop for (param . value) in `(("company_id" . ,company-id)
                                                                    ("offset" . ,offset)
                                                                    ("limit" . ,limit))
                                           when value
                                             collect `(,param . ,value)))))
    (cl-json:decode-json-from-string
     (request uri connection :method :get))))

(defmethod post-partners ((connection <freee-connection>) &rest args &key content)
  (declare (ignore args))
  (let ((uri (quri:make-uri :defaults *API-URI*
                            :path *API-PATH-PARTNERS*)))
    (cl-json:decode-json-from-string
     (request uri connection :method :post :content content))))
