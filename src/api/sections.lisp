(in-package :cl-user)
(defpackage cl-freee.api.sections
  (:use :cl
        :cl-freee.api)
  (:export :get-sections
           :post-sections))
(in-package :cl-freee.api.sections)

(defgeneric get-sections (connection &rest args &key company-id)
  (:documentation "指定した事業所の部門一覧を取得する"))

(defgeneric post-sections (connection &rest args &key content)
  (:documentation "指定した事業所の部門を作成する"))

(defmethod get-sections ((connection <freee-connection>) &rest args &key company-id)
  (declare (ignore args))
  (let ((uri (quri:make-uri :defaults *API-URI*
                            :path *API-PATH-SECTIONS*
                            :query `(("company_id" . ,company-id)))))
    (cl-json:decode-json-from-string
     (request uri connection :method :get))))

(defmethod post-sections ((connection <freee-connection>) &rest args &key content)
  (declare (ignore args))
  (let ((uri (quri:make-uri :defaults *API-URI*
                            :path *API-PATH-SECTIONS*)))
    (cl-json:decode-json-from-string
     (request uri connection :method :post :content content))))
