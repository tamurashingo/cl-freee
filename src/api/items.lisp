(in-package :cl-user)
(defpackage cl-freee.api.items
  (:use :cl
        :cl-freee.api)
  (:export :get-items
           :post-items))
(in-package :cl-freee.api.items)

(defgeneric get-items (connection &rest args &key company-id)
  (:documentation "指定した事業所の品目一覧を取得する"))

(defgeneric post-items (connection &rest args &key content)
  (:documentation "指定した事業所の品目を作成する"))

(defmethod get-items ((connection <freee-connection>) &rest args &key company-id)
  (declare (ignore args))
  (let ((uri (quri:make-uri :defaults *API-URI*
                            :path *API-PATH-ITEMS*
                            :query `(("company_id" . ,company-id)))))
    (cl-json:decode-json-from-string
     (request uri connection :method :get))))

(defmethod post-items ((connection <freee-connection>) &rest args &key content)
  (declare (ignore args))
  (let ((uri  (quri:make-uri :defaults *API-URI*
                             :path *API-PATH-ITEMS*)))
    (cl-json:decode-json-from-string
     (request uri connection :method :post :content content))))
