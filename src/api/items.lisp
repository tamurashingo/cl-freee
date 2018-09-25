(in-package :cl-user)
(defpackage cl-freee.api.items
  (:use :cl
        :cl-freee.api)
  (:export :get-items))
(in-package :cl-freee.api.items)

(defgeneric get-items (connection &rest args &key company-id)
  (:documentation "指定した事業所の品目一覧を取得する"))

(defmethod get-items ((connection <freee-connection>) &rest args &key company-id)
  (declare (ignore args))
  (let ((uri (quri:make-uri :defaults *API-URI*
                            :path *API-PATH-ITEMS*
                            :query `(("company_id" . ,company-id)))))
    (cl-json:decode-json-from-string
     (request uri connection :method :get))))
