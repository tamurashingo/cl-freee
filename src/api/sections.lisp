(in-package :cl-user)
(defpackage cl-freee.api.sections
  (:use :cl
        :cl-freee.api)
  (:export :get-sections))
(in-package :cl-freee.api.sections)

(defgeneric get-sections (connection &rest args &key company-id)
  (:documentation "指定した事業所の部門一覧を取得する"))

(defmethod get-sections ((connection <freee-connection>) &rest args &key company-id)
  (declare (ignore args))
  (let ((uri (quri:make-uri :defaults *API-URI*
                            :path *API-PATH-SECTIONS*
                            :query `(("company_id" . ,company-id)))))
    (cl-json:decode-json-from-string
     (request uri connection :method :get))))
