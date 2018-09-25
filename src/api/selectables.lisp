(in-package :cl-user)
(defpackage cl-freee.api.selectables
  (:use :cl
        :cl-freee.api)
  (:export :get-selectables))
(in-package :cl-freee.api.selectables)

(defgeneric get-selectables (connection &rest args &key company-id includes)
  (:documentation "指定した事業所のフォーム用選択項目情報を取得する"))

(defmethod get-selectables ((connection <freee-connection>) &rest args &key company-id includes)
  (declare (ignore args))
  (let ((uri (quri:make-uri :defaults *API-URI*
                            :path *API-PATH-SELECTABLES*
                            :query `,(loop for (param . value) in `(("company_id" . ,company-id)
                                                                    ("includes" . ,includes))
                                           when value
                                             collect `(,param . ,value)))))
    (cl-json:decode-json-from-string
     (request uri connection :method :get))))
