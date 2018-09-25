(in-package :cl-user)
(defpackage cl-freee.api.tags
  (:use :cl
        :cl-freee.api)
  (:export :get-tags))
(in-package :cl-freee.api.tags)

(defgeneric get-tags (connection &rest args &key company-id)
  (:documentation "指定した事業所のメモタグ一覧を取得する"))

(defmethod get-tags ((connection <freee-connection>) &rest args &key company-id)
  (declare (ignore args))
  (let ((uri (quri:make-uri :defaults *API-URI*
                            :path *API-PATH-TAGS*
                            :query `(("company_id" . ,company-id)))))
    (cl-json:decode-json-from-string
     (request uri connection :method :get))))

