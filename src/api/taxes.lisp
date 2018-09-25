(in-package :cl-user)
(defpackage cl-freee.api.taxes
  (:use :cl
        :cl-freee.api)
  (:export :get-taxes-codes))
(in-package :cl-freee.api.taxes)

(defgeneric get-taxes-codes (connection)
  (:documentation "税区分コード一覧を取得する"))

(defmethod get-taxes-codes ((connection <freee-connection>))
  (let ((uri (quri:make-uri :defaults *API-URI*
                            :path *API-PATH-TAXES-CODES*)))
  (cl-json:decode-json-from-string
   (request uri connection :method :get))))

