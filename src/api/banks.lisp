(in-package :cl-user)
(defpackage cl-freee.api.banks
  (:use :cl
        :cl-freee.api)
  (:export :get-banks))
(in-package :cl-freee.api.banks)

(defgeneric get-banks (connection &rest args &key offset limit type)
  (:documentation "連携しているサービス一覧を取得する"))

(defmethod get-banks ((connection <freee-connection>) &rest args &key offset limit type)
  (declare (ignore args))
  (let ((uri (quri:make-uri :defaults *API-URI*
                            :path *API-PATH-BANKS*
                            :query `,(loop for (param . value) in `(("offset" . ,offset)
                                                                    ("limit" . ,limit)
                                                                    ("type" . ,type))
                                           when value
                                             collect `(,param . ,value)))))
    (cl-json:decode-json-from-string
     (request uri connection :method :get))))
