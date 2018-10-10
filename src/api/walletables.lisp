(in-package :cl-user)
(defpackage cl-freee.api.walletables
  (:use :cl
        :cl-freee.api)
  (:export :get-walletables))
(in-package :cl-freee.api.walletables)

(defgeneric get-walletables (connection &rest args &key company-id with-balance)
  (:documentation "指定した事業所の口座一覧を取得する"))

(defgeneric post-walletables (connection &rest args &key content)
  (:documentation "指定した事業所に口座を作成する"))

(defmethod get-walletables ((connection <freee-connection>) &rest args &key company-id with-balance)
  (declare (ignore args))
  (let ((uri (quri:make-uri :defaults *API-URI*
                            :path *API-PATH-WALLETABLES*
                            :query `,(loop for (param . value) in `(("company_id" . ,company-id)
                                                                    ("with_balance" . ,with-balance))
                                           when value
                                             collect `(,param . ,value)))))
    (cl-json:decode-json-from-string
     (request uri connection :method :get))))

(defmethod post-walletables ((connection <freee-connection>) &rest args &key content)
  (declare (ignore args))
  (let ((uri (quri:make-uri :defaults *API-URI*
                            :path *API-PATH-WALLETABLES*)))
    (cl-json:decode-json-from-string
     (request uri connection :method :post :content content))))
