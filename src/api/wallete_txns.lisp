(in-package :cl-user)
(defpackage cl-freee.api.wallete-txns
  (:use :cl
        :cl-freee.api)
  (:export :get-wallete-txns
           :post-wallete-txns))
(in-package :cl-freee.api.wallete-txns)

(defgeneric get-wallete-txns (connection &rest args &key company-id walletable-type walletable-id start-date end-date entry-side offset limit)
  (:documentation "指定した事業所の明細一覧を取得する"))

(defgeneric post-wallete-txns (connection &rest args &key content)
  (:documentation "指定した事業所の明細を作成する"))

(defmethod get-wallete-txns ((connection <freee-connection>) &rest args &key company-id walletable-type walletable-id start-date end-date entry-side offset limit)
  (declare (ignore args))
  (let ((uri (quri:make-uri :defaults *API-URI*
                            :path *API-PATH-WALLETE-TXNS*
                            :query `,(loop for (param . value) in `(("company_id" . ,company-id)
                                                                    ("walletable_type" . ,walletable-type)
                                                                    ("walletable_id" . ,walletable-id)
                                                                    ("start_date" . ,start-date)
                                                                    ("end_date" . ,end-date)
                                                                    ("entry_side" . ,entry-side)
                                                                    ("offset" . ,offset)
                                                                    ("limit" . ,limit))
                                           when value
                                             collect `(,param . ,value)))))
    (cl-json:decode-json-from-string
     (request uri connection :method :get))))

(defmethod post-wallete-txns ((connection <freee-connection>) &rest args &key content)
  (declare (ignore args))
  (let ((uri (quri:make-uri :defaults *API-URI*
                            :path *API-PATH-WALLETE-TXNS*)))
    (cl-json:decode-json-from-string
     (request uri connection :method :post :content content))))
