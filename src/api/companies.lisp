(in-package :cl-user)
(defpackage cl-freee.api.companies
  (:use :cl
        :cl-freee.api)
  (:export :get-companies
           :get-companies-detail
           :put-companies))
(in-package :cl-freee.api.companies)

(defgeneric get-comapnies (connection)
  (:documentation "ユーザが所属する事業所の一覧を取得する"))

(defgeneric get-companies-detail (connection &rest args &key id details account-items taxes items partners sections tags walletables)
  (:documentation "ユーザが所属する事業所の詳細を取得する"))

(defgeneric put-companies (connection &rest args &key id content)
  (:documentation "ユーザが所属する事業所の情報を更新する"))


(defmethod get-companies ((connection <freee-connection>))
  (let ((uri (quri:make-uri :defaults *API-URI*
                            :path *API-PATH-COMPANIES*)))
    (cl-json:decode-json-from-string
     (request uri connection :method :get))))

(defmethod get-companies-detail ((connection <freee-connection>) &rest args &key id details account-items taxes items partners sections tags walletables)
  (declare (ignore args))
  (let ((uri (quri:make-uri :defaults *API-URI*
                            :path (format NIL "~A/~A" *API-PATH-COMPANIES* id)
                            :query `,(loop for (param . value) in `(("details" . ,details)
                                                                    ("account_items" . ,account-items)
                                                                    ("taxes" . ,taxes)
                                                                    ("items" . ,items)
                                                                    ("partners" . ,partners)
                                                                    ("sections" . ,sections)
                                                                    ("tags" . ,tags)
                                                                    ("walletables" . ,walletables))
                                           when value
                                             collect `(,param . "true")))))
    (cl-json:decode-json-from-string
     (request uri connection :method :get))))


    
(defmethod put-companies ((connection <freee-connection>) &rest args &key id content)
  (declare (ignore args))
  (let ((uri (quri:make-uri :defaults *API-URI-COMPANIES*
                            :path (format NIL "/~A" id))))
    (cl-json:decode-json-from-string
     (request uri connection :method :get :content content))))

