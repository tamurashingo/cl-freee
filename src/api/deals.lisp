(in-package :cl-user)
(defpackage cl-freee.api.deals
  (:use :cl
        :cl-freee.api)
  (:export :get-deals
           :get-deals-detail))
(in-package :cl-freee.api.deals)

(defgeneric get-deals (connection &rest args &key company-id partner-id status type start-issue-date end-issue-date start-due-date end-due-date offset limit registered-from)
  (:documentation "指定した事業所の取引一覧（収入／支出）を取得する"))

(defgeneric get-deals-detail (connection id &rest args &key company-id)
  (:documentation "指定した事業所の取引（収入／支出）を取得する"))


(defmethod get-deals ((connection <freee-connection>) &rest args &key company-id partner-id status type start-issue-date end-issue-date start-due-date end-due-date offset limit registered-from)
  (declare (ignore args))
  (let ((uri (quri:make-uri :defaults *API-URI*
                            :path *API-PATH-DEALS*
                            :query `,(loop for (param . value) in `(("company_id" . ,company-id)
                                                                    ("partner_id" . ,partner-id)
                                                                    ("status" . ,status)
                                                                    ("type" . ,type)
                                                                    ("start_issue_date" . ,start-issue-date)
                                                                    ("end_issue_date" . ,end-issue-date)
                                                                    ("start_due_date" . ,start-due-date)
                                                                    ("end_due_date" . ,end-due-date)
                                                                    ("offset" . ,offset)
                                                                    ("limit" . ,limit)
                                                                    ("registered_from" . ,registered-from))
                                           when value
                                             collect `(,param . ,value)))))
    (cl-json:decode-json-from-string
     (request uri connection :method :get))))

(defmethod get-deals-detail ((connection <freee-connection>) id &rest args &key company-id)
  (declare (ignore args))
  (let ((uri (quri:make-uri :defaults *API-URI*
                            :path (format NIL "~A/~A" *API-PATH-DEALS* id)
                            :query `(("company_id" . ,company-id)))))
    (cl-json:decode-json-from-string
     (request uri connection :method :get))))
