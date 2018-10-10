(in-package :cl-user)
(defpackage cl-freee.api.manual-journals
  (:use :cl
        :cl-freee.api)
  (:export :get-manual-journals
           :get-manual-journals-detail
           :post-manual-journals))
(in-package :cl-freee.api.manual-journals)

(defgeneric get-manual-journals (connection &rest args &key company-id start-issue-date end-issue-date entry-side account-item-id min-amount max-amount partner-id item-id section-id comment-status comment-important adjustment txn-number offset limit)
  (:documentation "指定した事業所の振替伝票一覧を取得する"))

(defmethod get-manual-journals-detail (connection id &rest args &key company-id)
  (:documentation "指定した事業所の振替伝票を取得する"))

(defmethod post-manual-journals (connection &rest args &key content)
  (:documentation "指定した事業所の振替伝票を作成する"))

(defmethod get-manual-journals ((connection <freee-connection>) &rest args &key company-id start-issue-date end-issue-date entry-side account-item-id min-amount max-amount partner-id item-id section-id comment-status comment-important adjustment txn-number offset limit)
  (declare (ignore args))
  (let ((uri (quri:make-uri :defaults *API-URI*
                            :path *API-PATH-MANUAL-JOURNALS*
                            :query `,(loop for (param . value) in `(("company_id" . ,company-id)
                                                                    ("start_issue_date" . ,start-issue-date)
                                                                    ("end_issue_date" . ,end-issue-date)
                                                                    ("entry_side" . ,entry-side)
                                                                    ("account_item_id" . ,account-item-id)
                                                                    ("min_amount" . ,min-amount)
                                                                    ("max_amount" . ,max-amount)
                                                                    ("partner_id" . ,partner-id)
                                                                    ("item_id" . ,item-id)
                                                                    ("section_id" . ,section-id)
                                                                    ("comment_status" . ,comment-status)
                                                                    ("comment_important" . ,comment-important)
                                                                    ("adjustment" . ,adjustment)
                                                                    ("txn_number" . ,txn-number)
                                                                    ("offset" . ,offset)
                                                                    ("limit" . ,limit))
                                           when value
                                             collect `(,param . ,value)))))
        (cl-json:decode-json-from-string
     (request uri connection :method :get))))

(defmethod get-manual-journals-detail ((connection <freee-connection>) id &rest args &key company-id)
  (declare (ignore args))
  (let ((uri (quri:make-uri :defaults *API-URI*
                            :path (format NIL "~A/~A" *API-PATH-MANUAL-JOURNALS* id)
                            :query `(("company_id" . ,company-id)))))
    (cl-json:decode-json-from-string
     (request uri connection :method :get))))

(defmethod post-manual-journals ((connection <freee-connection>) &rest args &key content)
  (declare (ignore args))
  (let ((uri (quri:make-uri :defaults *API-URI*
                            :path *API-PATH-MANUAL-JOURNALS*)))
    (cl-json:decode-json-from-string
     (request uri connection :method :post :content content))))
