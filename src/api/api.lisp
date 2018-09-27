(in-package :cl-user)
(defpackage cl-freee.api
  (:use :cl)
  (:import-from :cl-freee.connection
                :<freee-connection>
                :access-token
                :refresh
                :*PROXY*
                :*API-DEBUG*)
  (:export :<freee-connection>
           :<freee-connection-company>
           :*API-URI*
           :request))
(in-package :cl-freee.api)


;;; TODO: companyの指定が面倒な場合用に用意
(defclass <freee-connection-company> (<freee-connection>)
  ((company :initarg :company
            :accessor company
            :initform NIL)))

(defparameter *API-URI*
  "https://api.freee.co.jp")

;;; APIの変数名とパスの定義
;;; *API-PATH-ACCOUNT-ITEMS* -> "/api/1/account_items"
(defmacro define-path (name path)
  (let ((var-name (intern (format NIL "*API-PATH-~A*" name))))
    `(progn
       (export ',var-name)
       (defparameter ,var-name ,(format NIL "~A~A" "/api/1" path)))))

#.`(progn
     ,@(loop for (name . path) in '((ACCOUNT-ITEMS . "/account_items")
                                    (BANKS . "/banks")
                                    (COMPANIES . "/companies")
                                    (DEALS . "/deals")
                                    (ITEMS . "/items")
                                    (JOURNALS . "/journals")
                                    (MANUAL-JOURNALS . "/manual_journals")
                                    (PARTNERS . "/partners")
                                    (SECTIONS . "/sections")
                                    (SELECTABLES . "/forms/selectables")
                                    (TAGS . "/tags")
                                    ;; (TAXES . "/taxes") ; deprecated
                                    (TAXES-CODES . "/taxes/codes")
                                    (TRANSFERS . "/transfers")
                                    (USERS-ME . "/users/me")
                                    (USERS-CAPABILITIES . "/users/capabilities")
                                    (WALLETE-TXNS . "/wallet_txns")
                                    (WALLETABLES . "/walletables"))
             collect `(define-path ,name ,path)))


  
(defun request (uri connection &key method content)
  (let ((header `(("Authorization" . ,(format NIL "Bearer ~A" (access-token connection)))
                  ("accept" . "application/json"))))
    (cond ((eq method :get)
           (dex:get uri
                    :headers header
                    :proxy *PROXY*
                    :verbose *API-DEBUG*))
          ((eq method :post)
           (dex:post uri
                     :headers header
                     :content content
                     :proxy *PROXY*
                     :verbose *API-DEBUG*))
          ((eq method :put)
           (let ((header (push '("Content-Type" . "application/json") header))
                 (content (cl-json:encode-json content)))
             (dex:put uri
                      :headers header
                      :content content
                      :proxy *PROXY*
                      :verbose *API-DEBUG*)))
          ((eq method :delete)
           (dex:delete uri
                       :headers header
                       :proxy *PROXY*
                       :verbose *API-DEBUG*)))))
