(in-package :cl-user)
(defpackage cl-freee
  (:use :cl)
  (:import-from :cl-freee.connection
                :make-connection
                :authorize
                :refresh
                :*PROXY*
                :*API-DEBUG*)
  (:import-from :cl-freee.api.account-items
                :get-account-items
                :get-account-items-detail
                :post-account-items
                :put-account-items)
  (:import-from :cl-freee.api.banks
                :get-banks)
  (:import-from :cl-freee.api.companies
                :get-companies
                :get-companies-detail
                :put-companies)
  (:import-from :cl-freee.api.deals
                :get-deals
                :get-deals-detail
                :post-deals
                :post-deals-payments
                :put-deals)
  (:import-from :cl-freee.api.items
                :get-items
                :post-items)
  (:import-from :cl-freee.api.manual-journals
                :get-manual-journals
                :get-manual-journals-detail
                :post-manual-journals
                :put-manual-journals)
  (:import-from :cl-freee.api.partners
                :get-partners
                :post-partners
                :put-partners)
  (:import-from :cl-freee.api.sections
                :get-sections
                :post-sections)
  (:import-from :cl-freee.api.selectables
                :get-selectables)
  (:import-from :cl-freee.api.tags
                :get-tags
                :post-tags)
  (:import-from :cl-freee.api.taxes
                :get-taxes-codes)
  (:import-from :cl-freee.api.transfers
                :get-transfers
                :post-transfers)
  (:import-from :cl-freee.api.users
                :get-users-me
                :get-users-capabilities)
  (:import-from :cl-freee.api.walletables
                :get-walletables
                :post-walletables)
  (:import-from :cl-freee.api.wallete-txns
                :get-wallete-txns
                :post-wallete-txns)
  (:export ;connection
           :make-connection
           :authorize
           :refresh
           ; api
           ;   get
           :get-account-items
           :get-account-items-detail
           :get-banks
           :get-companies
           :get-companies-detail
           :get-deals
           :get-deals-detail
           :get-items
           :get-manual-journals
           :get-manual-journals-detail
           :get-partners
           :get-sections
           :get-selectables
           :get-tags
           :get-taxes-codes
           :get-transfers
           :get-users-me
           :get-users-capabilities
           :get-walletables
           :get-wallete-txns
           ;   post
           :post-account-items
           :post-deals
           :post-deals-payments
           :post-items
           :post-manual-journals
           :post-sections
           :post-tags
           :post-transfers
           :post-walletables
           :post-wallete-txns
           ;   put
           :put-account-items
           :put-companies
           :put-deals
           :put-manual-journals
           :put-partners
           ; variable
           :*PROXY*
           :*API-DEBUG*))
(in-package :cl-freee)

