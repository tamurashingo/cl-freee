#|
  This file is a part of cl-freee project.
  Copyright (c) 2018 tamura shingo (tamura.shingo@gmail.com)
|#

#|
  Author: tamura shingo (tamura.shingo@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-freee-asd
  (:use :cl :asdf))
(in-package :cl-freee-asd)

(defsystem cl-freee
  :version "0.1"
  :author "tamura shingo"
  :license "MIT"
  :depends-on (:dexador
               :quri
               :cl-json)
  :components ((:module "src"
                :components
                ((:file "cl-freee" :depends-on ("connection" "api"))
                 (:file "connection")
                 (:module "api" :depends-on ("connection")
                  :components
                  ((:file "api")
                   (:file "account_items" :depends-on ("api"))
                   (:file "banks" :depends-on ("api"))
                   (:file "companies" :depends-on ("api"))
                   (:file "deals" :depends-on ("api"))
                   (:file "items" :depends-on ("api"))
                   (:file "manual_journals" :depends-on ("api"))
                   (:file "partners" :depends-on ("api"))
                   (:file "sections" :depends-on ("api"))
                   (:file "selectables" :depends-on ("api"))
                   (:file "tags" :depends-on ("api"))
                   (:file "taxes" :depends-on ("api"))
                   (:file "transfers" :depends-on ("api"))
                   (:file "users" :depends-on ("api"))
                   (:file "walletables" :depends-on ("api"))
                   (:file "wallete_txns" :depends-on ("api")))))))
  :description ""
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op cl-freee-test))))
