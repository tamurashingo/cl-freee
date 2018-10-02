#|
  This file is a part of cl-freee project.
  Copyright (c) 2018 tamura shingo (tamura.shingo@gmail.com)
|#

(in-package :cl-user)
(defpackage cl-freee-test-asd
  (:use :cl :asdf))
(in-package :cl-freee-test-asd)

(defsystem cl-freee-test
  :author "tamura shingo"
  :license "MIT"
  :depends-on (:cl-freee
               :prove
               :cl-mock)
  :components ((:module "t"
                :components
                ((:test-file "cl-freee")
                 (:test-file "connection")
                 (:module "api"
                  :components
                  ((:file "api"))))))
  :description "Test system for cl-freee"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
