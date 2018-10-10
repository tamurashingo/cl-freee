(in-package :cl-user)
(defpackage cl-freee.connection-test
  (:use :cl
        :prove
        :cl-mock
        :cl-freee.connection))
(in-package :cl-freee.connection-test)

(plan 3)

(subtest "make-connection needs 'client-id 'client-secret 'redirect-uri"
  (is-error
   (make-connection)
   'simple-error
   "client-id, client-secret, redirect-uriが必須であることの確認")
  (is-error
   (make-connection :client-id "xxxxx")
   'simple-error
   "client-id, client-secret, redirect-uriが必須であることの確認")
  (is-error
   (make-connection :client-secret "yyyyy")
   'simple-error
   "client-id, client-secret, redirect-uriが必須であることの確認")
  (is-error
   (make-connection :redirect-uri "zzzzz")
   'simple-error
   "client-id, client-secret, redirect-uriが必須であることの確認")
  (is-error
   (make-connection :client-id "xxxxx"
                    :client-secret "yyyyy")
   'simple-error
   "client-id, client-secret, redirect-uriが必須であることの確認")
  (is-error
   (make-connection :client-id "xxxxx"
                    :redirect-uri "zzzzz")
   'simple-error
   "client-id, client-secret, redirect-uriが必須であることの確認")
  (is-error
   (make-connection :client-secret "yyyyy"
                    :redirect-uri "zzzzz")
   'simple-error
   "client-id, client-secret, redirect-uriが必須であることの確認")
  (ok
   (make-connection :client-id "xxxxx"
                    :client-secret "yyyyy"
                    :redirect-uri "zzzzz")
   "client-id, client-secret, redirect-uriを指定した場合はエラーとならないこと"))


(subtest "authorize"
  (let ((conn (make-connection :client-id "xxxxxx"
                               :client-secret "yyyyy"
                               :redirect-uri "zzzzz")))
    (with-mocks ()
      (answer (dex:post URI :content content :proxy proxy :verbose verbose)
"
{
  \"access_token\": \"access_token\", 
  \"token_type\": \"bearer\", 
  \"expires_in\": 86400, 
  \"refresh_token\": \"refresh_token\", 
  \"scope\": \"read write\" 
}
"
)
      (authorize conn "xxxxxxxxxx")
      (is (cl-freee.connection::access-token conn) "access_token")
      (is (cl-freee.connection::refresh-token conn) "refresh_token"))))

(subtest "refresh"
  (let ((conn (make-connection :client-id "xxxxxx"
                               :client-secret "yyyyy"
                               :redirect-uri "zzzzz"
                               :access-token "11111"
                               :refresh-token "22222")))
    (with-mocks ()
      (answer (dex:post URI :content content :proxy proxy :verbose verbose)
"
{
  \"access_token\": \"new_access_token\", 
  \"token_type\": \"bearer\", 
  \"expires_in\": 86400, 
  \"refresh_token\": \"new_refresh_token\", 
  \"scope\": \"read write\" 
}
"
)
      (refresh conn)
      (is (cl-freee.connection::access-token conn) "new_access_token")
      (is (cl-freee.connection::refresh-token conn) "new_refresh_token"))))

(finalize)
