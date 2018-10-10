(in-package :cl-user)
(defpackage cl-freee.api-test
  (:use :cl
        :prove
        :cl-mock))
(in-package :cl-freee.api-test)

(setf cl-freee:*API-DEBUG* T)

(plan 6)

(subtest "401が返ってきた時にリフレッシュして再度リクエストを投げること"
  (let ((conn (cl-freee:make-connection :client-id "xxxxx"
                                        :client-secret "yyyyy"
                                        :redirect-uri "zzzzz"
                                        :access-token "11111"
                                        :refresh-token "22222"))
        (CALL-1-GET NIL)
        (CALL-2-REFRESH NIL)
        (CALL-3-GET NIL))
    (with-mocks ()
      (answer (dex:get uri :headers header :proxy proxy :verbose verbose)
        (when (not CALL-1-GET)
          (setf CALL-1-GET T)
          (error (make-condition 'dexador.error:http-request-unauthorized
                                 :body "unauthorized error"
                                 :status 401
                                 :uri "https://api.freee.co.jp/")))
        (when (not CALL-3-GET)
          (setf CALL-3-GET T)
"
{
  \"companies\" : [
    {
      \"id\" : 101,
      \"name\" : \"freee事務所\",
      \"name_kana\" : \"フリージムショ\",
      \"display_name\" : \"freee事務所\",
      \"role\" : \"admin\"
    }
  ]
}
"
))
      (answer (dex:post uri :content content :proxy proxy :verbose verbose)
        (when (not CALL-2-REFRESH)
          (setf CALL-2-REFRESH T)
"
{
  \"access_token\": \"new_access_token\", 
  \"token_type\": \"bearer\", 
  \"expires_in\": 86400, 
  \"refresh_token\": \"new_refresh_token\", 
  \"scope\": \"read write\" 
}
"
))
      (let ((companies (cl-freee.api.companies:get-companies conn)))
        (ok CALL-1-GET
            "CALL-1-GETが呼ばれていること")
        (ok CALL-2-REFRESH
            "CALL-2-REFRESHが呼ばれていること")
        (ok CALL-3-GET
            "CALL-3-GETが呼ばれていること")
        (is (cl-freee.connection:access-token conn)
            "new_access_token"
            "新しいトークンがセットされていること")
        (is (cl-freee.connection::refresh-token conn)
            "new_refresh_token"
            "新しいトークンがセットされていること")
        (ok companies)))))

(subtest "403が返ってきた時にリフレッシュして再度リクエストを投げること"
  (let ((conn (cl-freee:make-connection :client-id "xxxxx"
                                        :client-secret "yyyyy"
                                        :redirect-uri "zzzzz"
                                        :access-token "11111"
                                        :refresh-token "22222"))
        (CALL-1-GET NIL)
        (CALL-2-REFRESH NIL)
        (CALL-3-GET NIL))
    (with-mocks ()
      (answer (dex:get uri :headers header :proxy proxy :verbose verbose)
        (when (not CALL-1-GET)
          (setf CALL-1-GET T)
          (error (make-condition 'dexador.error:http-request-forbidden
                                 :body "forbidden error"
                                 :status 403
                                 :uri "https://api.freee.co.jp/")))
        (when (not CALL-3-GET)
          (setf CALL-3-GET T)
"
{
  \"companies\" : [
    {
      \"id\" : 101,
      \"name\" : \"freee事務所\",
      \"name_kana\" : \"フリージムショ\",
      \"display_name\" : \"freee事務所\",
      \"role\" : \"admin\"
    }
  ]
}
"
))
      (answer (dex:post uri :content content :proxy proxy :verbose verbose)
        (when (not CALL-2-REFRESH)
          (setf CALL-2-REFRESH T)
"
{
  \"access_token\": \"new_access_token\", 
  \"token_type\": \"bearer\", 
  \"expires_in\": 86400, 
  \"refresh_token\": \"new_refresh_token\", 
  \"scope\": \"read write\" 
}
"
))
      (let ((companies (cl-freee.api.companies:get-companies conn)))
        (ok CALL-1-GET
            "CALL-1-GETが呼ばれていること")
        (ok CALL-2-REFRESH
            "CALL-2-REFRESHが呼ばれていること")
        (ok CALL-3-GET
            "CALL-3-GETが呼ばれていること")
        (is (cl-freee.connection:access-token conn)
            "new_access_token"
            "新しいトークンがセットされていること")
        (is (cl-freee.connection::refresh-token conn)
            "new_refresh_token"
            "新しいトークンがセットされていること")
        (ok companies)))))

(subtest "401が2回返ったときはエラーとなること"
  (let ((conn (cl-freee:make-connection :client-id "xxxxx"
                                        :client-secret "yyyyy"
                                        :redirect-uri "zzzzz"
                                        :access-token "11111"
                                        :refresh-token "22222"))
        (count 0))
    (with-mocks ()
      (answer (dex:get uri :headers header :proxy proxy :verbose verbose)
        (progn
          (setf count (1+ count))
          (error (make-condition 'dexador.error:http-request-unauthorized
                                 :body "unauthorized error"
                                 :status 401
                                 :uri "https://api.freee.co.jp/"))))
      (answer (dex:post uri :content content :proxy proxy :verbose verbose)
        (progn
          (setf count (1+ count))
          (error (make-condition 'dexador.error:http-request-unauthorized
                                 :body "unauthorized error"
                                 :status 401
                                 :uri "https://api.freee.co.jp/"))))
      (is-error (cl-freee.api.companies:get-companies conn)
                'dexador.error:http-request-unauthorized
                "401エラーが返ってくること")
      (is count 2
          "呼び出し回数が2回であること(get-companies, refresh)"))))

(subtest "403が2回返ったときはエラーとなること"
  (let ((conn (cl-freee:make-connection :client-id "xxxxx"
                                        :client-secret "yyyyy"
                                        :redirect-uri "zzzzz"
                                        :access-token "11111"
                                        :refresh-token "22222"))
        (count 0))
    (with-mocks ()
      (answer (dex:get uri :headers header :proxy proxy :verbose verbose)
        (progn
          (setf count (1+ count))
          (error (make-condition 'dexador.error:http-request-forbidden
                               :body "forbidden error"
                               :status 403
                               :uri "https://api.freee.co.jp/"))))
      (answer (dex:post uri :content content :proxy proxy :verbose verbose)
        (progn
          (setf count (1+ count))
          (error (make-condition 'dexador.error:http-request-forbidden
                               :body "forbidden error"
                               :status 403
                               :uri "https://api.freee.co.jp/"))))
      (is-error (cl-freee.api.companies:get-companies conn)
                'dexador.error:http-request-forbidden
                "403エラーが返ってくること")
      (is count 2
          "呼び出し回数が2回であること(get-companies, refresh)"))))

(subtest "401のあとに403が返ったときは403エラーとなること"
  (let ((conn (cl-freee:make-connection :client-id "xxxxx"
                                        :client-secret "yyyyy"
                                        :redirect-uri "zzzzz"
                                        :access-token "11111"
                                        :refresh-token "22222"))
        (count 0))
    (with-mocks ()
      (answer (dex:get uri :headers header :proxy proxy :verbose verbose)
        (progn
          (setf count (1+ count))
          (error (make-condition 'dexador.error:http-request-unauthorized
                                 :body "unauthorized error"
                                 :status 401
                                 :uri "https://api.freee.co.jp/"))))
      (answer (dex:post uri :content content :proxy proxy :verbose verbose)
        (progn
          (setf count (1+ count))
          (error (make-condition 'dexador.error:http-request-forbidden
                                 :body "forbidden error"
                                 :status 403
                                 :uri "https://api.freee.co.jp/"))))
      (is-error (cl-freee.api.companies:get-companies conn)
                'dexador.error:http-request-forbidden
                "403エラーが返ってくること")
      (is count 2
          "呼び出し回数が2回であること(get-companies, refresh)"))))

(subtest "403のあとに401が返ったときは401エラーとなること"
  (let ((conn (cl-freee:make-connection :client-id "xxxxx"
                                        :client-secret "yyyyy"
                                        :redirect-uri "zzzzz"
                                        :access-token "11111"
                                        :refresh-token "22222"))
        (count 0))
    (with-mocks ()
      (answer (dex:get uri :headers header :proxy proxy :verbose verbose)
        (progn
          (setf count (1+ count))
          (error (make-condition 'dexador.error:http-request-forbidden
                                 :body "forbidden error"
                                 :status 403
                                 :uri "https://api.freee.co.jp/"))))
      (answer (dex:post uri :content content :proxy proxy :verbose verbose)
        (progn
          (setf count (1+ count))
          (error (make-condition 'dexador.error:http-request-unauthorized
                                 :body "unauthorized error"
                                 :status 401
                                 :uri "https://api.freee.co.jp/"))))
      (is-error (cl-freee.api.companies:get-companies conn)
                'dexador.error:http-request-unauthorized
                "401エラーが返ってくること")
      (is count 2
          "呼び出し回数が2回であること(get-companies, refresh)"))))

(finalize)
