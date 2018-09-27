# cl-freee

freeeのAPIをCommon Lispで使うためのライブラリです。


## TOOD

- [X] 会計freee 参照系APIの作成
- [ ] 会計freee 更新系APIの作成
- [ ] 人事労務feee 参照系APIの作成
- [ ] 人事労務freee 更新系APIの作成
- [ ] トークン切れになった際の自動リフレッシュ
- [X] PROXY対応


## Usage

### 接続

アクセストークンをすでに取得済みの場合

```lisp
(defvar *conn*
  (make-connection :client-id "xxxxxxxxxxxxx"
                   :client-secret "xxxxxxxxxxxxx"
                   :redirect-uri "xxxxxxxxxxx"
                   :access-token "xxxxxxxxx"
                   :refresh-token "xxxxxxxxxxx"))
```

アクセストークンを取得していない場合（これから取得する場合）

```lisp
(defvar *conn*
  (make-connection :client-id "xxxxxxxxxxxxx"
                   :client-secret "xxxxxxxxxxxxx"
                   :redirect-uri "xxxxxxxxxxx"))

(authorize *conn* authorization-code)
```

### アクセストークン、リフレッシュトークンの保存

トークンをリフレッシュした際に、`callback`を呼ぶのでそれを使って新しいトークンを保存します。


```lisp
(defvar *access-token* NIL)
(defvar *refresh-token* NIL)

(defvar *conn*
  (make-connection :client-id "xxxxxxxxxxxxx"
                   :client-secret "xxxxxxxxxxxxx"
                   :redirect-uri "xxxxxxxxxxx"
                   :callback #'(lambda (access-token refresh-token)
                                 (setf *access-token* access-token)
                                 (setf *refresh-token* refresh-token))))
```

### API

#### 勘定科目

```lisp
(get-account-items *conn* :company-id xxxx)
```

```lisp
(get-account-items-detail *conn* xxx :company-id xxxx)
```

#### 連携サービス

```lisp
(get-banks *conn* :type "credit_card")
```

#### 事業所

##### 事業所一覧の取得

```lisp
(cl-freee:get-companies connection)
```

- `connection` (cl-freee.connection:<freee-connection>)
    - コネクション


##### 事業所の詳細情報の取得

ユーザが所属する事業所の詳細を取得する

```lisp
(cl-freee:get-companies-detail connection id &key details account-items taxes
                               items partners sections tags walletables)
```

- `connection` (cl-freee.connection:<freee-connection>)
    - コネクション
- `id` (number)
    - 必須
    - company-id
- `details` (boolean)
    - 取得情報に勘定科目・税区分コード・税区分・品目・取引先・部門・メモタグ・口座の一覧を含める
- `account-items` (boolean)
    - 取得情報に勘定科目一覧を含める
- `taxes` (boolean)
    - 取得情報に税区分コード・税区分一覧を含める
- `items` (boolean)
    - 取得情報に品目一覧を含める
- `partners` (boolean)
    - 取得情報に取引先一覧を含める
- `sections` (boolean)
    - 取得情報に部門一覧を含める
- `tags` (boolean)
    - 取得情報にメモタグ一覧を含める
- `walletables` (boolean)
    - 取得情報に口座一覧を含める


##### 事業所情報の更新

ユーザが所属する事業所の情報を更新する
※同時に複数のリクエストを受け付けない


```lisp
(cl-freee:put-companies connection id &key content)
```

- `connection` (cl-freee.connection:<freee-connection>)
    - コネクション
- `id` (number)
    - 必須
    - company-id
- `content` (alist)
    - 必須
    - 更新内容。パラメータはAPIドキュメントを参照してください

```lisp
'((:NAME . "新事業所名")
  (:NAME--KANA . "シンジギョウショメイ")
  (:CONTACT--NAME . "担当者名")
  (:ADDRESS--ATTRIBUTES . ((:ZIPCODE . "141-0031")
                           (:PREFECTURE--CODE . 12)
                           (:STREET--NAME-1 . "品川区西五反田2-8-1")
                           (:STREET--NAME-2 . "五反田ファーストビル9F"))))
```

#### 取引

```lisp
(get-deals *conn* :company-id xxxx)
```

```lisp
(get-deals-detail *conn* xxx :company-id xxxx)
```

#### 品目

```lisp
(get-items *conn* :company-id xxxx)
```

#### 仕訳帳

not implemented

#### 振替伝票

```lisp
(get-manual-journals *conn* :company-id xxxx)
```

```lisp
(get-manual-journals-detail *conn* xxx :company-id xxxx)
```

#### 取引先

```lisp
(get-partners *conn* :company-id xxxx)
```

#### 部門

```lisp
(get-sections *conn* :company-id xxxx)
```

#### フォーム用選択項目情報

```lisp
(get-selectables *conn* :company-id xxxx)
```

#### 税区分

```lisp
(get-taxes-codes *conn*)
```

#### 取引（振替）

```lisp
(get-transfers *conn* :company-id xxxx)
```

#### ユーザ

```lisp
(get-users-me *conn*)
```

```lisp
(get-users-capabilities *conn* :company-id xxxx)
```

#### 明細

```lisp
(get-wallete-txns *conn* :company-id xxxx)
```

#### 口座

```lisp
(get-walletables *conn* :company-id xxxx)
```

### PROXY
接続前に `*PROXY*` に値を設定してください。
[dexador](https://github.com/fukamachi/dexador#proxy) を使っているので、その形式で設定してください。


```lisp
(ql:quickload :cl-freee)
(setf cl-freee:*PROXY* "http://proxy:8080")
(defvar *conn*
  (cl-freee:make-connection
   :client-id "xxxxxxxxxx"
   :client-secret "xxxxxxxxxx"
   :redirect-uri "urn:ietf:wg:oauth:2.0:oob"))
(cl-freee:authorize *conn* "xxxxxxxxxx")
```

### デバッグ
通信内容を確認したい場合は、 `*API-DEBUG*` に `T` を設定してください。

```lisp
(cl-freee:get-companies *conn*)
....
....

(setf cl-freee:*API-DEBUG* T)
(cl-freee:get-companies *conn*)
>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
GET /api/1/companies HTTP/1.1
User-Agent: Dexador/0.9.10 (SBCL 1.3.21); Darwin; 16.7.0
Host: api.freee.co.jp
Accept: application/json
Authorization: Bearer xxxxxxxxxxxxxxxxxxxxxxxx

>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
HTTP/1.1 200 OK
Date: Thu, 27 Sep 2018 09:43:45 GMT
Content-Type: application/json; charset=UTF-8
Transfer-Encoding: chunked
Connection: keep-alive
Status: 200 OK
Cache-Control: max-age=0, private, must-revalidate
X-XSS-Protection: 1; mode=block
X-Request-Id: b639kqs0rg81hm8bo2m0
ETag: W/"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
X-Frame-Options: SAMEORIGIN
X-Runtime: 0.154992
X-Content-Type-Options: nosniff
Access-Control-Allow-Origin: https://developer.freee.co.jp
Access-Control-Allow-Headers: *
Access-Control-Allow-Methods: *

<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
.....
.....
```


## 例

一部マスクしてます

```
CL-USER> (defvar *conn*
           (cl-freee:make-connection
            :client-id "xxxxxxxxxxxxxxxxxx"
            :client-secret "xxxxxxxxxxxxxxxxxxx"
            :redirect-uri "urn:ietf:wg:oauth:2.0:oob"))
*CONN*
CL-USER> (cl-freee:authorize *conn* "xxxxxxxxxxxxxxx")
#<CL-FREEE.CONNECTION:<FREEE-CONNECTION> {100481E0D3}>

CL-USER> (cl-freee:get-companies-detail *conn* :id 1046386)
((:COMPANY (:ID . 1046386) (:NAME) (:NAME--KANA) (:DISPLAY--NAME . "タムシステムズ")
  (:TAX--AT--SOURCE--CALC--TYPE . 1) (:CONTACT--NAME) (:HEAD--COUNT)
  (:CORPORATE--NUMBER . "") (:TXN--NUMBER--FORMAT . "not_used")
  (:DEFAULT--WALLET--ACCOUNT--ID . 166540476) (:PRIVATE--SETTLEMENT . T)
  (:MINUS--FORMAT . 0) (:ROLE . "admin") (:PHONE-1 . "xxx-xxxx-xxxx")
  (:PHONE-2 . "") (:FAX . "") (:ZIPCODE . "xxxxxxx") (:PREFECTURE--CODE . 10)
  (:STREET--NAME-1 . "XXXXXXXXXXXXXXXXX") (:STREET--NAME-2 . "")
  (:INVOICE--LAYOUT . 0) (:INVOICE--STYLE . 0) (:AMOUNT--FRACTION . 0)
  (:INDUSTRY--CLASS . "") (:INDUSTRY--CODE . "")
  (:WORKFLOW--SETTING . "disable")
  (:FISCAL--YEARS
   ((:USE--INDUSTRY--TEMPLATE) (:INDIRECT--WRITE--OFF--METHOD)
    (:START--DATE . "2017-01-01") (:END--DATE . "2017-12-31")
    (:DEPRECIATION--RECORD--METHOD . 0) (:TAX--METHOD . 0)
    (:SALES--TAX--BUSINESS--CODE . 0) (:TAX--FRACTION . 0)
    (:TAX--ACCOUNT--METHOD . 0) (:RETURN--CODE . 0)))))
```


## Installation

## Author

* tamura shingo (tamura.shingo@gmail.com)

## Copyright

Copyright (c) 2018 tamura shingo (tamura.shingo@gmail.com)

## License

Licensed under the MIT License.
