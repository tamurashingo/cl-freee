# Cl-Freee

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

```lisp
(get-companies *conn*)
```

```lisp
(get-companies-details :id xxxx)
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


## Installation

## Author

* tamura shingo (tamura.shingo@gmail.com)

## Copyright

Copyright (c) 2018 tamura shingo (tamura.shingo@gmail.com)

## License

Licensed under the MIT License.
