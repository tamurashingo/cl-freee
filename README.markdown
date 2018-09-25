# Cl-Freee

## Usage

### 接続

アクセストークンをすでに取得済みの場合

```commonlisp
(defvar *conn*
  (make-connection :client-id "xxxxxxxxxxxxx"
                   :client-secret "xxxxxxxxxxxxx"
                   :redirect-uri "xxxxxxxxxxx"
                   :access-token "xxxxxxxxx"
                   :refresh-token "xxxxxxxxxxx"))
```

アクセストークンを取得していない場合（これから取得する場合）

```commonlisp
(defvar *conn*
  (make-connection :client-id "xxxxxxxxxxxxx"
                   :client-secret "xxxxxxxxxxxxx"
                   :redirect-uri "xxxxxxxxxxx"))

(authorize *conn* authorization-code)
```

### アクセストークン、リフレッシュトークンの保存

トークンをリフレッシュした際に、`callback`を呼ぶのでそれを使って新しいトークンを保存します。


```commonlisp
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




## Installation

## Author

* tamura shingo (tamura.shingo@gmail.com)

## Copyright

Copyright (c) 2018 tamura shingo (tamura.shingo@gmail.com)

## License

Licensed under the MIT License.
