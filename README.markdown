# Cl-Freee

## Usage

### connection

```commonlisp
(defvar *conn*
  (make-connection :client-id "xxxxxxxxxxxxx"
                   :client-secret "xxxxxxxxxxxxx"
                   :redirect-uri "xxxxxxxxxxx"
                   :access-token "xxxxxxxxx"
                   :refresh-token "xxxxxxxxxxx"))
```

```commonlisp
(defvar *conn*
  (make-connection :client-id "xxxxxxxxxxxxx"
                   :client-secret "xxxxxxxxxxxxx"
                   :redirect-uri "xxxxxxxxxxx"))

(authorize *conn* authorization-code)
```

### store access-token and refresh-token

use `:callback` to store access-token and refresh-token.


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


## Installation

## Author

* tamura shingo (tamura.shingo@gmail.com)

## Copyright

Copyright (c) 2018 tamura shingo (tamura.shingo@gmail.com)

## License

Licensed under the MIT License.
