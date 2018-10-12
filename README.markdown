# cl-freee
[![Build Status](https://travis-ci.org/tamurashingo/cl-freee.svg?branch=master)](https://travis-ci.org/tamurashingo/cl-freee)
[![License: MIT](https://img.shields.io/badge/License-MIT-brightgreen.svg)](https://opensource.org/licenses/MIT)

freeeのAPIをCommon Lispで使うためのライブラリです。


## TOOD

- [X] 会計freee 参照系APIの作成
- [ ] 会計freee 更新系APIの作成
- [ ] 人事労務feee 参照系APIの作成
- [ ] 人事労務freee 更新系APIの作成
- [X] トークン切れになった際の自動リフレッシュ
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

##### 勘定科目一覧の取得

指定した事業所の勘定科目一覧を取得する

```lisp
(cl-freee:get-account-items connection &key company-id base-date)
```

- `connection` (cl-freee.connection:&lt;freee-connection&gt;)
    - コネクション
- `company-id` (number)
    - 必須
    - 事業所ID
- `base-date` (string)
    - 基準日

##### 勘定科目の詳細情報の取得

指定した勘定科目の詳細を取得する

```lisp
(cl-freee:get-account-items-detail connection id &key company-id)
```

- `connection` (cl-freee.connection:&lt;freee-connection&gt;)
    - コネクション
- `id` (number)
    - 必須
    - 勘定科目ID
- `company-id` (number)
    - 必須
    - 事業所ID

##### 勘定科目の作成

指定した事業所の勘定科目を作成する


```lisp
(cl-freee:post-account-item connection &key content)
```

- `connection` (cl-freee.connection:&lt;freee-connection&gt;)
    - コネクション
- `content` (alist)
    - 必須
    - 勘定科目の作成。パラメータはAPIドキュメントを参照してください


```lisp
((:COMPANY--ID . 1046386)
 (:ACCOUNT--ITEM
  (:NAME . "新しい勘定科目")
  (:SHORTCUT . "NEWACCOUNTITEM")
  (:SHORTCUT--NUM . "999")
  (:TAX--NAME . "課税売上")
  (:GROUP--NAME . "その他預金")
  (:ACCOUNT--CATEGORY . "現金・預金")
  (:CORRESPONDING--INCOME--NAME . "売掛金")
  (:CORRESPONDING--EXPENSE--NAME . "買掛金")
  (:ACCUMULATED--DEP--ACCOUNT--ITEM--NAME . "減価償却累計額勘定科目")
  (:SEARCHABLE . 2)
  (:ITEMS
   ((:ID . 1)
    (:ID . 3)))
  (:PARTNERS
   ((:ID . 14134752)))))
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

- `connection` (cl-freee.connection:&lt;freee-connection&gt;)
    - コネクション


##### 事業所の詳細情報の取得

ユーザが所属する事業所の詳細を取得する

```lisp
(cl-freee:get-companies-detail connection id &key details account-items taxes
                               items partners sections tags walletables)
```

- `connection` (cl-freee.connection:&lt;freee-connection&gt;)
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

- `connection` (cl-freee.connection:&lt;freee-connection&gt;)
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

##### 指定した事業所の取引一覧（収入／支出）を取得する

```lisp
(cl-freee:get-deals connection &key company-id partner-id status type
                    start-issue-date end-issue-date start-due-date
                    end-due-date offset limit registered-from)
```

- `connection` (cl-freee.connection:&lt;freee-connection&gt;)
    - コネクション
- `company-id` (number)
    - 必須
    - 事業所ID
- `partner-id` (number)
    - 取引先で絞り込み
- `status` (string)
    - 決済状況で絞込 (未決済: unsettled, 完了: settled)
- `type` (string)
    - 収支区分 (収入: income, 支出: expense)
- `start-issue-date` (string)
    - 発生日で絞込：開始日(yyyy-mm-dd)
- `end-issue-date` (string)
    - 発生日で絞込：終了日(yyyy-mm-dd)
- `start-due-date` (string)
    - 支払期日で絞込：開始日(yyyy-mm-dd)
- `end-due-date` (string)
    - 支払期日で絞込：終了日(yyyy-mm-dd)
- `offset` (number)
    - 取得レコードのオフセット (デフォルト: 0)
- `limit` (number)
    - 取得レコードの件数 (デフォルト: 20, 最大: 100)
- `retistered-from` (string)
    - 取引登録元アプリで絞込（me: 本APIを叩くアプリ自身から登録した取引のみ）

##### 取引（収入／支出）の取得

指定した事業所の取引（収入／支出）を取得する

```lisp
(cl-freee:get-deals-detail connection id &key company-id)
```

- `connection` (cl-freee.connection:&lt;freee-connection&gt;)
    - コネクション
- `id` (number)
    - 取引ID
- `company-id` (number)
    - 必須
    - 事業所ID

##### 取引（収入／支出）の作成

指定した事業所の取引（収入／支出）を作成する

```lisp
(cl-freee:post-deals connection &key content)
```

- `connection` (cl-freee.connection:&lt;freee-connection&gt;)
    - コネクション
- `content` (alist)
    - 必須
    - 取引の作成。パラメータはAPIドキュメントを参照してください

```lisp
'((:COMPANY--ID . 1046386)
  (:ISSUE--DATE . "2018-10-01")
  (:DUE--DATE . NIL)
  (:TYPE . "expense")
  (:PARTNER--ID . 14134752)
  (:PREF--NUMBER . NIL)
  (:DETAILS
   ((:ACCOUNT--ITEM--ID . 166540467)
    (:TAX--CODE . 6)
    (:ITEM--ID . NIL)
    (:SECTION--ID . NIL)
    (:TAG--IDS . NIL)
    (:AMOUNT . 5250)
    (:DESCRIPTION . "備考")))
  (:PAYMENTS
   ((:DATE . "2018-10-01")
    (:FROM--WALLETABLE--TYPE . "bank_account")
    (:FROM--WALLETABLE--ID . 871657)
    (:AMOUNT . 5250))))
```

##### 取引（収入／支出）の支払行作成

指定した事業所の取引（収入／支出）の支払行を作成する

```lisp
(cl-freee:post-deals-payments connection id &key content)
```

- `connection` (cl-freee.connection:&lt;freee-connection&gt;)
    - コネクション
- `id` (number)
    - 取引ID
- `content` (alist)
    - 必須
    - 取引の支払行の内容。パラメータはAPIドキュメントを参照してください

```lisp
'((:COMPANY--ID . 1046386)
  (:DATE . "2018-10-09")
  (:FROM--WALLETABLE--TYPE . "wallet")
  (:FROM--WALLETABLE--ID . 1161161)
  (:AMOUNT . 3000)))
```

#### 品目

##### 品目一覧の取得

指定した事業所の品目一覧を取得する

```lisp
(cl-freee:get-items connection &key company-id)
```

- `connection` (cl-freee.connection:&lt;freee-connection&gt;)
    - コネクション
- `company-id` (number)
    - 必須
    - 事業所ID

##### 品目の作成

指定した事業所の品目を作成する

```lisp
(cl-freee:post-items connection &key content)
```

- `connection` (cl-freee.connection:&lt;freee-connection&gt;)
    - コネクション
- `content` (alist)
    - 必須
    - 品目の作成。パラメータはAPIドキュメントを参照してください

```lisp
'((:COMPANY--ID . 1)
  (:NAME . "新しい品目")
  (:SHORTCUT-1 . "NEWITEM")
  (:SHORTCUT-2 . "202"))
```

#### 仕訳帳

not implemented

#### 振替伝票

##### 振替伝票一覧の取得

指定した事業所の振替伝票一覧を取得する

```lisp
(cl-freee:get-manual-journals connection &key company-id start-issue-date
                              end-issue-date entry-side account-item-id
                              min-amount max-amount partner-id item-id
                              section-id comment-status comment-important
                              adjustment txn-number offset limit)
```

- `connection` (cl-freee.connection:&lt;freee-connection&gt;)
    - コネクション
- `company-id` (number)
    - 必須
    - 事業所ID
- `start-issue-date` (string)
    - 発生日で絞込：開始日(yyyy-mm-dd)
- `end-issue-date` (string)
    - 発生日で絞込：終了日(yyyy-mm-dd)
- `entry-side` (string)
    - 貸借で絞込 (貸方: credit, 借方: debit)
- `account-item-id` (number)
    - 勘定科目IDで絞込
- `min-amount` (number)
    - 金額で絞込：下限
- `max-amount` (number)
    - 金額で絞込：上限
- `partner-id` (numbrer)
    - 取引先IDで絞込（0を指定すると、取引先が未選択の貸借行を絞り込めます）
- `item-id` (number)
    - 品目IDで絞込（0を指定すると、品目が未選択の貸借行を絞り込めます）
- `section-id` (number)
    - 部門IDで絞込（0を指定すると、部門が未選択の貸借行を絞り込めます）
- `comment-status` (string)
    - コメント状態で絞込（自分宛のコメント: posted_with_mention, 自分宛のコメント-未解決: raised_with_mention, 自分宛のコメント-解決済: resolved_with_mention, コメントあり: posted, 未解決: raised, 解決済: resolved, コメントなし: none）
- `comment-important` (boolean)
    - 重要コメント付きの振替伝票を絞込
- `adjustment` (string)
    - 決算整理仕訳で絞込（決算整理仕訳のみ: only, 決算整理仕訳以外: without）
- `txn-number` (string)
    - 仕訳番号で絞込（事業所の仕訳番号形式が有効な場合のみ）
- `offset` (number)
    - 取得レコードのオフセット (デフォルト: 0)
- `limit` (number)
    - 取得レコードの件数 (デフォルト: 20, 最大: 500)


##### 振替伝票の取得

指定した事業所の振替伝票を取得する

```lisp
(cl-freee:get-manual-journals-detail connection id &key company-id)
```

- `connection` (cl-freee.connection:&lt;freee-connection&gt;)
    - コネクション
- `id` (number)
    - 振替伝票ID
- `company-id` (number)
    - 必須
    - 事業所ID

##### 振替伝票の作成

指定した事業所の振替伝票を作成する

```lisp
(cl-freee:post-manual-journals connection &key content)
```

- `connection` (cl-freee.connection:&lt;freee-connection&gt;)
    - コネクション
- `content` (alist)
    - 必須
    - 振替伝票の作成。パラメータはAPIドキュメントを参照してください

```lisp
'((:COMPANY--ID . 1)
  (:ISSUE--DATE . "2018-10-05")
  (:DETAILS
   ((:ENTRY--SIDE . "credit")
    (:ACCOUNT--ITEM--ID . 1)
    (:TAX--CODE . 108)
    (:AMOUNT . 3000)
    (:VAT . 222)
    (:ITEM--ID . 2)
    (:DESCRIPTION . "テスト"))
   ((:ENTRY--SIDE . "debit")
    (:ACCOUNT--ITEM--ID . 1)
    (:TAX--CODE . 108)
    (:AMOUNT . 3000)
    (:VAT . 222)
    (:PARTNER--ID . 3)
    (:ITEM--ID . 4)
    (:DESCRIPTION . "テスト"))))
```

#### 取引先

##### 取引先一覧の取得

指定した事業所の取引先一覧を取得する

```lisp
(cl-freee:get-partners connection &key company-id offset limit)
```

- `connection` (cl-freee.connection:&lt;freee-connection&gt;)
    - コネクション
- `company-id` (number)
    - 必須
    - 事業所ID
- `offset` (number)
    - 取得レコードのオフセット(デフォルト: 0)
- `limit` (number)
    - 取得レコードの件数

##### 取引先の作成

指定した事業所の取引先を作成する

```lisp
(cl-freee:post-partners connection &key content)
```

- `connection` (cl-freee.connection:&lt;freee-connection&gt;)
    - コネクション
- `content` (alist)
    - 必須
    - 取引先の作成。パラメータはAPIドキュメントを参照してください

```lisp
'((:COMPANY--ID . 1046386)
  (:NAME . "新しい取引先")
  (:SHORTCUT-1 . "NEWPARTNER")
  (:SHORTCUT-2 . "502")
  (:LONG--NAME . "新しい取引先正式名称")
  (:NAME--KANA . "アタラシイトリヒキサキメイショウ")
  (:DEFAULT--TITLE . "御中")
  (:PHONE . "03-3000-1111")
  (:CONTACT--NAME . "営業担当")
  (:EMAIL . "sample@sample.com")
  (:ADDRESS--ATTRIBUTES . ((:ZIPCODE . "012-0009")
                           (:PREFECTURE--CODE . 4)
                           (:STREET--NAME-1 . "湯沢市")
                           (:STREET--NAME-2 . "Aビル")))
  (:PARTNER--DOC--SETTING--ATTRIBUTES . ((:SENDING--METHOD . "posting")))
  (:PARTNER--BANK--ACCOUNT--ATTRIBUTES . ((:BANK--NAME . "みずほ銀行")
                                          (:BANK--NAME--KANA . "ミズホ")
                                          (:BANK--CODE . "001")
                                          (:BRANCH--NAME . "銀座支店")
                                          (:BRANCH--KANA . "ギンザ")
                                          (:BRANCH--CODE . "101")
                                          (:ACCOUNT--TYPE . "ordinary")
                                          (:ACCOUNT--NUMBER . "1010101")
                                          (:LONG--ACCOUNT--NAME . "freee太郎")
                                          (:ACCOUNT--NAME . "フリータロウ"))))
```

#### 部門

##### 部門一覧の取得

指定した事業所の部門一覧を取得する

```lisp
(cl-freee:get-sections connection &key company-id)
```

- `connection` (cl-freee.connection:&lt;freee-connection&gt;)
    - コネクション
- `company-id` (number)
    - 必須
    - 事業所ID

##### 部門の作成

指定した事業所の部門を作成する

```lisp
(cl-freee:post-sections connection &key content)
```

- `connection` (cl-freee.connection:&lt;freee-connection&gt;)
    - コネクション
- `content` (alist)
    - 必須
    - 部門の作成。パラメータはAPIドキュメントを参照してください

```lisp
'((:COMPANY--ID . 1)
  (:NAME . "開発1")
  (:LONG--NAME . "第一開発部")
  (:SHORTCUT-1 . "DEV1")
  (:SHORTCUT-2 . "11"))
```

#### フォーム用選択項目情報

##### フォーム用選択項目情報の取得

指定した事業所のフォーム用選択項目情報を取得する

```lisp
(cl-freee:get-selectables connection &key company-id includes)
```

- `connection` (cl-freee.connection:&lt;freee-connection&gt;)
    - コネクション
- `company-id` (number)
    - 必須
    - 事業所ID
- `includes` (string)
    - 取得する項目(項目: account_item)

#### メモタグ

##### メモタグ一覧の取得

指定した事業所のメモタグ一覧を取得する

```lisp
(cl-freee:get-tags connection &key company-id)
```

- `connection` (cl-freee.connection:&lt;freee-connection&gt;)
    - コネクション
- `company-id` (number)
    - 必須
    - 事業所ID

##### メモタグの作成

指定した事業所のメモタグを作成する

```lisp
(cl-freee:post-tags connection &key content)
```

- `connection` (cl-freee.connection:&lt;freee-connection&gt;)
    - コネクション
- `content` (alist)
    - 必須
    - メモタグの作成。パラメータはAPIドキュメントを参照してください

```lisp
'((:COMPANY--ID . 1046386)
  (:NAME . "メモタグ")
  (:SHORTCUT-1 . "memo")
  (:SHORTCUT-2 . "1"))
```

#### 税区分

##### 税区分コード一覧の取得

税区分コード一覧を取得する

```lisp
(cl-freee:get-taxes-codes connection)
```

- `connection` (cl-freee.connection:&lt;freee-connection&gt;)
    - コネクション

#### 取引（振替）

##### 取引（振替）一覧の取得

指定した事業所の取引（振替）一覧を取得する

```lisp
(cl-freee:get-transfers connection &key company-id start-date end-date offset limit)
```

- `connection` (cl-freee.connection:&lt;freee-connection&gt;)
    - コネクション
- `company-id` (number)
    - 必須
    - 事業所ID
- `start-date` (string)
    - 振替日で絞込：開始日 (yyyy-mm-dd)
- `end-date` (string)
    - 振替日で絞込：終了日 (yyyy-mm-dd)
- `offset` (number)
    - 取得レコードのオフセット (デフォルト: 0)
- `limit` (number)
    - 取得レコードの件数 (デフォルト: 20, 最大: 100)

##### 取引（振替）の作成

指定した事業所の取引（振替）を作成する

```lisp
(cl-freee:post-transfers connection &key content)
```

- `connection` (cl-freee.connection:&lt;freee-connection&gt;)
    - コネクション
- `content` (alist)
    - 必須
    - 取引（振替）の作成。パラメータはAPIドキュメントを参照してください

```lisp
'((:TO--WALLETABLE--ID . 1)
  (:TO--WALLETABLE--TYPE . "bank_account")
  (:FROM--WALLETABLE--ID . 2)
  (:FROM--WALLETABLE--TYPE . "wallet")
  (:AMOUNT . 5000)
  (:DATE . "2018-10-10")
  (:COMPANY--ID . 1)
  (:DESCRIPTION . "テスト"))
```

#### ユーザ

##### ログインユーザ情報の取得

ユーザの情報を取得する

```lisp
(cl-freee:get-users-me connection &key companies)
```

- `connection` (cl-freee.connection:&lt;freee-connection&gt;)
    - コネクション
- `companies` (boolean)
    - 取得情報にユーザが所属する事業所一覧を含める

##### ログインユーザの権限の取得

ユーザの権限情報を取得する

```lisp
(cl-freee:get-users-capabilities &key company-id)
```

- `connection` (cl-freee.connection:&lt;freee-connection&gt;)
    - コネクション
- `company-id` (number)
    - 必須
    - 事業所ID

#### 明細

##### 明細一覧の取得

指定した事業所の明細一覧を取得する

```lisp
(cl-freee:get-wallete-txns connection &key company-id walletable-type walletable-id
                           start-date end-date entry-side offset limit)
```

- `connection` (cl-freee.connection:&lt;freee-connection&gt;)
    - コネクション
- `company-id` (number)
    - 必須
    - 事業所ID
- `walletable-type` (string)
    - 口座区分 (銀行口座: bank_account, クレジットカード: credit_card, 現金: wallet)
- `walletable-id` (number)
    - 口座ID
- `start-date` (string)
    - 取引日で絞込：開始日 (yyyy-mm-dd)
- `end-date` (string)
    - 取引日で絞込：終了日 (yyyy-mm-dd)
- `entry-side` (string)
    - 入金／出金 (入金: income, 出金: expense)
- `offset` (number)
    - 取得レコードのオフセット (デフォルト: 0)
- `limit` (number)
    - 取得レコードの件数 (デフォルト: 20, 最大: 100)

##### 明細の作成

指定した事業所の明細を作成する

```lisp
(cl-freee:post-wallete-txns connection &key content)
```

- `connection` (cl-freee.connection:&lt;freee-connection&gt;)
    - コネクション
- `content` (alist)
    - 必須
    - 明細の作成。パラメータはAPIドキュメントを参照してください

```lisp
'((:ENTRY--SIDE . "income")
  (:DESCRIPTION . "振込 カ）ABC")
  (:AMOUNT . 5000)
  (:WALLETABLE--ID . 1)
  (:WALLETABLE--TYPE . "bank_account")
  (:DATE . "2018-01-01")
  (:COMPANY--ID . 1)
  (:BALANCE . 10000))
```

#### 口座

##### 口座一覧の取得

指定した事業所の口座一覧を取得する

```lisp
(cl-freee:get-walletables connection &key company-id with-balance)
```

- `connection` (cl-freee.connection:&lt;freee-connection&gt;)
    - コネクション
- `company-id` (number)
    - 必須
    - 事業所ID
- `with-balance`(string)
    - 残高情報を含める

##### 口座の作成

指定した事業所に口座を作成する

```lisp
(cl-freee:post-walletables connection &key content)
```

- `connection` (cl-freee.connection:&lt;freee-connection&gt;)
    - コネクション
- `content` (alist)
    - 必須
    - 講座の作成。パラメータはAPIドキュメントを参照してください

```lisp
'((:NAME . "〇〇銀行")
  (:TYPE . "bank_account")
  (:BANK--ID . 1)
  (:COMPANY--ID . 1)
  (:GROUP--NAME . "前受金"))
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
