# Cl-Interceptor


`cl-interceptor` は、Common Lisp において構造的な実行パイプライン（往路・復路・異常系）を構築するためのライブラリです。

### 1. インターセプタの定義 (`define-interceptor`)

ロジックの最小単位を定義します。共通の `lambda-list` を持ち、各フェーズ（`:enter`, `:leave`, `:error`）を記述します。

```lisp
(define-interceptor my-interceptor (a b)
  (:enter (+ a b))                     ; 往路の処理
  (:leave (* a b))                     ; 復路の処理
  (:error (c) (format t "Error: ~A" c))) ; 異常系の処理（c はコンディション）

```

### 2. エグゼキュータの定義 (`define-executor`)

データスロット（`contexts`）を定義し、インターセプタを特定の変数にバインドします。

```lisp
(define-executor test-executor
    ;; Contexts: (変数名 初期値)
    (((num1 10) (num2 20) result)
     ;; Definitions: (エイリアス名 インターセプタ名 入力スロット 出力スロット)
     (step1 my-interceptor (num1 num2) (result)))
    ;; Actions & Return
    ((step1) 
     :return (values num1 num2 result)))

```

### 3. パイプラインの実行 (`execute`)

エグゼキュータ名を指定して実行します。初期値を動的に上書きすることも可能です。

```lisp
;; デフォルト値で実行
(execute 'test-executor) 
;; => 10, 20, 200

;; 初期値を指定して実行
(execute 'test-executor :num1 100 :num2 200)
;; => 100, 200, 20000

```

### 4. 動的な操作 (`push-interceptor`, `pop-interceptor`)

実行中に `signal` を通じてパイプラインを書き換えます。フェーズに応じて自動的に挿入先（Queue または Stack）が決定されます。

* **`push-interceptor`**: 新しいインターセプタを注入します。
* **`pop-interceptor`**: 現在のターゲットから直近のインターセプタを削除します。

```lisp
(define-interceptor dynamic-injector (a)
  (:enter 
    ;; 実行中に別のインターセプタを注入
    (push-interceptor 'another-interceptor '(a) '(a))
    ;; 次の予定をキャンセル
    (pop-interceptor)))

```

### 5. エラーハンドリング

`interceptor-error` が発生すると、スタックを遡りながら各インターセプタの `:error` フェーズが順次実行されます。遡及中にさらにエラーが発生した場合も、再帰的に解決を試みます。


## Author and License

`cl-interceptor` was written by Yukko and is distributed
under the terms of the MIT license.
