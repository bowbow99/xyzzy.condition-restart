概要
====
Common Lisp のコンディションシステムにある再起動 (Restart) です。

個々の関数やマクロについては、付属のへなちょこリファレンス形式のリフ
ァレンスを参照してください。

- File: reference/condition-restart.xml

参考リンク:
- [CLHS: 9.1.4.2 Restarts] http://www.lispworks.com/documentation/HyperSpec/Body/09_adb.htm

おまけ: 再起動を使える assert と check-type も混ざってます。

インストール
============
netinstaller でインストールした人はそのままでおｋです。
手動でインストールする場合は、アーカイブを xyzzy のディレクトリにまる
っと解答すればおｋなはずです。

設定？
======
*scratch* とかで評価してエラーったときに再起動を選択して起動する方法に
ついては condition-restart-support.l を参照してください。

使い方 for 開発者
=================
再起動を使いたい拡張 lisp で require して下さい。必要な関数やマクロは
lisp パッケージから export してあるので、通常はそれだけで使えるように
なります。

    (require :condition-restart)

それぞれのマクロや関数についてはリファレンスを参照してください。

- reference/condition-restart.xml

とりあえず試したい人向けサンプル
--------------------------------
lisp-mode の C-x C-e か lisp-interaction-mode の C-j で上から順に評価
してみてください。

(require "condition-restart")
(require "condition-restart-support")

(restart:setup-key-bindings)

(cerror "知るかちょ" "ﾌﾞﾙｧ!!!")

(restart-case
    (/ 3 2 1 0)
  (zero () :report "0 を返す。" 0)
  (one () :report "1 を返す。" 1)
  (use-value (value)
    :report "返す値を入力する。"
    :interactive (lambda ()
                   (list (eval (read-sexp ">> "))))
    value))

(let ((name :fred)
      (age -3))
  (assert (and (stringp name) (< (length name) 64)) (name)
          "名前が変です: ~S" name)
  (assert (and (numberp age) (<= 0 age 120)) (age)
          "トシが変です: ~S" age)
  (list name age))

上書き注意報
============
- 標準の関数 warn を上書きしています。
  再起動を用意するようにした以外は標準のものと同じ挙動にしたつもり
  ですが、おかしくなっているかも知れません。
