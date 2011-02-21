2011-02-22  Ver. 0.01.04
========================
- `check-type` で [typespec+] を使うようにした。

- `assert` と `check-type` を別ファイル "assertions.l" に移動。
  上記の変更で `check-type` が [typespec+] に依存するようになったけど、
  "condition-restart" 自体は依存しないでおくために。
  - これらのマクロを使うには（"condition-restart" ではなく）"assertions" 
    を `require` する必要があります。
  - 別途 [typespec+] をインストールしておく必要があります。

  [typespec+]: http://github.com/bowbow99/xyzzy.typespec-plus

- `ed:read-value` を追加
  ミニバッファから値の入力を受け付ける関数。再起動の INTERACTIVE-FUNCTION
  で使うのに欲しかったので。

- 関数 `abort`: 再起動 `abort` が見つからなかったらエラー `quit` を投げ
  るようにした。
  結果的に、再起動 `abort` があれば使う関数 `quit` みたいになった。

- パッケージ condition-restart から以下を export しておいた。
  - +version+
  - restart-not-found
  - restart-not-fonud-designator
  - restart-not-found-condition

- リファレンスをあちこち書き直し。

- いろいろバグ修正。
  詳しくは [ChangeLog] かリポジトリの [Commits] あたり参照してください。

  [ChangeLog]: http://github.com/bowbow99/xyzzy.condition-restart/blob/master/ChangeLog
  [Commits]: http://github.com/bowbow99/xyzzy.condition-restart/commits

2010-08-06  Ver. 0.01.03
========================
- assert のメッセージを少しマシにした。
- 配布物にゴミが混入してたので削除。(thx to @southly)

2010-06-13  Ver. 0.01.02
========================
- リファレンスの file が間違ってたのを修正した。

- select-restart-interactively が再帰的に呼ばれた時にウィンドウのサイズ
がおかしくなってたのを修正したつもり。

- assert の PLACES が nil のときのメッセージを変更した。

2010-06-10  Ver. 0.01.01
========================
- 再起動選択を繰り返した時の画面分割のバグに対処した。

2010-06-10  Ver. 0.01.00
========================
- 作った。公開なう。
