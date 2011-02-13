;;; -*- mode: lisp; package: lisp -*-
;;;
;;; assertions.l --- Common Lisp の assert など
;;;
;;; このファイルは condition-restart の付属品です。
;;;
;;; Author:     bowbow99  <bowbow99@gmail.com>
;;; License:    MIT (see COPYING)

;;; Commentary:
;; これは何
;; ========
;; 値をチェックするマクロで再起動を使うようにしたものです。
;; - `assert`
;; - `check-type`
;;
;; 依存するライブラリ
;; ==================
;; - typespec+
;;
;; 使い方
;; ======
;; `assert` や `check-type` を使うファイルで `require` してください。
;;
;;     (eval-when (:compile-toplevel :load-toplevel :execute)
;;       (require "assertions"))
;;
;; `assert` も `check-type` もマクロなのですが、展開後に condition-restart
;; で定義されている関数が必要になるので、コンパイル済みファイルを読み込む
;; 場合にも `require` しておく必要があります。
;;
;; 後は `assert` 式や `check-type` 式を書いておけば、いちいち指定された通り
;; になっているか確認してくれます。
;; もし指定された通りになっていなければ再起動を用意してエラーを投げるので、
;; （再起動の選択をサポートしていれば）変数の値を変更して続行したりできます。
;;
;; それぞれのマクロについてはリファレンスを参照してください。
;;
;; - File: reference/condition-restart.xml

;;; Code:

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "condition-restart")
  (require "typespec+"))

(in-package :lisp)

(export '(assert
          +check-type-error-symbols+
          ))

;;;;
;;;; * Assertions

;;; NOTE: 不可解なことに xyzzy 標準の `check-type` で使っている `check-type-error`
;;; は system パッケージで定義されていて、捕まえようとすると `si::check-type-error`
;;; と書かなきゃいけなかったりして不便なので、関係あるシンボルのリストを用意して
;;; あります。
;;; `import` しておけばそのパッケージ内で "si::" 抜きで使えます。
;;;
;;;     (import +check-type-error-symbols+)
;;;
(defconstant +check-type-error-symbols+
  '(si::check-type-error
    si::check-type-error-datum
    si::check-type-error-expected-type
    si::check-type-error-place
    si::check-type-error-string
    ))

(defmacro assert (test-form &optional places (datum t datum-supplied-p) &rest args)
  "Ensure TEST-FORM evaluates to non-nil."
  `(tagbody
     #1=#:assertion-root
     (unless ,test-form
       (restart-case
           (error ,@(if datum-supplied-p
                      (list* datum args)
                      `("assert に失敗しました: ~S" ',test-form)))
         (continue (&rest new-values)
           :report (lambda (stream)
                     (format stream "~:[やり直す。~;~@*値を変更してやり直す。(~{~S~^ ~})~]"
                       ',places))
           :interactive (lambda ()
                          (mapcar (lambda (place)
                                    (prompt-for t "`~S' の値: " place))
                            ',places))
           (setf ,@(let ((i -1))
                     (mapcan (lambda (place)
                               `(,place (nth ,(incf i) new-values)))
                       places)))
           (go #1#))))))

(defmacro check-type (place typespec &optional string)
  "Ensure value of PLACE is type of TYPESPEC."
  `(tagbody
     #1=#:check-type-root
     (let ((#2=#:datum ,place))
       (unless ,(typespec+:optimize-type-check '#2# typespec)
         (restart-case
             (error 'si::check-type-error
                    :place ',place
                    :datum #2#
                    :expected-type ',typespec
                    :string ,string)
           (store-value (new-value)
             :report (lambda (stream)
                       (format stream "`~S' の値を変更してやり直す。" ',place))
             :interactive (lambda ()
                            (list (ed:read-value ',typespec "`~S' の値: " ',place)))
             (setf ,place new-value)
             (go #1#)))))))

(provide "assertions")
;;; assertions.l ends here.
