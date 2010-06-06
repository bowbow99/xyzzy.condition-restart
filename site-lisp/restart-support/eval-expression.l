;;; -*- mode: lisp; package: editor -*-
;;;
;;; restart-support/eval-expression.l
;;;
;;; This file is part of xyzzy extension 'condition-restart'.
;;;
;;; Author:    bowbow99 <bowbow99@gmail.com>
;;; License:   MIT

;;; Commentary:

;; トップレベルまでエラーが飛んできたときに再起動を選択して起動できる
;; eval-expression (ESC ESC) です。
;;
;; 上書き注意報
;; ============
;; - eval-expression を上書きしています。オリジナルは lisp/cmds.l で
;;   定義されています。

;;; Code:

(provide "restart-support/eval-expression")

(require "restart-support/select-restart-interactively")

(in-package :editor)

(defun ed:eval-expression (x)
  (interactive "xEval: ")
  (restart-case
      (message "~S" (handler-bind ((condition 'select-restart-interactively))
                      (eval x)))
    (abort () :report "やめる。")))

;;; restart-support/eval-expression.l ends here.
