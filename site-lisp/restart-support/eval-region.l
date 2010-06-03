;;; -*- mode: lisp; package: editor -*-
;;;
;;; restart-support/eval-region.l
;;;
;;; This file is part of xyzzy extension 'condition-restart'.
;;;

;;; Commentary:

;; トップレベルまでエラーが飛んできたときに、再起動を選択して起動できる
;; eval-region です。lisp-mode とかのバッファにある lisp の式を評価する
;; いくつかの関数（コマンド）で再起動を選択できるようになります。
;;  - eval-print-last-sexp
;;  - eval-last-sexp
;;  - eval-defun
;;  - eval-buffer
;;
;; * 設定
;;     (require :restart-support/eval-region)
;;

;;; Code:

(require :condition-restart)
(require :restart-support/select-restart-interactively)

(in-package :editor)

(defun ed:eval-region (from to &optional stream)
  (interactive "r")
  (let ((result nil))
    (when (> from to)
      (rotatef from to))
    (let ((s (make-buffer-stream (selected-buffer) from to)))
      (handler-case
          (let ((*package* *package*))
            (while (< (buffer-stream-point s) to)
              (let ((form (read s nil '#1=#:eof)))
                (when (eq form '#1#)
                  (return))
                (setq result (multiple-value-list
                              (handler-bind ((condition 'select-restart-interactively))
                                (eval form)))))))
        ((or error reader-error quit) (c)
         (when (<= (buffer-stream-point s) to)
           (goto-char (buffer-stream-point s))
           (when stream
             (eval-region-print stream
               (format t "~A~%" c)))
           (let ((si:*trace-on-error* nil))
             (error c))))))
    (eval-region-print stream
      (if (null stream)
          (message "~S" (car result))
        (format t "~{~S~%~}" result)))
    (values-list result)))

;;; restart-support/eval-region.l ends here.
