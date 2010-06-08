;;; -*- mode: lisp; package: editor -*-
;;;
;;; condition-restart-support.l --- Select and invoke restart interactively.
;;;
;;; This file is part of xyzzy extension 'condition-restart'.
;;;
;;; Author:    bowbow99 <bowbow99@gmail.com>
;;; License:   MIT
;;;

;;; Commentary:

;; 概要
;; ====
;; エラーがトップレベルまで飛んでいった時に、再起動を選択して起動できる
;; コマンドとかです。
;;

;; 能書き
;; ======
;; 標準の関数を上書きしたりする感じになってしまうので、今のところ激しく
;; 自己責任でお願いしますという類のものです。
;;

;; 使い方 1: *scratch* とかで
;; ==========================
;; lisp 書くけどよくわかんない、あるいはとりあえず動かしてみたい人向け。
#|
(require "condition-restart-support")
(restart:setup-condition-restart-support)
|#
;; 上記の設定を .xyzzy とかに書いておくと、いくつかのキーバインドを変更
;; して、以下のコマンドを使えるようになります。
;;
;; - restart:eval-print-last-sexp  lisp-interaction-mode の C-j(LFD)
;; - restart:eval-last-sexp        lisp-mode とかの C-x C-e
;; - restart:eval-defun            lisp-mode とかの M-C-x
;; - restart:eval-buffer
;; - restart:eval-expression       グローバルの ESC ESC
;;

;; 使い方 2: 任意のコマンドで
;; ==========================
;; 自分用の eval-print-last-sexp みたいなものを定義するような人向け。
;;
;; - 全てのコンディションを select-restart-interactively で処理するよう
;;   に handler-bind で包みます。
;; - 常に abort 再起動が用意されるようにします。
#|
    (restart-case
        (handler-bind ((condition 'select-restart-interactively))
          <任意の処理>)
      (abort ()
        :report "処理をやめる。"
        ;; 何もしなければ restart-case 式から nil が返る。それじゃマズ
        ;; い場合は return-from とか go で適当なところへ飛ばす。
        (values)))
|#


;;; TODO:
;; - abort が無かったら quit とか。
;; - INTERACTIVE-FUNCTION で引数取得してる間は、再起動選択バッファ（とい
;;   うかエラーメッセージ）を表示しておきたい。
;; - CALL STACK

;;; Code:

(provide "condition-restart-support")

(eval-when (:load-toplevel :compile-toplevel :execute)
  (require "condition-restart"))

(eval-when (:load-toplevel :compile-toplevel :execute)
  (export '(restart::eval-region
            restart::eval-expression
            restart::eval-print-last-sexp
            restart::eval-last-sexp
            restart::eval-defun
            restart::eval-buffer
            restart::setup-condition-restart-support)
          :restart))

;;;;
;;;; * Package

(in-package :editor)

(export '(select-restart-interactively))

;;;;
;;;; * Utilities

(defun %condition-type (condition)
  (si:*structure-definition-name
   (si:*structure-definition condition)))

(defun %report-condition (condition &optional (stream *standard-output*))
  (format stream "~&~S:~%    ~A"
          (%condition-type condition)
          condition))

(defun %print-restart-options (restarts &optional (stream *standard-output*))
  (let ((i -1))
    (dolist (restart restarts t)
      (format stream "~&~3D [~7A] "
              (incf i)
              (or (restart-name restart) "(名無しさん)"))
      (let ((report-fn (condition-restart::restart-report-function restart)))
        (if report-fn
            (progn (funcall report-fn stream) (fresh-line stream))
          (format stream "~A~&" (or (restart-name restart) "")))))))

;;;;
;;;; * Select and Invoke Restart Interactively

(defun select-restart-interactively (condition)
  (let ((restarts (compute-restarts condition)))
    (unless restarts
      (return-from select-restart-interactively nil))
    (with-output-to-temp-buffer (" *Select Restart*" nil)
      (let ((buffer (buffer-stream-buffer *standard-output*))
            (winconf (current-window-configuration)))
        (unwind-protect
            (progn
              ;; prepare window
              (unless (get-buffer-window buffer)
                (if (= 1 (count-windows))
                    (let ((col (window-columns)))
                      (if (> col 140)
                          (split-window (round col -2) t)
                        (split-window (round (window-height) -2))))
                  (other-window))
                (set-buffer buffer))
              ;; print condition and restarts
              ;; TODO: obtain and print CALL STACK
              (%report-condition condition)
              (format t "~&~%")
              (%print-restart-options restarts)
              ;; ask and invoke a restart
              (handler-case
                  (let ((i nil)
                        (prompt (format nil "Restart[0-~S]: "
                                        (1- (length restarts)))))
                    (loop
                      (setq i (read-integer prompt))
                      (if (and (numberp i) (< -1 i (length restarts)))
                          (return)
                        (message "Invalid number of restart: ~S" i)))
                    (handler-bind ((condition 'select-restart-interactively))
                      (invoke-restart-interactively (nth i restarts))))
                (quit (q) (abort))))
          (set-window-configuration winconf))))))


;;;;
;;;; * Restart Supporting `Eval' Commands

;;; Originally defined in lisp/lispmode.l

(defun restart:eval-region (from to &optional stream)
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
                              (restart-case
                                  (handler-bind ((condition 'select-restart-interactively))
                                    (eval form))
                                (abort ()
                                  :report "やめる。"
                                  (return-from restart:eval-region (values)))))))))
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

(defun restart:eval-buffer (buffer)
  (interactive "bEval Buffer: ")
  (save-excursion
    (set-buffer buffer)
    (restart:eval-region (point-min) (point-max))))

(defun restart:eval-last-sexp (&optional stream)
  (interactive "p")
  (let ((syntab (syntax-table)))
    (let (from to)
      (cond ((selection-start-end (start end)
               (setq from start to end)))
            (t
             (use-syntax-table *lisp-mode-syntax-table*)
             (unwind-protect
                 (let ((opoint (point)))
                   (setq from (progn
                                (backward-sexp)
                                (point))
                         to (progn
                              (forward-sexp)
                              (point)))
                   (goto-char (max opoint to)))
               (use-syntax-table syntab))))
      (save-excursion
        (restart:eval-region from to
                             (cond ((or (streamp stream)
                                        (bufferp stream))
                                    stream)
                                   (stream
                                    (selected-buffer))
                                   (t nil)))))))

(defun restart:eval-print-last-sexp ()
  (interactive "*")
  (with-output-to-selected-buffer
    (let ((*standard-input* *cooked-mode-input-stream*))
      (when (fresh-line)
        (goto-char (buffer-stream-point *standard-output*))
        (refresh-screen))
      (restart:eval-last-sexp *standard-output*))))

(defun restart:eval-defun (&optional (arg nil f))
  (interactive "p")
  (save-excursion
    (let ((opoint (point)))
      (end-of-defun)
      (let ((to (point))
            (from (progn
                    (beginning-of-defun)
                    (point))))
        (end-of-defun)
        (goto-char (if (< (point) opoint) opoint to))
        (restart:eval-region from (point)
                             (if f (selected-buffer) nil))))))

;;; Originally defined in lisp/cmds.l

(defun restart:eval-expression (x)
  (interactive "xEval: ")
  (restart-case
      (message "~S" (handler-bind ((condition 'select-restart-interactively))
                      (eval x)))
    (abort () :report "やめる。" (values))))


(defun restart:setup-condition-restart-support ()
  ;; for lisp-interaction-mode
  (define-key ed::*lisp-interaction-mode-map* #\LFD 'restart:eval-print-last-sexp)
  (define-key ed::*lisp-interaction-mode-map* #\C-M-x 'restart:eval-defun)
  (define-key ed::*lisp-interaction-mode-map* '(#\C-x #\C-e) 'restart:eval-last-sexp)
  ;; for lisp-mode
  (define-key ed::*lisp-mode-map* '(#\C-x #\C-e) 'restart:eval-last-sexp)
  (define-key ed::*lisp-mode-map* #\C-M-x 'restart:eval-defun)
  ;; global
  (define-key esc-map #\ESC 'restart:eval-expression)
  t)

;;; restart-support/select-restart-interactively.l ends here
