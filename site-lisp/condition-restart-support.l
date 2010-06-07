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
;; *scratch* とかでエラーったら再起動選択
(require "condition-restart")
(require "condition-restart-support")

(defconstant +original-eval-region+ #'eval-region)
(defconstant +original-eval-expression+ #'eval-expression)

(setf (symbol-function 'eval-region) #'restart:eval-region
      (symbol-function 'eval-expression) #'restart:eval-expression)
|#
;; 上記の設定を .xyzzy とかに書いておくと、以下のコマンドで lisp 式を評
;; 価してエラーったときに、再起動を選択して起動できるようになります。
;;
;; - eval-print-last-sexp  lisp-interaction-mode の C-j
;; - eval-last-sexp        lisp-mode とかの C-x C-e
;; - eval-defun            lisp-mode とかの M-C-x
;; - eval-buffer
;; - eval-expression       グローバルの ESC ESC
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

(provide "restart-support/select-restart-interactively")

(require "condition-restart")

(export '(restart::eval-region
          restart::eval-expression
          restart::eval-print-last-sexp
          restart::eval-last-sexp
          restart::eval-defun
          restart::eval-buffer)
        :restart)
;;;;
;;;; * Package

(in-package :editor)

(export '(select-restart-interactively))

;;;;
;;;; * Utilities

(defmacro %with-other-window (&body forms)
  `(save-excursion
     (save-window-excursion
       (if (= 1 (count-windows))
           (let ((col (window-columns)))
             (if (> col 140)
                 (split-window (round col -2) t)
               (split-window (round (window-height) -2))))
         (other-window))
       ,@forms)))

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

#+xyzzy
(setf (get 'with-other-window 'ed:lisp-indent-hook) 'defun)

;;;;
;;;; * Select and Invoke Restart Interactively

(defun select-restart-interactively (condition)
  (let ((restarts (compute-restarts condition))
        i)
    (when restarts
      (%with-other-window
        (with-output-to-temp-buffer (" *Select Restart*" nil)
          (set-buffer (buffer-stream-buffer *standard-output*))
          (%report-condition condition)
          (format t "~&~%")
          (%print-restart-options restarts)
          ;; TODO: obtain and print CALL STACK
          (handler-case
              (setq i (read-integer "Restart: "))
            (quit (q) nil))))
      (if (and (numberp i)
               (condition-restart::restart-p (nth i restarts)))
          (invoke-restart-interactively (nth i restarts))
        (abort)))))

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

(defun eval-buffer (buffer)
  (interactive "bEval Buffer: ")
  (save-excursion
    (set-buffer buffer)
    (eval-region (point-min) (point-max))))

(defun eval-last-sexp (&optional stream)
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
        (eval-region from to
                     (cond ((or (streamp stream)
                                (bufferp stream))
                            stream)
                           (stream
                            (selected-buffer))
                           (t nil)))))))

(defun eval-print-last-sexp ()
  (interactive "*")
  (with-output-to-selected-buffer
    (let ((*standard-input* *cooked-mode-input-stream*))
      (when (fresh-line)
        (goto-char (buffer-stream-point *standard-output*))
        (refresh-screen))
      (eval-last-sexp *standard-output*))))

(defun eval-defun (&optional (arg nil f))
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
        (eval-region from (point) (if f (selected-buffer) nil))))))

;;; Originally defined in lisp/cmds.l

(defun restart:eval-expression (x)
  (interactive "xEval: ")
  (restart-case
      (message "~S" (handler-bind ((condition 'select-restart-interactively))
                      (eval x)))
    (abort () :report "やめる。" (values))))

;;; restart-support/select-restart-interactively.l ends here
