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
;; - restart:eval-region
;; - restart:eval-expression

;;;;
;;;; *** OLD COMMENTARY of restart-support/select-restart-interactively.l ***

;; 概要
;; ====
;; ウィンドウを分割して選択可能な再起動の一覧を表示し、ミニバッファから
;; 再起動を選択るす何かです。
;; eval-expression のような、任意の lisp コードを実行するコマンドを実装
;; する場合に使うことを想定しています。
;;
;; handler-bind にそのまま使えるようになってるので、以下のようにすると
;; <DO WHATEVER> でエラーが投げられたときに再起動を選択できます。
#|
(handler-bind ((condition 'select-restart-interactively))
  <DO WHATEVER>)
|#
;; select-restart-interactively は再起動を対話的に起動します。リファレン
;; ス（reference/condition-restart.xml）の invoke-restart-interactively
;; を参照してください。
;;
;; 再起動 abort について。
;; -----------------------
;; 任意の lisp コードを実行（評価）する際に、コードの実行を止める再起動
;; abort があることを想定しています。（C-g とかすると abort 再起動を起動
;; しようとします。）
;; 「トップレベル」風なものを作るときはできれば abort 再起動を用意しとい
;; てください。
#|
(defun my-eval-something (sexp)
  (restart-case
      (handler-bind ((condition 'select-restart-interactively))
        (eval sexp))
    (abort ()
      :report "やめる。"
      ;; 何もしなければ restart-case から nil が返る。
      ;; 必要なら return-from や go で制御を飛ばす。
      (return-from my-eval-something (values)))))
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
          restart::eval-expression)
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
                                  (return-from eval-region nil))))))))
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

(defun restart:eval-expression (x)
  (interactive "xEval: ")
  (restart-case
      (message "~S" (handler-bind ((condition 'select-restart-interactively))
                      (eval x)))
    (abort () :report "やめる。" (values))))

;;; restart-support/select-restart-interactively.l ends here
