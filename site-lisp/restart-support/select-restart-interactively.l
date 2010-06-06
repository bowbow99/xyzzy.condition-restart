;;; -*- mode: lisp; package: editor -*-
;;;
;;; restart-support/select-restart-intractively.l
;;;     --- Interface to select and invoke restart interactively.
;;;
;;; This file is part of xyzzy extension 'condition-restart'.
;;;
;;; Author:    bowbow99 <bowbow99@gmail.com>
;;; License:   MIT
;;;

;;; Commentary:

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

;;;;
;;;; * Package

(in-package :editor)

(export '(select-restart-interactively))


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

;;; restart-support/select-restart-interactively.l ends here
