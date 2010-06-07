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

;; �T�v
;; ====
;; �E�B���h�E�𕪊����đI���\�ȍċN���̈ꗗ��\�����A�~�j�o�b�t�@����
;; �ċN����I���邷�����ł��B
;; eval-expression �̂悤�ȁA�C�ӂ� lisp �R�[�h�����s����R�}���h������
;; ����ꍇ�Ɏg�����Ƃ�z�肵�Ă��܂��B
;;
;; handler-bind �ɂ��̂܂܎g����悤�ɂȂ��Ă�̂ŁA�ȉ��̂悤�ɂ����
;; <DO WHATEVER> �ŃG���[��������ꂽ�Ƃ��ɍċN����I���ł��܂��B
#|
(handler-bind ((condition 'select-restart-interactively))
  <DO WHATEVER>)
|#
;; select-restart-interactively �͍ċN����Θb�I�ɋN�����܂��B���t�@����
;; �X�ireference/condition-restart.xml�j�� invoke-restart-interactively
;; ���Q�Ƃ��Ă��������B
;;
;; �ċN�� abort �ɂ��āB
;; -----------------------
;; �C�ӂ� lisp �R�[�h�����s�i�]���j����ۂɁA�R�[�h�̎��s���~�߂�ċN��
;; abort �����邱�Ƃ�z�肵�Ă��܂��B�iC-g �Ƃ������ abort �ċN�����N��
;; ���悤�Ƃ��܂��B�j
;; �u�g�b�v���x���v���Ȃ��̂����Ƃ��͂ł���� abort �ċN����p�ӂ��Ƃ�
;; �Ă��������B
#|
(defun my-eval-something (sexp)
  (restart-case
      (handler-bind ((condition 'select-restart-interactively))
        (eval sexp))
    (abort ()
      :report "��߂�B"
      ;; �������Ȃ���� restart-case ���� nil ���Ԃ�B
      ;; �K�v�Ȃ� return-from �� go �Ő�����΂��B
      (return-from my-eval-something (values)))))
|#

;;; TODO:
;; - abort ������������ quit �Ƃ��B
;; - INTERACTIVE-FUNCTION �ň����擾���Ă�Ԃ́A�ċN���I���o�b�t�@�i�Ƃ�
;;   �����G���[���b�Z�[�W�j��\�����Ă��������B
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
              (or (restart-name restart) "(����������)"))
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