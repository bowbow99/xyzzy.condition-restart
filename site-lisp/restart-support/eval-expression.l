;;; -*- mode: lisp; package: editor -*-
;;;
;;; restart-support/eval-expression.l
;;;

(require :restart-support/select-restart-interactively)

(defun ed:eval-expression (x)
  (interactive "xEval: ")
  (message "~S" (handler-bind ((condition 'select-restart-interactively))
                  (eval x))))

;;; restart-support/eval-expression.l ends here.
