;;; -*- mode: lisp; package: editor -*-
;;;
;;; restart-support/eval-expression.l
;;;

(require :restart-support/select-restart-interactively)

(defun ed:eval-expression (x)
  (interactive "xEval: ")
  (restart-case
      (message "~S" (handler-bind ((condition 'select-restart-interactively))
                      (eval x)))
    (abort () :report "‚â‚ß‚éB")))

;;; restart-support/eval-expression.l ends here.
