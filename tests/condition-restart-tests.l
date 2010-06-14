;;; -*- mode: lisp; package: user -*-
;;;
;;; condition-restart-tests.l
;;;

;;; Code:

(require "lisp-unit")
(use-package :lisp-unit)

(require "condition-restart")

(remove-all-tests)

;;;
;;; * restart-bind

(define-test restart-bind-1
  "restart-bind establish restarts with specified name."
  (restart-bind ((foo (lambda () :foo))
                 (bar (lambda () :bar)))
    (let ((restarts (compute-restarts)))
      (dolist (name '(foo bar))
        (assert-true (find name restarts :key 'restart-name)
            name restarts)))))

(define-test restart-bind-2
  "restart-bind establish restart which funcall given function on invocation."
  (restart-bind ((fred (lambda (x) (list :fred x))))
    (assert-equal '(:fred 3) (invoke-restart 'fred 3))))


;;;
;;; * find-restart

(define-test find-restart-1
  "find-restart finds restart by name."
  (assert-eql 'foo (restart-bind ((foo (lambda () :foo))
                                  (bar (lambda () :bar)))
                     (restart-name (find-restart 'foo))))
  (assert-eql 'bar (restart-bind ((foo (lambda () :foo))
                                  (bar (lambda () :bar)))
                     (restart-name (find-restart 'bar)))))

(define-test find-restart-2
  "find-restart returns nil if no restart found."
  (assert-eql nil (restart-bind ((foo (lambda () :foo))
                                 (bar (lambda () :bar)))
                    (find-restart 'fred))))

(define-test find-restart-3
  "find-restart don't find restart which isn't associated with the condition."
  (let ((error (make-condition 'simple-error)))
    (assert-eql nil (restart-bind ((foo (lambda () :foo)))
                      (with-condition-restarts error (list (find-restart 'foo))
                        (find-restart 'foo error))))))

;;; condition-restart-tests.l ends here.
