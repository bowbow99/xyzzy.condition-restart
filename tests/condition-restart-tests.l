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
  "find-restart finds restart that is associated with the condition."
  (let ((error (make-condition 'simple-error)))
    (assert-eql 'foo (restart-bind ((foo (lambda () :foo)))
                       (with-condition-restarts error (list (find-restart 'foo))
                         (restart-name (find-restart 'foo error)))))))

(define-test find-restart-4
  "find-restart don't find restart that is associated another condition."
  (let ((error (make-condition 'simple-error))
        (warning (make-condition 'simple-warning)))
    (assert-eql nil (restart-bind ((foo (lambda () :foo)))
                      (with-condition-restarts error (list (find-restart 'foo))
                        (find-restart 'foo warning))))))

(define-test find-restart-5
  "find-restart also find restart that is associated with no condition."
  (let ((error (make-condition 'simple-error)))
    (assert-eql 'foo (restart-bind ((foo (lambda () :foo)))
                       (restart-name (find-restart 'foo error))))))

(define-test find-restart-6
  "find-restart find inner most restart."
  (assert-eql :inner (restart-bind ((foo (lambda () :outer)))
                       (restart-bind ((foo (lambda () :middle)))
                         (restart-bind ((foo (lambda () :inner)))
                           (invoke-restart (find-restart 'foo)))))))

;;; condition-restart-tests.l ends here.
