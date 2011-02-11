;;; -*- mode: lisp; package: lisp -*-
;;;
;;; assertions.l --- Common Lisp の assert など
;;;
;;; このファイルは condition-restart の付属品です。
;;;
;;; Author:     bowbow99  <bowbow99@gmail.com>
;;; License:    MIT (see COPYING)
;;; Commentary:

;;; Code:

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "condition-restart"))

(in-package :lisp)

(export '(assert
          +check-type-error-symbols+
          ))

;;;;
;;;; * Assertions

(defconstant +check-type-error-symbols+
  '(si::check-type-error
    si::check-type-error-datum
    si::check-type-error-expected-type
    si::check-type-error-place
    si::check-type-error-string
    ))

(defun read-value-for (place)
  (restart-case
      (eval (ed:read-sexp (format nil "`~S' の値: " place)))
    (re-enter ()
      :report (lambda (stream)
                (format stream "`~S' の値を入力し直す。" place))
      (read-value-for place))))

(defmacro assert (test-form &optional places (datum t datum-supplied-p) &rest args)
  "Ensure TEST-FORM evaluates to non-nil."
  `(tagbody
     #1=#:assertion-root
     (unless ,test-form
       (restart-case
           (error ,@(if datum-supplied-p
                      (list* datum args)
                      `("assert に失敗しました: ~S" ',test-form)))
         (continue (&rest new-values)
           :report (lambda (stream)
                     (format stream "~:[やり直す。~;~@*値を変更してやり直す。(~{~S~^ ~})~]"
                       ',places))
           :interactive (lambda ()
                          (mapcar #'read-value-for ',places))
           (setf ,@(let ((i -1))
                     (mapcan (lambda (place)
                               `(,place (nth ,(incf i) new-values)))
                       places)))
           (go #1#))))))

(defmacro check-type (place typespec &optional string)
  "Ensure value of PLACE is type of TYPESPEC."
  `(tagbody
     #1=#:check-type-root
     (let ((#2=#:datum ,place))
       (unless (typep #2# ',typespec)
         (restart-case
             (error 'si::check-type-error
                    :place ',place
                    :datum #2#
                    :expected-type ',typespec
                    :string ,string)
           (store-value (new-value)
             :report (lambda (stream)
                       (format stream "`~S' の値を変更してやり直す。" ',place))
             :interactive (lambda ()
                            (list (read-value-for ',place)))
             (setf ,place new-value)
             (go #1#)))))))

;;; assertions.l ends here.
