;;; -*- mode: lisp; package: lisp -*-
;;;
;;; assertions.l
;;;
;;; Author: bowbow99 <bowbow99@gmail.com>
;;;

;; 概要:
;; 再起動を使った Common Lisp の assert と check-type です。

;; 使い方:
;; require すればおｋです。
;;     (require :assertions)

;; 注意事項:
;; - 標準のマクロ check-type を上書きしています。
;;   再起動を使うようにした以外は標準のものと同じ挙動にしたつもりですが
;;   おかしくなっているかも知れません。
;;   - おかしかったら教えてもらえると助かります。
;;   - ほんのり遅くなってます。

;;; Code:

(require "condition-restart")

(in-package :lisp)

(export '(assert
          +check-type-error-symbols+
          ))

;;; NOTE: Since xyzzy defining check-type-error condition type in system
;;; package, they aren't visible in most packages, and you need to add
;;; package prefix ("si::") to slot accesser and such.
;;; To get rid of this, you can import relevant symbols by put following
;;; in your code.
;;;     (import +check-type-error-symbols+)
(defconstant +check-type-error-symbols+
  '(si::check-type-error si::check-type-error-datum si::check-type-error-expected-type
    si::check-type-error-place si::check-type-error-string))

(defmacro assert (test-form &optional places
                  (datum t datum-supplied-p) &rest args)
  "Ensure TEST-FORM evaluates to non-nil."
  `(tagbody
     #1=#:assertion-root
     (unless ,test-form
       (restart-case
           (error ,@(if datum-supplied-p
                        (list* datum args)
                      `("The assertion ~S failed." ',test-form)))
         (continue (&rest new-values)
           :report (lambda (stream)
                     (format stream "Retry assertion with new values for ~S"
                             ',places))
           :interactive (lambda ()
                          (mapcar (lambda (place)
                                    (eval (ed:read-sexp
                                           (format nil "New Value for ~S: "
                                                   place))))
                            ',places))
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
                    :place ',place :datum #2# :expected-type ',typespec)
           (store-value (new-value)
             :report (lambda (stream)
                       (format stream "Supply a new value for ~S" ',place))
             :interactive (lambda ()
                            (list
                             (eval (ed:read-sexp
                                    (format nil "New Value for ~S: " ',place)))))
             (setf ,place new-value)
             (go #1#)))))))

;;; assertions.l ends here
