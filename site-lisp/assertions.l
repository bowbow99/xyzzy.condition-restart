;;;; -*- mode: lisp; package: lisp -*-
;;;;
;;;; assertions.l
;;;;

(require "condition-restart")

(in-package :lisp)

(export '(assert
          ;check-type
          ;ccase
          ;ctypecase
          ;ecase
          ;etypecase
          ))

(defmacro assert (test-form &optional places
                  (datum t datum-supplied-p) &rest args)
  `(tagbody
     #1=#:assertion-top
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
  `(tagbody
     #1=#:check-type-top
     (let ((#2=#:datum ,place))
       (restart-case
           (unless (typep #2# ',typespec)
             (error 'si::check-type-error
                    :place ',place :datum #2# :expected-type ',typespec))
         (store-value (new-value)
          :report
          (lambda (stream)
            (format stream "Supply a new value for ~S" ',place))
          :interactive (lambda ()
                         (list
                          (eval (ed:read-sexp
                                 (format nil "New Value for ~S: " ',place)))))
          (setf ,place new-value)
          (go #1#))))))

;;;; assertions.l ends here
