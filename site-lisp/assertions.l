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


(defmacro assert (test-form &optional places datum &rest args)
  `(tagbody
     #1=#:assertion-top
     (restart-case
         (unless ,test-form
           (error ,datum ,@args))
       (continue
        (&rest new-values)
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
        (go #1#)))))



;;;; assertions.l ends here
