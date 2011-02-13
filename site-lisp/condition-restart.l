;;; -*- mode: lisp; package: condition-restart -*-
;;;
;;; condition-restart.l --- Common Lisp の再起動（Restart）
;;;
;;; Author:     bowbow99 <bowbow99@gmail.com>
;;; Version:    0.01.03
;;; Category:   Lisp Libraries
;;; License:    MIT (see COPYING)
;;; Commentary: (see README.md)
;;; ChangeLog:  (see ChangeLog)

;;; Code:

;;;;
;;;; * Package

(when (find-package :elisp)
  (shadow :abort :elisp))

(eval-when (:load-toplevel :compile-toplevel :execute)

  ;; NOTE: CL standard symbols are exported from lisp package.
  (export (mapcar (lambda (name) (intern (string name) :lisp))
            '(;; Restart establishing macros
              #:restart-case
              #:restart-bind
              #:with-simple-restart
              #:with-condition-restart
              ;; Restart manipulating functions
              #:restart-name
              #:compute-restart
              #:find-restart
              #:invoke-restart
              #:invoke-restart-interactively
              ;; Condition signaller
              #:cerror
              ;; Standard restart names
              #:abort
              #:continue
              #:muffle-warning
              #:use-value
              #:store-value
              ))
          :lisp)

  ;; NOTE: Export `prompt-for` from editor package to be visible from
  ;; most of user code.
  (export (mapcar (lambda (name) (intern (string name) :editor))
            '(#:prompt-for
              ))
          :editor)

  ) ; end of `eval-when`

(defpackage :condition-restart
  (:use :lisp)
  (:nicknames "restart"))

(in-package :condition-restart)

(defconstant +version+ "0.01.03")


;;;;
;;;; * Utilities

(defmacro with-ca/dr ((cons &optional (car-name 'car) (cdr-name 'cdr)) &body body)
  `(let ((#1=#:obj ,cons))
     (let ((,car-name (car #1#))
           (,cdr-name (cdr #1#)))
       ,@body)))

#+xyzzy
(setf (get 'with-ca/dr 'ed:lisp-indent-hook) 1)

(defun %add-key/value (key datum alist)
  (cons (cons key (union datum (cdr (assoc key alist))))
        (remove key alist :key #'car)))

;;;;
;;;; * Restart Object

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun print-restart (restart stream #:what?)
    (format stream "#<restart: ~A>"
      (or (restart-name restart)
          "(anonymous)"))))

(defstruct (restart
            (:print-function print-restart))
  name
  function
  interactive-function
  report-function
  (test-function (lambda (&optional condition) t)))

(defparameter *active-restarts* nil
  "List of active restart.")

(defparameter *condition/restart-association* nil
  "List of condition and associated restarts.")

;;;; * Restart Establishing Macros

;;; - Macro restart-bind

(defmacro restart-bind ((&rest restarts) &body body)
  "Evaluate the BODY in a dynamic environment where RESTARTS are in effect."
  `(let ((*active-restarts*
           (append (list ,@(mapcar (lambda (restart)
                                     `(make-restart :name ',(car restart)
                                                    :function ,(cadr restart)
                                                    ,@(cddr restart)))
                             restarts))
                   *active-restarts*)))
     ,@body))

#+xyzzy
(setf (get 'restart-bind 'ed:lisp-indent-hook) 1)

;;; - Macro with-condition-restarts

(defmacro with-condition-restarts (condition restarts &body body)
  "Evaluate BODY with associating RESTARTS with CONDITION."
  `(let ((*condition/restart-association*
           (%add-key/value ,condition ,restarts
                           *condition/restart-association*)))
     ,@body))

#+xyzzy
(setf (get 'with-condition-restarts 'ed:lisp-indent-hook) 2)

;;; - Macro restart-case
(eval-when (:load-toplevel :compile-toplevel :execute)
  (defun transform-keyword-option-report (form)
    (list :report-function
          (case (type-of form)
            (simple-string
             `(lambda (#1=#:stream) (format #1# ,form)))
            (symbol
             (if form
               `',form
               (error "Invalid function for :report option: ~S" form)))
            (cons ; lambda form
             (if (eql 'lambda (car form))
               form
               (error "Invalid function for :report option: ~S" form)))
            (t (error "Invalid function for :report option: ~S" form)))))

  (defun transform-keyword-option-test (form)
    (list :test-function `#',form))

  (defun transform-keyword-option-interactive (form)
    (list :interactive-function `#',form))

  (defun transform-keyword-option (keyword/form)
    (let ((keyword (car keyword/form))
          (form (cadr keyword/form)))
      (funcall (case keyword
                 (:report 'transform-keyword-option-report)
                 (:interactive 'transform-keyword-option-interactive)
                 (:test 'transform-keyword-option-test)
                 (t (error "Invalid keyword option for restart-case clause: ~S"
                           keyword)))
               form)))
  )

(defmacro %with-tag/clause ((tag/clause) &body body)
  `(let ((tag (first ,tag/clause))
         (name (second ,tag/clause))
         (args (third ,tag/clause))
         (options nil)
         (body (cdddr ,tag/clause)))
     (while (and (keywordp (car body))
                 (member (car body) '(:report :interactive :test)))
       (setf options (cons (subseq body 0 2) options)
             body (cddr body)))
     (setf options (nreverse options))
     ,@body))

#+xyzzy
(setf (get '%with-tag/clause 'ed:lisp-indent-hook) 'defun)

(defun %make-condition (datum args default)
  (case (type-of datum)
    ((simple-string string)
     (make-condition default :format-string datum :format-arguments args))
    (symbol
     (apply #'make-condition datum args))
    (t
     (if (and (typep datum 'condition) (null args))
       datum
       (error 'type-error
              :datum datum
              :expected-type '(or condition symbol string))))))

(defmacro restart-case (form &rest clauses)
  "Evaluate the FORM in a environment where clauses establishes restarts."
  (let ((tags/clauses (mapcar (lambda (clause)
                                (cons (gensym) clause))
                        clauses)))
    `(block #1=#:restart-case
       (let ((#2=#:args nil))
         (tagbody
           (restart-bind
               (,@(mapcar (lambda (tag/clause)
                            (%with-tag/clause (tag/clause)
                              `(,name (lambda (&rest #3=#:temp)
                                        (setq #2# #3#)
                                        (go ,tag))
                                ,@(mapcan #'transform-keyword-option options))))
                    tags/clauses))
             (return-from #1#
               ,(let ((form (macroexpand form)))
                  (if (and (consp form)
                           (member (car form) '(signal warn error cerror)))
                    (let ((operator (car form))
                          (datum (cadr form))
                          (args (cddr form)))
                      `(let ((#4=#:condition
                               (%make-condition ,datum (list ,@args)
                                                ',(case operator
                                                    ((error cerror) 'simple-error)
                                                    (signal 'simple-condition)
                                                    (warn 'simple-warning)))))
                         (with-condition-restarts #4#
                             (list ,@(mapcar (lambda (tag/clause)
                                               (%with-tag/clause (tag/clause)
                                                 `(find-restart ',name)))
                                       tags/clauses))
                           (,operator #4#))))
                    form))))
           ,@(mapcan (lambda (tag/clause)
                       (%with-tag/clause (tag/clause)
                         `(,tag (return-from #1#
                                  (apply #'(lambda ,args ,@body)
                                         #2#)))))
               tags/clauses))))))

#+xyzzy
(setf (get 'restart-case 'ed:lisp-indent-hook) 1)

(defmacro with-simple-restart ((name fmt-control &rest fmt-arguments)
                               &body forms)
  "Evaluate FORMS with a retart NAME which simply returns nil and t."
  `(restart-case
       (progn ,@forms)
     (,name ()
      :report (lambda (stream)
                (format stream ,fmt-control ,@fmt-arguments))
      (values nil t))))

;;;;
;;;; * Functions Use Active Restarts

(defun restart-applicable-p (restart condition)
  "Return true if restart is visible according to condition."
  (macrolet
      ((restart-is-associated-with-condition ()
         `(find restart (cdr (assoc condition *condition/restart-association*))))
       (restart-is-not-associated-at-all ()
         `(notany (lambda (association)
                    (member restart (cdr association)))
            *condition/restart-association*)))
    (and
     ;; check if its visible according to :test-function
     ;; NOTE: If test-function calls `compute-restarts` or `find-restarts`,
     ;; it invokes test-function once more and result in an infinite loop.
     ;; To prevent it, make the restart invisible while execution of its
     ;; test-function.
     (let ((*active-restarts* (remove restart *active-restarts*)))
       ;; NOTE: :test-function defaults to (constantly t)
       (funcall (restart-test-function restart) condition))
     ;; check association
     (or (null condition)
         (restart-is-associated-with-condition)
         (restart-is-not-associated-at-all)))))

(defun compute-restarts (&optional condition)
  "Return a list of restarts which are currently active and applicable."
  (remove-if-not #'(lambda (restart)
                     (restart-applicable-p restart condition))
    *active-restarts*))

(defun find-restart (identifier &optional condition)
  "Return a particular restart from current dynamic environment."
  (cond
   ((symbolp identifier)
    (find-if (lambda (restart)
               (and (eql (restart-name restart) identifier)
                    (restart-applicable-p restart condition)))
      *active-restarts*))
   ((restart-p identifier)
    (when (and (member identifier *active-restarts*)
               (restart-applicable-p identifier condition))
      identifier))
   (t (error "invalid restart designator: ~S" identifier))))

(define-condition restart-not-found (control-error)
  (designator condition)
  (:report (lambda (condition stream)
             (format stream "Could not find active restart ~S~@[ for condition ~S~]"
               (restart-not-found-designator condition)
               (restart-not-found-condition condition)))))
#+xyzzy
(setf (get 'restart-not-found 'ed::lisp-indent-handler) t)

(defun invoke-restart (identifier &rest arguments)
  "Call the function associated with RESTART with ARGUMENTS."
  (apply (restart-function (or (find-restart identifier)
                               (error 'restart-not-found
                                      :designator identifier)))
         arguments))

(defun invoke-restart-interactively (identifier)
  (let ((restart (or (find-restart identifier)
                     (error 'restart-not-found
                            :designator identifier))))
    (apply #'invoke-restart
           restart
           (let ((it (restart-interactive-function restart)))
             (when it (funcall it))))))


;;; - Interface Functions for Standard Restart

(defun abort (&optional condition)
  (let ((r (find-restart 'abort condition)))
    (if r (invoke-restart r) (quit))))

(defun continue (&optional condition)
  (let ((restart (find-restart 'continue condition)))
    (when restart
      (invoke-restart restart))))

(defun muffle-warning (&optional warning)
  (invoke-restart 'muffle-warning))

(defun use-value (value &optional condition)
  (let ((r (find-restart 'use-value condition)))
    (when r (invoke-restart r value))))

(defun store-value (new-value &optional condition)
  (let ((r (find-restart 'store-value condition)))
    (when r (invoke-restart r new-value))))


;;;;
;;;; * Signaling Condition with Restarts

(defun warn (datum &rest args)
  (let ((warning (%make-condition datum args 'simple-warning)))
    (restart-case
        (with-condition-restarts warning (list (find-restart 'muffle-warning))
          (si:*throw-error warning))
      (muffle-warning ()
        :report "この警告は無かったことに。"
        nil))))

(defun cerror (fmt-control datum &rest args)
  (let ((error (%make-condition datum args 'simple-error)))
    (restart-case
        (with-condition-restarts error (list (find-restart 'continue))
          (si:*throw-error error))
      (continue ()
        :report (lambda (stream)
                  (apply #'format stream fmt-control args))))))

;;;;
;;;; * Function ed:prompt-for

;; >>>
;; 注意事項: 本章中の多くの場所で使用されている関数 prompt-for は、Common
;; Lisp の一部ではない。表示を容易にするだけの理由で本章の中で使用されてい
;; るのである。型指定子、任意のフォーマット文字列と、それに関連する引数を
;; 受け入れることが仮定されている。それは、会話形式の入力促進の一部として
;; フォーマット文字列と関連する引数を使用し、Lisp オブジェクトを読むために
;; read を使用する。しかしながら、型指定子に指示された型のオブジェクトだけ
;; が受け入れられる。
;; <<< CLtL2 p.774
(defun ed:prompt-for (typespec prompt-control &rest args)
  "Ask user a value that is type of TYPESPEC."
  (labels ((again (fmt &rest margs)
             (apply #'ed:message fmt margs)
             (apply #'ed:prompt-for typespec prompt-control args)))
    (let ((form (ed:read-sexp (apply #'format nil prompt-control args))))
      (handler-case
          (let ((value (eval form)))
            (if (typep value typespec)
              value
              (again "`~S' は `~S' ではありません" value typespec)))
        (error (e)
          (again "Error: ~A" e))))))

(provide "condition-restart")
;;;; condition-restart.l ends here
