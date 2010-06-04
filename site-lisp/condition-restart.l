;;; -*- mode: lisp; package: condition-restart -*-
;;;
;;; condition-restart.l
;;;
;;; Author: bowbow99 <bowbow99@gmail.com>
;;; Version: 0.00.**

;; 概要
;; ====
;; Common Lisp のコンディションシステムにある再起動 (Restart) です。

;; 使い方 for エンドユーザ
;; =======================
;; require すればおｋです。
;;     (require :condition-restart)

;; 使い方 for 開発者
;; =================
;; 再起動使うためのマクロやら関数やらについては、リファレンスを参照
;; してください。
;; - ${XYZZY}/reference/condition-restart.xml
;;
;; エラーがトップレベルまで飛んでいったときに、再起動を選択・起動で
;; きるようにするには、ed:select-restart-interactively を使います。

;; 注意事項
;; ========
;; - 標準の関数 warn を上書きしています。
;;   再起動を用意するようにした以外は標準のものと同じ挙動にしたつもり
;;   ですが、おかしくなっているかも知れません。

;;; Code:

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defpackage :condition-restart
    (:use :lisp)))

(when (find-package :elisp)
  (shadow :abort :elisp))

;;; exporting from lisp package
(in-package :lisp)

(export '(;; Macros establishes restarts
          restart-case
          restart-bind
          with-simple-restart
          with-condition-restarts
          ;; Functions deals with restarts
          restart-name
          compute-restarts
          find-restart
          invoke-restart
          invoke-restart-interactively
          ;; Signallers
          cerror
          ;; standard restart interface
          abort
          continue
          muffle-warning
          use-value
          store-value
          ))


(in-package :condition-restart)

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

(defun print-restart (restart stream #:what?)
  (format stream "#<restart: ~A>"
          (or (restart-name restart)
              "(anonymous)")))

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

(defun restart-active-p (restart)
  (find restart *active-restarts*))

(defun restart-associated-p (restart condition)
  "Return true if RESTART is associated with CONDITION, or RESTART isn't
 associated with any condition."
  (or (find restart (cdr (assoc condition *condition/restart-association*)))
      (notany (lambda (assoc)
                (member restart (cdr assoc)))
        *condition/restart-association*)))

;;;; * Restart Establishing Macros

;;; - Macro restart-bind

(defmacro restart-bind ((&rest restarts) &body body)
  "Evaluate the BODY in a dynamic environment where RESTARTS are in effect."
  `(let ((*active-restarts*
          (append (list ,@(mapcar (lambda (restart)
                                    `(make-restart :name ',(car restart)
                                                   :function #',(cadr restart)
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

(defun restart-test (restart condition)
  (or (null condition)
      (null (restart-test-function restart))
      (funcall (restart-test-function restart) condition)))

(defun compute-restarts (&optional condition)
  "Return a list of restarts which are currently active and applicable."
  (remove-if-not (lambda (restart)
                   (and (restart-test restart condition)
                        (restart-associated-p restart condition)))
    *active-restarts*))

(defun find-restart (identifier &optional condition)
  "Return a particular restart from current dynamic environment."
  (case (type-of identifier)
    (symbol
     (find-if (lambda (restart)
                (and (eql (restart-name restart) identifier)
                     (restart-test restart condition)
                     (restart-associated-p restart condition)))
       *active-restarts*))
    (structure
     (when (and (typep identifier 'restart)
                (member identifier *active-restarts*))
       identifier))))

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
  (invoke-restart (find-restart 'abort condition)))

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
        (with-condition-restarts warning (find-restart 'muffle-warning)
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

;;;; condition-restart.l ends here
