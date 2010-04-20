;;;; -*- mode: lisp; package: condition-restart -*-
;;;;
;;;; condition-restart.l
;;;;


(eval-when (:load-toplevel :compile-toplevel :execute)
  (defpackage #:condition-restart
    (:use #:lisp)))

;;; exporting from lisp package
(in-package "lisp")

(export '(restart-case
          restart-bind
          compute-restarts
          find-restart
          invoke-restart
          with-simple-restart
          ))


(in-package "condition-restart")

(require "utilities")
(use-package "utilities")

;;;
;;; * Restart Object
;;;

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
  test-function)

(defvar *active-restarts* nil)

;;;
;;; * Macro restart-bind
;;;
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

;;;
;;; * Macro restart-case
;;;

(defun transform-keyword-option-report (form)
  (list :report-function
        (case (type-of form)
          (simple-string
           (with-gensyms*
             `(lambda (,$stream)
                (format ,$stream ,form))))
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
#+xyzzy (setf (get '%with-tag/clause 'ed:lisp-indent-hook) 'defun)

(defmacro restart-case (form &rest clauses)
  "Evaluate the FORM in a environment where clauses establishes restarts."
  (with-gensyms*
    (let ((tags/clauses (mapcar (lambda (clause)
                                  (cons (gensym) clause))
                          clauses)))
      `(block ,$restart-case
         (let ((,$args nil))
           (tagbody
             (restart-bind
                 (,@(mapcar (lambda (tag/clause)
                              (%with-tag/clause (tag/clause)
                                `(,name
                                  (lambda (&rest ,$temp)
                                    (setq ,$args ,$temp)
                                    (go ,tag))
                                  ,@(mapcan #'transform-keyword-option
                                      options))))
                      tags/clauses))
               (return-from ,$restart-case ,form))
             ,@(mapcan (lambda (tag/clause)
                         (%with-tag/clause (tag/clause)
                           `(,tag (return-from ,$restart-case
                                    (apply #'(lambda ,args ,@body)
                                           ,$args)))))
                 tags/clauses)))))))

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

;;;
;;; * Functions Use Active Restarts
;;;;

(defun restart-test (restart condition)
  (or (null condition)
      (null (restart-test-function restart))
      (funcall (restart-test-function restart) condition)))

(defun compute-restarts (&optional condition)
  "Return a list of restarts which are currently active and applicable."
  (remove-if-not (lambda (restart)
                   (restart-test restart condition))
    *active-restarts*))


(defun find-restart (identifier &optional condition)
  "Return a particular restart from current dynamic environment."
  (case (type-of identifier)
    (symbol
     (find-if (lambda (restart)
                (and (eql identifier (restart-name restart))
                     (restart-test restart condition)))
       *active-restarts*))
    (structure
     (when (and (typep identifier 'restart)
                (member identifier *active-restarts*))
       identifier))))

(define-condition restart-not-found (error)
  (designator)
  (:report (lambda (condition stream)
             (format stream "Could not find active restart: ~S"
                     (restart-not-found-designator condition)))))

(defun invoke-restart (identifier &rest arguments)
  "Call the function associated with RESTART with ARGUMENTS."
  (apply (restart-function (or (find-restart identifier)
                               (error 'restart-not-found
                                      :designator identifier)))
         arguments))


;;;
;;; 
;;;

#|
CLHS ‚ÅŠÖŒW‚ ‚è‚»‚¤‚È‚à‚Ì
-------------------------
- restart-case
- restart-bind
- with-simple-restart
- invoke-restart
- find-restart
- invoke-restart-interactively
- Glossary#"restart function"
- abort, continue, muffle-warning, store-value, use-value

- there's a system class named RESTART
- restart function has name, possiblly be anonymous (or nil)
- when restart-case nested (and name shadowed?), most inner one
  get invoked

|#
;;;; condition-restart.l ends here
