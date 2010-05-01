;;;; -*- mode: lisp; package: condition-restart -*-
;;;;
;;;; condition-restart.l
;;;;

(eval-when (:load-toplevel :compile-toplevel :execute)
  (defpackage #:condition-restart
    (:use #:lisp)))

;;; exporting from lisp package
(in-package "lisp")

(export '(;; Macros establishes restarts
          restart-case
          restart-bind
          with-simple-restart
          ;; Functions deals with restarts
          compute-restarts
          find-restart
          invoke-restart
          invoke-restart-interactively
          ;; Signallers
          cerror
          assert
          ;; standard restart interface
          abort
          continue
          muffle-warning
          use-value
          store-value
          ))

(in-package "condition-restart")

;;;;
;;;; * Utilities

(defun %condition-type (condition)
  (si:*structure-definition-name
   (si:*structure-definition condition)))


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
  test-function)

(defvar *active-restarts* nil)

;; NOTE:
;; Since xyzzy's typep doesn't return true for structure instance
;; of *old* structure definition after structure has re-defined,
;; it is neccessary to re-instantiate it when this file has been
;; reloaded.
(defparameter +global-abort+
  (make-restart
   :name 'abort
   :function (lambda () (signal 'silent-quit))
   :report-function (lambda (stream)
                      (format stream "Stop evaluation, return to toplevel."))))


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
                  (or *active-restarts*
                      (list +global-abort+)))))
     ,@body))

#+xyzzy
(setf (get 'restart-bind 'ed:lisp-indent-hook) 1)

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
#+xyzzy (setf (get '%with-tag/clause 'ed:lisp-indent-hook) 'defun)

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
                              `(,name
                                (lambda (&rest #3=#:temp)
                                  (setq #2# #3#)
                                  (go ,tag))
                                ,@(mapcan #'transform-keyword-option
                                    options))))
                    tags/clauses))
             (return-from #1# ,form))
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

(defun invoke-restart-interactively (restart)
  (apply #'invoke-restart
         restart
         (let ((it (restart-interactive-function restart)))
           (when it (funcall it)))))


;;; - Interface Functions for Standard Restart

(defun abort (&optional condition)
  (invoke-restart 'abort condition))

(defun continue (&optional condition)
  (let ((cont (find-restart 'continue condition)))
    (when cont
      (invoke-restart cont))))

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
  (restart-case
      (lisp::*error datum args 'simple-warning)
    (muffle-warning () nil)))

(defun cerror (fmt-control datum &rest args)
  (restart-case
      (apply #'error datum args)
    (continue ()
      :report (lambda (stream)
                (format stream fmt-control args)))))

;;;;
;;;; * eval-region with restart support
;;;;

(defun %print-condition (condition &optional (stream *standard-output*))
  (format stream "~&~A~%   [Condition Type: ~S]~%"
          condition
          (%condition-type condition)))


(defun %print-restart-options (restarts &optional (stream *standard-output*))
  (format stream "~&Retart Options:~%")
  (let ((i -1))
    (dolist (restart restarts)
      (format stream "~&~4D [~A] "
              (incf i)
              (or (restart-name restart) "(Anonymous)"))
      (let ((report-fn (restart-report-function restart)))
        (if report-fn
            (progn (funcall report-fn stream) (fresh-line stream))
          (format stream "~A~&" (or (restart-name restart) "")))))))


(in-package "editor")

(defmacro with-other-window (&body forms)
  `(save-window-excursion
    (if (= 1 (count-windows))
        (split-window (let ((col (window-columns)))
                        (round (if (< 140 col) col (window-height)) -2))
                      t)
      (other-window))
    ,@forms))

#+xyzzy (setf (get 'with-other-window 'ed:lisp-indent-hook) 'defun)

(defun select-restart-interactively (condition)
  (let ((restarts (compute-restarts)))
    (when restarts
      (with-other-window
        (with-output-to-temp-buffer ("*RESTART*" nil)
          (set-buffer (buffer-stream-buffer *standard-output*))
          (condition-restart::%print-condition condition)
          (format t "~&~%")
          (condition-restart::%print-restart-options restarts)
          ;; TODO: obtain and print CALL STACK
          (let ((i (handler-case
                       (read-integer "Restart: ")
                     (quit (q)
                       (handler-case
                           (invoke-restart 'abort)
                         (condition-restart::restart-not-found ()
                           nil))))))
            (let ((it (and i (nth i restarts))))
              (invoke-restart-interactively it))))))))

(defun eval-region (from to &optional stream)
  (interactive "r")
  (let ((result nil))
    (when (> from to)
      (rotatef from to))
    (let ((s (make-buffer-stream (selected-buffer) from to)))
      (handler-case
          (let ((*package* *package*))
            (while (< (buffer-stream-point s) to)
              (let ((form (read s nil '#1=#:eof)))
                (when (eq form '#1#)
                  (return))
                (setq result (multiple-value-list
                              (handler-bind ((serious-condition 'select-restart-interactively))
                                (eval form)))))))
        ((or error reader-error quit) (c)
         (when (<= (buffer-stream-point s) to)
           (goto-char (buffer-stream-point s))
           (when stream
             (eval-region-print stream
                                (format t "~A~%" c)))
           (let ((si:*trace-on-error* nil))
             (error c))))))
    (eval-region-print stream
                       (if (null stream)
                           (message "~S" (car result))
                         (format t "~{~&=> ~S~%~}" result)))
    (values-list result)))


;;;; condition-restart.l ends here
