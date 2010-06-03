;;; -*- mode: lisp; package: lisp -*-
;;;
;;; restart-support/load-file.l
;;;
;;; This file is part of xyzzy extension 'condition-restart'.
;;;
(require :restart-support/select-restart-interactively)

(in-package :lisp)

(defun load-file (filename)
  (ed:interactive
      (let ((ed:ignored-extensions
             (remove ".lc" ed:ignored-extensions :test #'string-equal)))
        (list (ed:read-exist-file-name "Load file: "))))
  (handler-bind ((condition 'ed:select-restart-interactively))
    (load filename)))

(defun mc-load-file (filename &optional encoding)
  (ed:interactive
      (list (let ((ed:ignored-extensions
                   (remove ".lc" ed:ignored-extensions :test #'string-equal)))
              (ed:read-exist-file-name "Load file: "))
            (and ed:*prefix-value*
                 (ed:read-char-encoding "Encoding: "))))
  (unless (file-exist-p filename)
    (error 'file-not-found
           :datum "ƒtƒ@ƒCƒ‹‚ªŒ©‚Â‚©‚è‚Ü‚¹‚ñ"
           :pathname filename))
  (let (buffer
        (loading (if (ed:string-matchp "\\.lc$" filename)
                     "loading" "LOADING")))
    (unwind-protect
        (ed:save-excursion
          (setq buffer (ed:create-new-buffer " *load file*"))
          (ed:set-buffer buffer)
          (let ((ed:*expected-fileio-encoding*
                 (or encoding
                     (ed::find-file-auto-encoding filename)
                     ed:*expected-fileio-encoding*)))
            (declare (special ed:*expected-fileio-encoding*))
            (ed:message "~A ~A..." loading filename)
            (ed:insert-file-contents filename)
            (handler-bind ((condition 'ed:select-restart-interactively))
              (load (ed:make-buffer-stream buffer) :verbose nil))
            (ed:message "~A ~A...done" loading filename)))
      (when buffer
        (ed:delete-buffer buffer)))))


#|
list of lisp file loading functions:

using load directly
--------------------
* load-file
* mc-load-file

using si:*load-library directly
--------------------------------
- load-library
- require
- autoload (load-and-go)

via mc-load-file
----------------
- mc-load-library
- mc-require
- mc-autoload

|#

;;; restart-support/load-file.l