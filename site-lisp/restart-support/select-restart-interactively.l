;;; -*- mode: lisp; package: editor -*-
;;;
;;; restart-support/select-restart-intractively.l
;;;

(require :condition-restart)

(in-package :editor)

(export '(select-restart-interactively))

(defmacro %with-other-window (&body forms)
  `(save-excursion
     (save-window-excursion
       (if (= 1 (count-windows))
           (let ((col (window-columns)))
             (if (> col 140)
                 (split-window (round col -2) t)
               (split-window (round (window-height) -2))))
         (other-window))
       ,@forms)))

(defun %condition-type (condition)
  (si:*structure-definition-name
   (si:*structure-definition condition)))

(defun %report-condition (condition &optional (stream *standard-output*))
  (format stream "~&~S:~%    ~A"
          (%condition-type condition)
          condition))

(defun %print-restart-options (restarts &optional (stream *standard-output*))
  (let ((i -1))
    (dolist (restart restarts t)
      (format stream "~&~3D [~7A] "
              (incf i)
              (or (restart-name restart) "(–¼–³‚µ‚³‚ñ)"))
      (let ((report-fn (condition-restart::restart-report-function restart)))
        (if report-fn
            (progn (funcall report-fn stream) (fresh-line stream))
          (format stream "~A~&" (or (restart-name restart) "")))))))

#+xyzzy
(setf (get 'with-other-window 'ed:lisp-indent-hook) 'defun)

(defun select-restart-interactively (condition)
  (let ((restarts (compute-restarts condition))
        i)
    (when restarts
      (%with-other-window
        (with-output-to-temp-buffer (" *Select Restart*" nil)
          (set-buffer (buffer-stream-buffer *standard-output*))
          (%report-condition condition)
          (format t "~&~%")
          (%print-restart-options restarts)
          ;; TODO: obtain and print CALL STACK
          (handler-case
              (setq i (read-integer "Restart: "))
            (quit (q) nil))))
      (if (and (numberp i)
               (condition-restart::restart-p (nth i restarts)))
          (invoke-restart-interactively (nth i restarts))
        (abort)))))

;;; restart-support/select-restart-interactively.l ends here
