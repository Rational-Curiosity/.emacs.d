;;; config-lib.el --- Library for configurations

;;; Commentary:

;; Usage:
;; (require 'config-lib)

;;; Code:

(require 'cl-lib)
;; [ get current function name
;; thanks to: https://emacs.stackexchange.com/a/2312
(defun call-stack ()
  "Return the current call stack frames."
  (let ((frames)
        (frame)
        (index 5))
    (while (setq frame (backtrace-frame index))
      (push frame frames)
      (incf index))
    (remove-if-not 'car frames)))

(defmacro compile-time-function-name ()
  "Get the name of calling function at expansion time."
  (symbol-name
   (cl-cadadr
    (cl-caddr
     (cl-find-if (lambda (frame)
                (ignore-errors (equal (car (cl-caddr frame)) 'defalias)))
              (reverse (call-stack)))))))
;; ]

;; Converts calls to COMPOSE to lambda forms with everything written
;; out and some things written as direct function calls.
;; Example: (compose #'1+ #'2* #'-) => (LAMBDA (X) (1+ (2* (- X))))
(cl-define-compiler-macro compose (&rest functions)
  (cl-labels ((sharp-quoted-p (x)
                           (and (listp x)
                                (eql (cl-first x) 'function)
                                (symbolp (cl-second x)))))
    `(lambda (x) ,(cl-reduce #'(lambda (fun arg)
                         (if (sharp-quoted-p fun)
                             (list (cl-second fun) arg)
                           (list 'funcall fun arg)))
                     functions
                     :initial-value 'x
                     :from-end t))))

;; Eval checking bound before
(defmacro bound-and-eval (func &rest args)
  "Ensures FUNC exist and eval with ARGS."
  `(and (fboundp ,func) (eval (list ,func ,@args))))

;; Silent messages
;; Usage:
;; (advice-add '<orig-fun> :around #'message-silent-advice)
(defun message-silent-advice (orig-fun &rest args)
  "Silent and eval ORIG-FUN with ARGS."
  (let ((message-log-max nil)
        (inhibit-message t))
    (apply orig-fun args)))

;; Truncate messages
;; Usage:
;; (advice-add '<orig-fun> :around #'message-truncate-advice)
(defun message-truncate-advice (orig-fun &rest args)
  "Stablish `message-truncate-lines' and eval ORIG-FUN with ARGS."
  (let ((message-truncate-lines t))
    (apply orig-fun args)))


(provide 'config-lib)
;;; config-lib.el ends here
