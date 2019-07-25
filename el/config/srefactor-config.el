;;; srefactor-config.el --- Configure srefactor

;;; Commentary:

;; Usage:
;; (require 'srefactor-config)

;;; Code:

(message "Importing srefactor-config")
;;(require 'srefactor-bug)

(with-eval-after-load 'cc-mode
  (require 'srefactor)
  (setq srefactor-ui-menu-show-help nil
        srefactor--getter-prefix "get"
        srefactor--setter-prefix "set"
        srefactor--getter-setter-removal-prefix "_"
        srefactor--getter-setter-capitalize-p t)
  (define-key c++-mode-map (kbd "C-c s") #'srefactor-refactor-at-point)
  (define-key c-mode-map (kbd "C-c s") #'srefactor-refactor-at-point))


(with-eval-after-load 'elisp-mode
  (require 'srefactor-lisp)
  (mapc (lambda (x)
          (define-key lisp-mode-map
            (kbd (concat "C-c s " (car x))) (cdr x)))
        '(("o" . srefactor-lisp-one-line)
          ("m" . srefactor-lisp-format-sexp)
          ("d" . srefactor-lisp-format-defun)
          ("b" . srefactor-lisp-format-buffer))))

;; (define-key lisp-mode-map (kbd "M-RET o") 'srefactor-lisp-one-line)
;; (define-key lisp-mode-map (kbd "M-RET m") 'srefactor-lisp-format-sexp)
;; (define-key lisp-mode-map (kbd "M-RET d") 'srefactor-lisp-format-defun)
;; (define-key lisp-mode-map (kbd "M-RET b") 'srefactor-lisp-format-buffer)


(provide 'srefactor-config)
;;; srefactor-config.el ends here
