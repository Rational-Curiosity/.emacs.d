;;; compile-config.el --- Configure and improve compile

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'compile
;;   (require 'compile-config))
;; never:
;; (require 'compile-config)

;; Do not include in this file:
;; (require 'compile)

;;; Code:
(setq ;; compile-command "cbuild -g "
      compilation-scroll-output 'first-error)

;; (defun compilation-conditional-scroll-output ()
;;   (let ((name (buffer-name)))
;;     (if (and name
;;              (string-match-p "log" name))
;;         (set (make-local-variable 'compilation-scroll-output) nil))))
;; (add-hook 'compilation-mode-hook 'compilation-conditional-scroll-output)


(provide 'compile-config)
;;; compile-config.el ends here
