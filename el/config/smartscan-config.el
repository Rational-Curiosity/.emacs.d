;;; smartscan-config.el --- Configure smartscan

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'smartscan
;;   (require 'smartscan-config))

;;; Code:
(require 'config-lib)
(advice-add 'smartscan-symbol-goto :around #'message-silent-advice)

(define-key smartscan-map (kbd "M-n") nil)
(define-key smartscan-map (kbd "C-c C-n") 'smartscan-symbol-go-forward)
(define-key smartscan-map (kbd "M-p") nil)
(define-key smartscan-map (kbd "C-c C-p") 'smartscan-symbol-go-backward)
(define-key smartscan-map (kbd "M-'") nil)
(define-key smartscan-map (kbd "C-c C-r") 'smartscan-symbol-replace)

(global-set-key (kbd "<f5>") 'smartscan-mode)


(provide 'smartscan-config)
;;; smartscan-config.el ends here
