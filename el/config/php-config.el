;;; php-config.el --- Configure php and improve it

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'php
;;   (require 'php-config))
;; never:
;; (require 'php-config)

;; Do not include in this file:
;; (require 'php)

;;; Code:

(setq php-mode-coding-style 'psr2)

(c-mode)
(php-set-style "psr2")

;; Keys
(define-key php-mode-map (kbd "M-b") nil)
(define-key php-mode-map (kbd "C-.") nil)


(provide 'php-config)
;;; php-config.el ends here
