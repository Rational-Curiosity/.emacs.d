;;; php-config.el --- Configure and improve php

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'php-mode
;;   (require 'php-config))
;; or:
;; (with-eval-after-load 'php-config
;;   )
;; never:
;; (require 'php-config)

;; Do not include in this file:
;; (require 'php-mode)

;;;; Install dependencies
;; sudo apt install php<x.x>

;;;; Configure
;; cd <project root>
;; touch .ac-php-conf.json

;;; Code:

(message "Importing php-config")

(when (load "company-php" t)
  ;; Enable ElDoc support (optional)
  (ac-php-core-eldoc-setup)
  (add-to-list 'company-backends 'company-ac-php-backend)
  ;; Jump to definition (optional)
  (define-key php-mode-map (kbd "M-.") 'ac-php-find-symbol-at-point)

  ;; Return back (optional)
  (define-key php-mode-map (kbd "M-,") 'ac-php-location-stack-back))

;;;;;;;;;;
;; Keys ;;
;;;;;;;;;;
(define-key php-mode-map (kbd "M-b") nil)


(provide 'php-config)
;;; php-config.el ends here
