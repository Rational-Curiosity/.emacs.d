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

;;; Code:

(message "Importing php-config")

(when (load "company-php" t)
  (add-to-list 'company-backends 'company-ac-php-backend))


(provide 'php-config)
;;; php-config.el ends here
