;;; web-config.el --- Configure and improve web

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'web-mode
;;   (require 'web-config))
;; never:
;; (require 'web-config)

;; Do not include in this file:
;; (require 'web-mode)

;;; Code:

(message "Importing web-config")

(setq web-mode-enable-auto-indentation nil)


(provide 'web-config)
;;; web-config.el ends here
