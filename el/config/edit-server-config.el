;;; edit-server-config.el --- Configure edit-server

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'edit-server
;;   (require 'edit-server-config))

;; Do not include in this file:
;; (require 'edit-server)

;;; Code:

(message "Importing edit-server-config")

(setcar (cdr (assq 'edit-server-edit-mode minor-mode-alist)) "Es")

(setq edit-server-url-major-mode-alist
      '(("github\\.com" . markdown-mode)
        ("gitlab\\."    . markdown-mode)))


(provide 'edit-server-config)
;;; edit-server-config.el ends here
