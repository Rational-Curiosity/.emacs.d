;;; eglot-config.el --- Configure and improve eglot

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'eglot-mode
;;   (require 'eglot-config))
;; never:
;; (require 'eglot-config)

;; Do not include in this file:
;; (require 'eglot-mode)

;;; Code:

(message "Importing eglot-config")

(setq mode-line-misc-info
      (delq (assoc 'eglot--managed-mode
                   mode-line-misc-info)
            mode-line-misc-info)
      eglot-sync-connect nil
      eglot-ignored-server-capabilites '(:documentHighlightProvider))



(provide 'eglot-config)
;;; eglot-config.el ends here
