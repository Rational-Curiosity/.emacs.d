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

(defun eglot-xor-semantic ()
  (semantic-mode (if (eglot-managed-p)
                     -1
                   1)))
(setq mode-line-misc-info
      (delq (assoc 'eglot--managed-mode
                   mode-line-misc-info)
            mode-line-misc-info)
      ;; performance
      read-process-output-max (* 3 1024 1024)
      ;; eglot
      eglot-events-buffer-size 50000
      eglot-sync-connect nil
      eglot-ignored-server-capabilites '(:documentHighlightProvider)
      eglot-managed-mode-hook (nconc
                               '(eglot-xor-semantic
                                 (lambda ()
                                   (eldoc-mode -1)))
                               eglot-managed-mode-hook))



(provide 'eglot-config)
;;; eglot-config.el ends here
