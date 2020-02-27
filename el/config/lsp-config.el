;;; lsp-config.el --- Configure and improve lsp

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'lsp-mode
;;   (require 'lsp-config))
;; never:
;; (require 'lsp-config)

;; Do not include in this file:
;; (require 'lsp-mode)

;;; Code:

(message "Importing lsp-config")

(set-face-attribute 'lsp-face-highlight-read nil :underline nil)

(when (bug-check-function-bytecode
       'lsp-mode-line
       "wCCJgxcAwcLDAyLExcYDAyO2glCCHgDBx8jJyiNQhw==")
  (require 'subr-x)
  (defun lsp-mode-line ()
    "Construct the mode line text."
    (if-let (workspaces (lsp-workspaces))
        (string-join (--map (format "[%s]" (lsp--workspace-print it))
                            workspaces))
      (propertize "[Disconnected]" 'face 'warning))))

(require 'lsp-ui)
(require 'lsp-ui-doc)
(require 'lsp-pyls)
(when (and (featurep 'company)
           (load "company-lsp" t))
  (push 'company-lsp company-backends))

(setq lsp-enable-xref nil  ;; lsp-enable-xref t suppress etags--xref-backend
      lsp-diagnostic-package :auto
      lsp-flycheck-live-reporting nil
      lsp-file-watch-ignored
      (cons "[/\\\\]tmp$"
            lsp-file-watch-ignored)
      lsp-signature-auto-activate nil ;; <xor signature>
      ;; lsp-ui
      lsp-ui-doc-include-signature t  ;; <xor signature>
      lsp-ui-doc-position 'top
      lsp-ui-doc-alignment 'frame
      ;; lsp-pyls
      lsp-pyls-plugins-flake8-enabled t)

(define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
(define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)


(provide 'lsp-config)
;;; lsp-config.el ends here
