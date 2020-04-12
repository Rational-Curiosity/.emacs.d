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
(add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)

(set-face-attribute 'lsp-face-highlight-read nil
                    :inherit 'unspecified
                    :underline t
                    :weight 'bold
                    :background 'unspecified)
(set-face-attribute 'lsp-face-highlight-write nil
                    :inherit 'unspecified
                    :underline t
                    :weight 'bold
                    :background 'unspecified)

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
(when (and (featurep 'company)
           (load "company-lsp" t))
  (push 'company-lsp company-backends))

(setq lsp-enable-xref nil ;; lsp-enable-xref t suppress etags--xref-backend
      ;; performance
      gc-cons-threshold 100000000
      read-process-output-max (* 3 1024 1024)
      lsp-prefer-capf t
      lsp-idle-delay 0.5
      ;; lsp
      lsp-keymap-prefix "s-l"
      lsp-diagnostic-package :flycheck
      lsp-file-watch-ignored (cons "[/\\\\]tmp$"
                                   lsp-file-watch-ignored)
      lsp-signature-auto-activate nil ;; <xor signature>
      ;; lsp-ui-doc
      lsp-ui-doc-enable nil
      lsp-ui-doc-include-signature t  ;; <xor signature>
      lsp-ui-doc-position 'top
      lsp-ui-doc-alignment 'frame
      lsp-ui-doc-max-height 10
      lsp-ui-doc-max-width 80
      ;; lsp-ui-sideline
      lsp-ui-sideline-delay 0.5
      lsp-ui-sideline-show-hover t)

(defun lsp-ui-or-xref-find-definitions ()
  (interactive)
  (condition-case nil
      (call-interactively 'lsp-ui-peek-find-definitions)
    (error (call-interactively 'xref-find-definitions))))

(defun lsp-ui-or-xref-find-references ()
  (interactive)
  (condition-case nil
      (call-interactively 'lsp-ui-peek-find-references)
    (error (call-interactively 'xref-find-references))))

(define-key lsp-ui-mode-map [remap xref-find-definitions] 'lsp-ui-or-xref-find-definitions)
(define-key lsp-ui-mode-map [remap xref-find-references] 'lsp-ui-or-xref-find-references)


(provide 'lsp-config)
;;; lsp-config.el ends here
