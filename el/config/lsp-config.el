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

(add-hook 'lsp-after-open-hook #'lsp-enable-imenu)
(add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
(when (fboundp 'lsp-origami-try-enable)
  (add-hook 'lsp-after-open-hook #'lsp-origami-try-enable))

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

(require 'lsp-pyls)
(setq lsp-enable-xref nil ;; lsp-enable-xref t suppress etags--xref-backend
      ;; performance
      gc-cons-threshold 100000000
      read-process-output-max (* 3 1024 1024)
      lsp-completion-provider :capf
      lsp-idle-delay 2.0
      lsp-enable-file-watchers nil
      lsp-file-watch-threshold 500
      ;; snippet is slow
      lsp-enable-snippet nil
      ;; lsp log
      lsp-log-io nil    ;; `(lsp-workspace-show-log)' Display the log buffer
      lsp-log-max 1000  ;; Max lines in the log buffer
      ;; lsp
      lsp-keymap-prefix "s-l"
      ;; flycheck option bug https://github.com/emacs-lsp/lsp-ui/issues/347
      lsp-diagnostics-provider :flymake
      lsp-modeline-code-actions-enable nil
      lsp-modeline-diagnostics-enable nil
      lsp-file-watch-ignored (cons "[/\\\\]tmp$"
                                   lsp-file-watch-ignored)
      ;; signature
      lsp-signature-render-documentation nil
      ;; lsp-pyls
      ;; [ Fix https://github.com/palantir/python-language-server/issues/771
      ;;   changing autopep8 for yapf
      lsp-pyls-plugins-autopep8-enabled nil
      lsp-pyls-plugins-yapf-enabled t
      ;; ]
      )


;; [ lsp-ui
(if (require 'lsp-ui nil 'noerror)
    (progn (add-hook 'lsp-mode-hook #'lsp-ui-mode)

           (setq lsp-ui-doc-enable nil
                 lsp-signature-auto-activate nil ;; <xor signature>
                 lsp-ui-doc-include-signature t  ;; <xor signature>
                 lsp-ui-doc-position 'top
                 lsp-ui-doc-alignment 'frame
                 lsp-ui-doc-max-height 10
                 lsp-ui-doc-max-width 80
                 ;; lsp-ui-sideline
                 lsp-ui-sideline-delay 0.5
                 lsp-ui-sideline-show-hover t)
           (when (or (eq lsp-diagnostics-provider :flymake)
                     (and (eq lsp-diagnostics-provider :auto)
                          (not (locate-library "flycheck"))))
             ;; not working with flymake
             (setq lsp-ui-sideline-show-diagnostics nil))

           (defun lsp-ui-or-xref-find-definitions ()
             (interactive)
             (let (result)
               (condition-case nil
                   (setq result (call-interactively 'lsp-ui-peek-find-definitions))
                 (error (call-interactively 'xref-find-definitions)))
               (if (and (stringp result)
                        (string-equal "Not found" (substring result 0 9)))
                   (call-interactively 'xref-find-definitions))))

           (defun lsp-ui-or-xref-find-references ()
             (interactive)
             (let (result)
               (condition-case nil
                   (setq result (call-interactively 'lsp-ui-peek-find-references))
                 (error (call-interactively 'xref-find-references)))
               (if (and (stringp result)
                        (string-equal "Not found" (substring result 0 9)))
                   (call-interactively 'xref-find-references))))

           (define-key lsp-ui-mode-map [remap xref-find-definitions] 'lsp-ui-or-xref-find-definitions)
           (define-key lsp-ui-mode-map [remap xref-find-references] 'lsp-ui-or-xref-find-references))

  (defun lsp-or-xref-find-definition ()
    (interactive)
    (let (result)
      (condition-case nil
          (setq result (call-interactively 'lsp-find-definition))
        (error (call-interactively 'xref-find-definitions)))
      (if (and (stringp result)
               (string-equal "Not found" (substring result 0 9)))
          (call-interactively 'xref-find-definitions))))

  (defun lsp-or-xref-find-references ()
    (interactive)
    (let (result)
      (condition-case nil
          (setq result (call-interactively 'lsp-find-references))
        (error (call-interactively 'xref-find-references)))
      (if (and (stringp result)
               (string-equal "Not found" (substring result 0 9)))
          (call-interactively 'xref-find-references))))

  (define-key lsp-mode-map [remap xref-find-definitions] 'lsp-or-xref-find-definition)
  (define-key lsp-mode-map [remap xref-find-references] 'lsp-or-xref-find-references))
;; ]

;; [ company-lsp -- not supported by lsp-mode
;; (when (and (featurep 'company)
;;            (load "company-lsp" t))
;;   (push 'company-lsp company-backends)
;;   (setq company-lsp-cache-candidates 'auto))
;; ]

;; [ dap-mode
(when (locate-library "dap-mode")
  (defun dap-enable-which-key-integration ()
    (apply
     #'which-key-add-major-mode-key-based-replacements
     major-mode
     (lsp--prepend-prefix
      (list "d" "debugging"
            "d m" "dap hydra menu"
            "d d" "start dap debugging"
            "d t" "toggle dap tooltip mode"))))
  (add-hook 'lsp-mode-hook #'dap-enable-which-key-integration)
  (define-key lsp-command-map (kbd "d m") #'dap-hydra)
  (define-key lsp-command-map (kbd "d d") #'dap-debug)
  (define-key lsp-command-map (kbd "d t") #'dap-tooltip-mode))
;; ]


(provide 'lsp-config)
;;; lsp-config.el ends here
