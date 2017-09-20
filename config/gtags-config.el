;;; gtags-config.el --- Configure gtags

;;; Commentary:

;;; Code:

;;;;;;;;;;;;;
;;  gtags  ;;
;;;;;;;;;;;;;

;; Enable helm-gtags-mode
(dolist (hook '(c-mode-hook
               c++-mode-hook
               asm-mode-hook))
  (add-hook hook 'helm-gtags-mode))

;; ;; customize
;; (custom-set-variables
;;  '(helm-gtags-path-style 'relative)
;; ; '(helm-gtags-ignore-case t)
;;  '(helm-gtags-auto-update t)
;;  )

;; key bindings
(with-eval-after-load 'helm-gtags
  (bind-keys
              ("C-c g d" . helm-gtags-dwim)
              ("C-c g u" . helm-gtags-pop-stack)
              ("C-c g t" . helm-gtags-find-tag)
              ("C-c g r" . helm-gtags-find-rtag)
              ("C-c g s" . helm-gtags-find-symbol)
              ("C-c g <" . helm-gtags-previous-history)
              ("C-c g >" . helm-gtags-next-history)
              ("C-c g h" . helm-gtags-show-stack)
              ("C-c g a" . helm-gtags-tags-in-this-function)
              ("C-c g p" . helm-gtags-parse-file))
  (setcar (cdr (assq 'helm-gtags-mode minor-mode-alist)) "Hg"))


(provide 'gtags-config)
;;; gtags-config.el ends here
