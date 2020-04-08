;;; helm-extensions-config.el --- Configure helm

;;; Commentary:

;; Usage:

;; (when (load "helm" t)
;;   (require 'helm-extensions-config))

;;; Code:
(setq helm-completion-mode-string ""
      helm-split-window-inside-p t
      helm-autoresize-max-height 60
      helm-autoresize-min-height 6)

(set-face-attribute 'helm-source-header nil
                    :family 'unspecified
                    :height 'unspecified
                    :weight 'unspecified
                    :foreground "white"
                    :background "slate gray")

;; (defun helm-insert-utf8 ()
;;   (interactive)
;;   (let ((utf8-hash-table (ucs-names)))
;;     (helm :sources
;;           `((name . "Unicode character by name")
;;             (candidates . ,(hash-table-keys utf8-hash-table))
;;             (action . (lambda (key) (insert (gethash key ,utf8-hash-table))))))))

(require 'helm-config)
(require 'helm-mode)
(helm-mode 1)
(helm-autoresize-mode 1)
(define-key global-map (kbd "C-x C-f") 'helm-find-files)
(define-key global-map [remap occur] 'helm-occur)
(define-key global-map [remap list-buffers] 'helm-buffers-list)
(define-key global-map (kbd "C-x C-r") 'helm-recentf)
(define-key global-map (kbd "M-g a") 'helm-ag)
(define-key global-map (kbd "M-y") 'helm-show-kill-ring)
(define-key global-map (kbd "C-x e") 'helm-execute-kmacro)
(define-key global-map (kbd "C-x b") 'helm-mini)
(define-key global-map (kbd "C-h SPC") 'helm-all-mark-rings)
(define-key global-map [remap insert-register] 'helm-register)
(define-key global-map [remap bookmark-jump] 'helm-filtered-bookmarks)
(define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
(define-key global-map [remap execute-extended-command] 'helm-M-x)
(define-key global-map [remap apropos-command] 'helm-apropos)
(unless (boundp 'completion-in-region-function)
  (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
  (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))

(with-eval-after-load 'company
  (define-key company-mode-map (kbd "C-:") 'helm-company)
  (define-key company-active-map (kbd "C-:") 'helm-company))

(with-eval-after-load 'flycheck
  (define-key flycheck-mode-map (kbd "C-c ! h") 'helm-flycheck))

(with-eval-after-load 'org
  (require 'helm-org)
  (add-to-list 'helm-completing-read-handlers-alist '(org-capture . helm-org-completing-read-tags))
  (add-to-list 'helm-completing-read-handlers-alist '(org-set-tags . helm-org-completing-read-tags)))

(with-eval-after-load 'projectile
  (require 'helm-projectile)
  (helm-projectile-on))

(require 'helm-elisp)
(setq helm-show-completion-display-function #'helm-show-completion-default-display-function)

(require 'helm-swoop)
(setq helm-swoop-split-with-multiple-windows t)
;; Change the keybinds to whatever you like :)
(global-set-key (kbd "M-i") 'helm-swoop)
(global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
(global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)
;; When doing isearch, hand the word over to helm-swoop
(define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
;; From helm-swoop to helm-multi-swoop-all
(define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
;; When doing evil-search, hand the word over to helm-swoop
;; (define-key evil-motion-state-map (kbd "M-i") 'helm-swoop-from-evil-search)
;; Instead of helm-multi-swoop-all, you can also use helm-multi-swoop-current-mode
(define-key helm-swoop-map (kbd "M-m") 'helm-multi-swoop-current-mode-from-helm-swoop)
;; Move up and down like isearch
(define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
(define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
(define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)
;; Go to the opposite side of line from the end or beginning of line
(setq helm-swoop-move-to-line-cycle t)

(add-hook 'helm-after-initialize-hook #'helm-init-relative-display-line-numbers)

(provide 'helm-extensions-config)
;;; helm-extensions-config.el ends here
