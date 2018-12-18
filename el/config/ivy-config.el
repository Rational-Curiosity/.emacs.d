;;; ivy-config.el --- Configure ivy-mode

;;; Commentary:

;; Usage:
;; (require 'ivy-config)

;;; Code:

(require 'ivy)
(setcar (cdr (assq 'ivy-mode minor-mode-alist)) "")
(ivy-mode 1)
(setq ivy-use-virtual-buffers nil
      enable-recursive-minibuffers t
      ivy-count-format "(%d/%d) ")
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c j") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
(global-set-key (kbd "C-x C-r") 'counsel-recentf)
(global-set-key (kbd "C-h SPC") 'counsel-mark-ring)
(global-set-key (kbd "C-x r b") 'counsel-bookmark)
(global-set-key (kbd "M-y") 'counsel-yank-pop)

(provide 'ivy-config)
;;; ivy-config.el ends here
