;;; menu-config.el --- menu Configurations

;;; Commentary:

;; Usage:
;; (require 'menu-config)

;;; Code:

(require 'recentf)
(setq recentf-max-saved-items 500
      recentf-max-menu-items 30
      recentf-exclude '("\\.emacs\\.d/elpa/.*\\.el\\'" "\\.el\\.gz\\'")
      recentf-filename-handlers '(file-truename abbreviate-file-name))
;; There are machine dependent configurations
(with-eval-after-load 'machine-config
  (recentf-mode 1))
;; [ Se encarga helm
;;(global-set-key "\C-x\ \C-r" 'recentf-open-files)
;; ]
(set 'tool-bar-max-label-size 12)
(set 'tool-bar-style 'image)


(provide 'menu-config)
;;; menu-config.el ends here
