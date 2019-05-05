;;; menu-config.el --- menu Configurations

;;; Commentary:

;; Usage:
;; (require 'menu-config)

;;; Code:

(require 'recentf)
(setq recentf-max-saved-items 500
      recentf-max-menu-items 30
      recentf-exclude '("\\.emacs\\.d/elpa/.*\\.el\\'" "\\.el\\.gz\\'")
      recentf-filename-handlers '(file-truename abbreviate-file-name)
      tool-bar-max-label-size 12
      tool-bar-style 'image)
;; There are machine dependent configurations
(with-eval-after-load 'machine-config
  (recentf-mode 1))


(provide 'menu-config)
;;; menu-config.el ends here
