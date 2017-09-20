;;; theme-config.el --- Setup emacs theme

;;; Commentary:

;; Usage:
;; (require 'theme-config)

;;; Code:

;; (set 'custom-enabled-themes 'wheatgrass)
(load-theme 'wheatgrass t)

(with-eval-after-load 'mode-line-config
  (set-face-background 'mode-line "dark slate gray")
  (set-face-background 'mode-line-inactive "gray14"))

(provide 'theme-config)
;;; theme-config.el ends here
