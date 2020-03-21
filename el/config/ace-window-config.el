;;; ace-window-config.el --- Translation tools

;;; Commentary:

;; Usage:
;; after (require 'mode-line-config)
;; (require 'ace-window-config)

;;; Code:
(require 'ace-window)
(set-face-attribute 'aw-mode-line-face nil
                    :bold t
                    :foreground "red")

(setq aw-scope 'global
      aw-minibuffer-flag t
      aw-background t)

(ace-window-display-mode)

(global-set-key (kbd "M-o") 'ace-window)


(provide 'ace-window-config)
;;; ace-window-config.el ends here
