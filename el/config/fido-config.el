;;; fido-config.el --- Configure fido

;;; Commentary:

;; Usage:

;;
;; (require 'fido-config)

;; sudo apt install fd-find
;; sudo apt install ripgrep
;;; Code:

(require 'browse-kill-ring)
(require 'icomplete)

(set-face-attribute 'icomplete-first-match nil :foreground "YellowGreen")

(setq browse-kill-ring-display-duplicates nil
      browse-kill-ring-display-leftmost-duplicate nil
      browse-kill-ring-highlight-current-entry t
      browse-kill-ring-resize-window t
      icomplete-prospects-height 4)

(browse-kill-ring-default-keybindings)

(rg-enable-default-bindings "\M-ga")

(global-set-key (kbd "M-g f") 'fd-find)
(global-set-key (kbd "M-s O") 'multi-occur)

(fido-mode)


(provide 'fido-config)
;;; fido-config.el ends here
