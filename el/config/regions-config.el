;;; regions-config.el --- Configure expand region

;;; Commentary:

;; Usage:
;; (require 'regions-config)

;;; Code:

(require 'subword)
(setq minor-mode-alist (assq-delete-all 'subword-mode minor-mode-alist))

(set-face-attribute 'region nil
                    :foreground 'unspecified
                    :background "DarkSlateGray"
                    :box '(:line-width -1 :color "CadetBlue" :style nil))

(require 'expand-region)

(setq expand-region-autocopy-register "º"
      expand-region-smart-cursor t
      expand-region-subword-enabled t)

(global-subword-mode)

(global-set-key (kbd "M-s r") #'er/expand-region)
(global-set-key (kbd "M-s s") #'er/mark-symbol)
(global-set-key (kbd "M-s d") #'mark-defun)
(global-set-key (kbd "M-s S") #'swap-regions)


(provide 'regions-config)
;;; regions-config.el ends here

