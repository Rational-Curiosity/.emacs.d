;;; objed-config.el --- Configure objed

;;; Commentary:

;; Usage:

;;
;; (require 'objed-config)

;; sudo apt install fd-find
;; sudo apt install ripgrep
;;; Code:

(setq objed-disabled-modes
      '(exwm-mode
        browse-kill-ring-mode
        completion-list-mode)
      objed-mode-line-format
      '(:eval (propertize
               (format "%s(%s)"
                       (symbol-name objed--object)
                       (char-to-string (aref (symbol-name objed--obj-state) 0)))
               'face 'objed-mode-line))
      objed-cursor-color "#ff8c00"
      objed-use-hl nil)

(objed-mode)


(provide 'objed-config)
;;; objed-config.el ends here
