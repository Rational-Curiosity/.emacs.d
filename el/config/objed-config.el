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
        browse-kill-ring-mode))

(objed-mode)


(provide 'objed-config)
;;; objed-config.el ends here
