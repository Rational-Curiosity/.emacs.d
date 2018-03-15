;;; figlet-config.el --- Configure figlet

;;; Commentary:

;; Usage:
;; (require 'figlet-config)

;;; Code:

(require 'figlet)

(setq figlet-default-font "banner"
      figlet-options
      '("-w" "90"))

(provide 'figlet-config)
;;; figlet-config.el ends here
