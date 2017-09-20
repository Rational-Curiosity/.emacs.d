;;; regions-config.el --- Configure expand region

;;; Commentary:

;; Usage:
;; (require 'regions-config)

;;; Code:

(require 'expand-region)
(bind-keys
 ("M-º" . er/expand-region))


(provide 'regions-config)
;;; regions-config.el ends here
