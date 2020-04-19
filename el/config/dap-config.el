;;; dap-config.el --- Configure and improve dap

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'dap-mode
;;   (require 'dap-config))
;; never:
;; (require 'dap-config)

;; Do not include in this file:
;; (require 'dap-mode)

;;; Code:

(message "Importing dap-config")

(dap-ui-mode)
(dap-tooltip-mode)


(provide 'dap-config)
;;; dap-config.el ends here
