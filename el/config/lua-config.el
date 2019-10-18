;;; lua-config.el --- Configure and improve lua

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'lua-mode
;;   (require 'lua-config))
;; never:
;; (require 'lua-config)

;; Do not include in this file:
;; (require 'lua-mode)

;;; Code:
(message "Importing lua-config")
(setq lua-indent-level 4)

(provide 'lua-config)
;;; lua-config.el ends here
