;;; emms-config.el --- Configure and improve emms

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'emms
;;   (require 'emms-config))
;; never:
;; (require 'emms-config)

;; Do not include in this file:
;; (require 'emms)

;;; Code:

(message "Importing emms-config")

(require 'emms-setup)
(emms-all)
(emms-default-players)

(provide 'emms-config)
;;; emms-config.el ends here
