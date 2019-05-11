;;; tramp-config.el --- Configure tramp

;;; Commentary:

;; Usage:
;; (require 'tramp-config)

;;; Code:

(require 'tramp)

(setq password-cache-expiry nil
      tramp-default-method "ssh")


(provide 'tramp-config)
;;; tramp-config.el ends here
