;;; tramp-config.el --- Configure tramp

;;; Commentary:

;; Usage:
;; (require 'tramp-config)

;;; Code:

(require 'tramp)
(require 'config-lib)

(setq password-cache-expiry nil
      tramp-default-method "ssh")

(bound-and-eval 'config-02)

(provide 'tramp-config)
;;; tramp-config.el ends here
