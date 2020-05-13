;;; tramp-config.el --- Configure tramp

;;; Commentary:

;; Usage:
;; (require 'tramp-config)

;;; Code:

(with-eval-after-load 'tramp
  (setq password-cache t
        password-cache-expiry 3600
        auth-sources '((:source "~/.emacs.d/authinfo.gpg"))
        tramp-default-method "ssh"))


(provide 'tramp-config)
;;; tramp-config.el ends here
