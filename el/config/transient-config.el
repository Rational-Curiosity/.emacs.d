;;; transient-config.el --- Configure transient

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'transient
;;   (require 'transient-config))

;; Do not include in this file:
;; (require 'transient)

;;; Code:

(message "Importing transient-config")

(face-spec-set 'transient-argument
               '((t (:foreground "forest green" :weight bold))))
(face-spec-set 'transient-value
               '((t (:inherit font-lock-string-face :weight bold))))


(provide 'transient-config)
;;; transient-config.el ends here
