;;; magit-popup-config.el --- Configure magit-popup

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'magit-popup
;;   (require 'magit-popup-config))

;; Do not include in this file:
;; (require 'magit-popup)

;;; Code:

(message "Importing magit-popup-config")

(face-spec-set 'magit-popup-argument
               '((t (:foreground "forest green" :weight bold))))
;; (face-spec-set 'magit-popup-disabled-argument
;;                '((t (:foreground "slate gray" :weight light))))


(provide 'magit-popup-config)
;;; magit-popup-config.el ends here
