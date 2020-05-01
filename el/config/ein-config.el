;;; ein-config.el --- Configure and improve ein

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'ein-core
;;   (require 'ein-config))
;; never:
;; (require 'ein-config)

;; Do not include in this file:
;; (require 'ein-core)

;;; Code:

(message "Importing ein-config")

(setq ein:output-area-inlined-images t)


(provide 'ein-config)
;;; ein-config.el ends here
