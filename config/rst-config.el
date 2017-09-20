;;; rst-config.el --- Configure rst

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'rst
;;   (require 'rst-config))
;; or:
;; (with-eval-after-load 'rst-config
;;   )
;; never:
;; (require 'rst-config)

;; Do not include in this file:
;; (require 'rst)

;;; Code:

(message "Importing rst-config")
;;;;;;;;;;;;
;; Sphinx ;;
;;;;;;;;;;;;
(require 'sphinx-frontend-config)


(provide 'rst-config)
;;; rst-config.el ends here
