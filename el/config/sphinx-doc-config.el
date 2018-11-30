;;; sphinx-doc-config.el --- Configure and improve sphinx doc

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'sphinx-doc
;;   (require 'sphinx-doc-config))
;; or:
;; (with-eval-after-load 'sphinx-doc-config
;;   )
;; never:
;; (require 'sphinx-doc-config)

;; Do not include in this file:
;; (require 'sphinx-doc)

;;; Code:

(message "Importing sphinx-doc")
(setcar (cdr (assq 'sphinx-doc-mode minor-mode-alist)) "")

;;;;;;;;;;;;;;;;
;; Sphinx doc ;;
;;;;;;;;;;;;;;;;
(cl-defstruct sphinx-doc-doc
  (summary "") ; summary line that fits on the first line
  before-fields                                ; list of comments before fields
  after-fields                                 ; list of comments after fields
  fields)                                      ; list of field objects


(provide 'sphinx-doc-config)
;;; sphinx-doc-config.el ends here
