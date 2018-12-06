;;; nim-config.el --- Configure nim-mode

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'nim-mode
;;   (require 'nim-config))

;;; Code:

(with-eval-after-load 'nim-suggest
  (setcar (cdr (assq 'nimsuggest-mode minor-mode-alist)) ""))


(provide 'nim-config)
;;; nim-config.el ends here
