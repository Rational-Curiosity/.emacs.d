;;; regions-config.el --- Configure expand region

;;; Commentary:

;; Usage:
;; (require 'regions-config)

;;; Code:

(require 'subword)
(setcar (cdr (assq 'subword-mode minor-mode-alist)) "")

(require 'expand-region)

(setq expand-region-autocopy-register "º"
      expand-region-smart-cursor t
      expand-region-subword-enabled t)

(global-subword-mode)

(global-set-key (kbd "M-º") #'er/expand-region)


(provide 'regions-config)
;;; regions-config.el ends here

