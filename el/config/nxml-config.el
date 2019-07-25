;;; nxml-config.el --- Configure nxml

;;; Commentary:

;; Usage:
;; (require 'nxml-config)

;;; Code:

(defun xml-format (start end)
  "Format xml START END region or entire buffer."
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (point-min) (point-max))))
  (if (executable-find "xmllint")
      (shell-command-on-region start end
                               "xmllint --format -" t t)
    (error "Cannot find xmllint command")))

(global-set-key (kbd "C-c x f") #'xml-format)


(provide 'nxml-config)
;;; nxml-config.el ends here
