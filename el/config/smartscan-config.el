;;; smartscan-config.el --- Configure smartscan

;;; Commentary:

;; Usage:
;; (require 'smartscan-config)

;;; Code:

(defvar smartscan-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-n") 'smartscan-symbol-go-forward)
    (define-key m (kbd "C-c C-p") 'smartscan-symbol-go-backward)
    (define-key m (kbd "C-c C-r") 'smartscan-symbol-replace)
    m)
  "Keymap for `smartscan'.")

(global-smartscan-mode 1)

(require 'config-lib)
(advice-add 'smartscan-symbol-goto :around #'message-silent-advice)


(provide 'smartscan-config)
;;; smartscan-config.el ends here
