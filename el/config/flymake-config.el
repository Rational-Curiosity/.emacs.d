;;; flymake-config.el --- Configure and improve flymake

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'flymake
;;   (require 'flymake-config))
;; never:
;; (require 'flymake-config)

;; Do not include in this file:
;; (require 'flymake)

;;; Code:

(message "Importing flymake-config")

(define-key flymake-mode-map (kbd "M-g n") #'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-g M-n") #'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-g p") #'flymake-goto-prev-error)
(define-key flymake-mode-map (kbd "M-g M-p") #'flymake-goto-prev-error)


(provide 'flymake-config)
;;; flymake-config.el ends here
