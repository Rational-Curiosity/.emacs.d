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

(setq flymake-no-changes-timeout nil)

;; thanks to: stackoverflow.com/questions/6110691/is-there-a-way-to-make-flymake-to-compile-only-when-i-save
;; (defun flymake-after-change-function (start stop len)
;;   "Start syntax check for current buffer if it isn't already running.
;; START and STOP and LEN are as in `after-change-functions'."
;;     ;; Do nothing, don't want to run checks until I save.
;;   )

(define-key flymake-mode-map (kbd "M-g n") #'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-g M-n") #'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-g p") #'flymake-goto-prev-error)
(define-key flymake-mode-map (kbd "M-g M-p") #'flymake-goto-prev-error)


(provide 'flymake-config)
;;; flymake-config.el ends here
