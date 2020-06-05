;;; polymode-config.el --- Translation tools

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'polymode
;;   (require 'polymode-config))
;;; Code:

(message "Importing polymode-config")
(setcar (cdr (assq 'polymode-minor-mode minor-mode-alist)) "◱")

(with-eval-after-load 'poly-org
  (setcar (cdr (assq 'poly-org-mode minor-mode-alist)) "◱"))

(defun polymode-disable-semantic-modes ()
  (semantic-mode -1)
  (semantic-idle-scheduler-mode -1))
(add-hook 'polymode-minor-mode-hook 'polymode-disable-semantic-modes)

(define-key polymode-minor-mode-map (kbd "C-'")
  (lookup-key polymode-minor-mode-map "\M-n"))
(define-key polymode-minor-mode-map "\M-n" nil)


(provide 'polymode-config)
;;; polymode-config.el ends here
