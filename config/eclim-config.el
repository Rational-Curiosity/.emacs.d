;;; eclim-config.el --- Configure eclim for java

;;; Commentary:

;;; Code:
(message "Importing eclim-config")

(setq eclimd-autostart t
      emacs-eclim-ignore-case t)

(defun my-java-mode-hook ()
    (eclim-mode t))

(add-hook 'java-mode-hook 'my-java-mode-hook)

(provide 'eclim-config)
;;; eclim-config.el ends here
