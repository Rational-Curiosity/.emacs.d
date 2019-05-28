;;; version-control-config.el --- Configure version control

;;; Commentary:

;; Usage:
;; (require 'version-control-config)

;;; Code:
(message "Importing version-control-config")
(require 'frames-windows-buffers-config)

(with-eval-after-load 'magit-branch
  (defun magit-branch-advice ()
    (dolist (buffer (buffers-from-file))
      (with-current-buffer buffer
        (vc-refresh-state))))
  (advice-add 'magit-branch :after 'magit-branch-advice))


(provide 'version-control-config)
;;; version-control-config.el ends here
