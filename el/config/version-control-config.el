;;; version-control-config.el --- Configure version control

;;; Commentary:

;; Usage:
;; (require 'version-control-config)

;;; Code:
(message "Importing version-control-config")
(require 'frames-windows-buffers-config)

(with-eval-after-load 'magit-mode
  (defun vc-refresh-buffers ()
    (dolist (buffer (buffers-from-file))
      (with-current-buffer buffer
        (vc-refresh-state))))
  (advice-add 'magit-refresh :after 'vc-refresh-buffers))


(provide 'version-control-config)
;;; version-control-config.el ends here
