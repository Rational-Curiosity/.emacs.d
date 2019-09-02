;;; vlf-config.el --- Configure very large files

;;; Commentary:

;; Usage:
;; (require 'vlf-config)

;;; Code:

(require 'vlf-setup)

(setq vlf-batch-size (* 1024 1025))

(defun find-file-check-make-large-file-read-only-hook ()
  "If a file is over a given size, make the buffer read only."
  (when (> (buffer-size) vlf-batch-size)
    (setq buffer-read-only t)
    (buffer-disable-undo)
    (fundamental-mode)
    (message "Big file: Read-only, fundamental mode & undo disabled.")))
(add-hook 'find-file-hook 'find-file-check-make-large-file-read-only-hook)


(provide 'vlf-config)
;;; vlf-config.el ends here
