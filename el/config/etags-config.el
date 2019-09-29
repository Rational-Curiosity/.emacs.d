;;; etags-config.el --- Configure and improve etags

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'prog-mode
;;   (require 'etags-config))
;; never:
;; (require 'etags-config)

;; Do not include in this file:
;; (require 'etags-mode)

;;; Code:

;; Generate tags file:
;; # find <root code path> -type f -name "<source files pattern>" -print 2>/dev/null | xargs etags --append

(defun visit-tags-table-advice (orig-fun &optional file &rest args)
  (if file
      (apply orig-fun file args)
    (let* ((tags-file "TAGS")
           (tags-directory (locate-dominating-file default-directory tags-file)))
      (if tags-directory
          (let ((tags-path (expand-file-name tags-file tags-directory)))
            (message "%s file path: %s" tags-file tags-path)
            ;; (advice-remove 'visit-tags-table 'visit-tags-table-advice)
            (apply orig-fun tags-path args))
        (apply orig-fun file args)))))
(advice-add 'visit-tags-table :around 'visit-tags-table-advice)

(defun visit-tags-table-buffer-advice (orig-fun &rest args)
  (advice-remove 'visit-tags-table-buffer 'visit-tags-table-buffer-advice)
  (visit-tags-table)
  (apply orig-fun args))
(advice-add 'visit-tags-table-buffer :around 'visit-tags-table-buffer-advice)


(provide 'etags-config)
;;; etags-config.el ends here
