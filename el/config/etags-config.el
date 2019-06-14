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
          (progn
            (message "%s file founded in %s" tags-file tags-directory)
            (apply orig-fun (concat tags-directory tags-file) args))
        (apply orig-fun file args)))))
(advice-add 'visit-tags-table :around 'visit-tags-table-advice)
(advice-add 'visit-tags-table-buffer :around 'visit-tags-table-advice)


(provide 'etags-config)
;;; etags-config.el ends here
