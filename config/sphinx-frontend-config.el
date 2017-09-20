;;; sphinx-frontend-config.el --- Configure and improve sphinx frontend

;;; Commentary:

;; Usage:
;; (require 'sphinx-frontend-config)

;;; Code:

(message "Importing sphinx-frontend-config")
(require 'sphinx-frontend)
(require 'projectile)

(setq sphinx-build-command "python3 -m sphinx -b "
      sphinx-output-dir-html "_build/html")

(defun sphinx-get-root-document-dir ()
  "Recursively search current document's tree root.
The root sign is the location in the directory `sphinx-conf-file-name' file.
Returns current document's tree root directory."
  (or (locate-dominating-file default-directory sphinx-conf-file-name)
      (let* ((project-root (projectile-project-root))
             (sphinx-root-dir
              (locate-file
               sphinx-conf-file-name
               (list
                (concat project-root "doc/")
                (concat project-root "docs/")
                project-root))))
        (if sphinx-root-dir
            (file-name-directory sphinx-root-dir)
          (error (concat "Can't find file " sphinx-conf-file-name " in " project-root))))))


(provide 'sphinx-frontend-config)
;;; sphinx-frontend-config.el ends here
