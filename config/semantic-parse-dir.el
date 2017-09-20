;;; semantic-parse-dir.el --- Force semantic to parse directory

;;; Commentary:

;; Usage:
;; (require 'semantic-parse-dir)

;;; Code:

(require 'semantic)

(defvar semantic-parse-c-files-regex "\\.\\(c\\|cc\\|cpp\\|cxx\\|h\\|hpp\\|hxx\\)$"
  "A regular expression to match any c/c++ related files under a directory.")

(defvar semantic-parse-exclude-files-regex "/\\.\\(hg\\|git\\)/")

(defun semantic-parse-dir-regex (root regex &optional exclude)
  "Parse dirs in ROOT that match REGEX and exclude EXCLUDE."
  (dolist (file (cl-remove-if
                 (lambda (arg) (string-match-p
                           (or exclude semantic-parse-exclude-files-regex)
                           arg))
                 (directory-files-recursively
                  root
                  regex)))
    (semanticdb-file-table-object file)))

(defun semantic-parse-dir (root)
  "Make Semantic parse all source files in directory ROOT, recursively."
  (interactive (list (read-directory-name "Root directory: "
                                          default-directory)))
  (semantic-parse-dir-regex root semantic-parse-c-files-regex))

;; (defun semantic-parse-dir (root regex)
;;   "This function is an attempt of mine to force semantic to
;;    parse all source files under a root directory. Arguments:
;;    -- root: The full path to the root directory
;;    -- regex: A regular expression against which to match all files in the directory"
;;   (let (
;;         ;;make sure that root has a trailing slash and is a dir
;;         (root (file-name-as-directory root))
;;         (files (directory-files root t ))
;;        )
;;     ;; remove current dir and parent dir from list
;;     (setq files (delete (format "%s." root) files))
;;     (setq files (delete (format "%s.." root) files))
;;     ;; remove any known version control directories
;;     (setq files (delete (format "%s.git" root) files))
;;     (setq files (delete (format "%s.hg" root) files))
;;     (while files
;;       (setq file (pop files))
;;       (if (not(file-accessible-directory-p file))
;;           ;;if it's a file that matches the regex we seek
;;           (progn (when (string-match-p regex file)
;;                    (save-excursion
;;                      (semanticdb-file-table-object file))
;;            ))
;;           ;;else if it's a directory
;;           (semantic-parse-dir file regex)
;;       )
;;      )
;;   )
;; )

;; (defun semantic-parse-current-dir (regex)
;;   "Parses all files under the current directory matching regex"
;;   (semantic-parse-dir (file-name-directory buffer-file-name) regex)
;; )
 
;; (defun semantic-parse-curdir-c ()
;;   "Parses all the c/c++ related files under the current directory
;;    and inputs their data into semantic"
;;   (interactive)
;;   (semantic-parse-current-dir semantic-parse-c-files-regex)
;; )
 
;; (defun semantic-parse-dir-c (dir)
;;   "Prompts the user for a directory and parses all c/c++ related files
;;    under the directory"
;;   (interactive (list (read-directory-name "Provide the directory to search in:")))
;;   (semantic-parse-dir (expand-file-name dir) semantic-parse-c-files-regex)
;; )


(provide 'semantic-parse-dir)
;;; semantic-parse-dir.el ends here
