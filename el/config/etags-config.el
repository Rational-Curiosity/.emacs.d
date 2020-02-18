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
;; # cd <project root path>
;; # rm TAGS
;; # find <root code path> -type f -name "<source files pattern>" -print 2>/dev/null | xargs etags --append

(defvar tags-default-file-name "TAGS")

(defun visit-tags-table-advice (orig-fun &optional file &rest args)
  (if file
      (apply orig-fun file args)
    (let ((tags-directory (locate-dominating-file default-directory tags-default-file-name)))
      (if tags-directory
          (let ((tags-path (expand-file-name tags-default-file-name tags-directory)))
            (message "%s file path: %s" tags-default-file-name tags-path)
            ;; (advice-remove 'visit-tags-table 'visit-tags-table-advice)
            (apply orig-fun tags-path args))
        (message "%s file not found." tags-default-file-name)))))
(advice-add 'visit-tags-table :around 'visit-tags-table-advice)

(defun visit-tags-table-buffer-advice (orig-fun &rest args)
  (advice-remove 'visit-tags-table-buffer 'visit-tags-table-buffer-advice)
  (visit-tags-table)
  (apply orig-fun args))
(advice-add 'visit-tags-table-buffer :around 'visit-tags-table-buffer-advice)

(defun tags-update-etags-file ()
  (interactive)
  (let ((tags-directory (locate-dominating-file default-directory tags-default-file-name)))
    (if tags-directory
        (let ((default-directory tags-directory)
              (extension (file-name-extension (buffer-file-name))))
          (if (= 0
                 (shell-command (concat "rm TAGS && find . ! -readable -prune -o -type f -name \"*."
                                        extension
                                        "\" -print -exec etags --append {} \\;")))
              (message "%s file created with .%s files."
                       (expand-file-name tags-default-file-name tags-directory)
                       extension)
            (message "%s file creation failed."
                     (expand-file-name tags-default-file-name tags-directory))))
      (message "%s file not found." tags-default-file-name))))

(defun tags-create-etags-file (directory)
  (interactive "DCreate etags file in path: ")
  (let ((tags-path (expand-file-name tags-default-file-name directory)))
    (if (file-exists-p tags-path)
        (message "%s file already exists in %s." tags-default-file-name directory)
      (let ((default-directory directory)
            (extension (file-name-extension (buffer-file-name))))
        (if (= 0
               (shell-command (concat "find . ! -readable -prune -o -type f -name \"*."
                                      extension
                                      "\" -print -exec etags --append {} \\;")))
            (message "%s file created with .%s files." tags-path extension)
          (message "%s file creation failed." tags-path))))))

;;;;;;;;;;
;; Keys ;;
;;;;;;;;;;
(defhydra hydra-xref (:foreign-keys run :hint nil)
  ("M-," #'xref-pop-marker-stack "pop")
  ("M-'" #'xref-find-references "ref")
  ("M-ç" #'xref-find-apropos "apropos")
  ("M-." #'xref-find-definitions "def")
  ("M--" #'xref-find-definitions-other-window "def win")
  ("M-+" #'xref-find-definitions-other-frame "def frame")
  ("M-s" #'tags-search "search")
  ("M-t" #'xref-query-replace-in-results "repl results")
  ("M-r" #'tags-query-replace "repl")
  ("M-c" #'tags-loop-continue "cont")
  ("M-f" #'find-tag "find")
  ("M-w" #'find-tag-other-window "find win")
  ("M-F" #'find-tag-other-frame "find frame")
  ("M-p" #'pop-tag-mark "pop tag")
  ("M-a" #'tags-apropos "apropos tag")
  ("M-l" #'list-tags "list")
  ("M-q" nil "quit"))

(global-set-key (kbd "M-ç") 'hydra-xref/body)


(provide 'etags-config)
;;; etags-config.el ends here
