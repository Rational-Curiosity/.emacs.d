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
(require 'etags)

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

(defun tags-update-etags-file (extension)
  (interactive (list (read-string "File extension ."
                                  nil nil
                                  (file-name-extension (buffer-file-name)))))
  (if (stringp extension)
      (let ((tags-directory (locate-dominating-file default-directory
                                                    tags-default-file-name)))
        (if tags-directory
            (let ((default-directory tags-directory))
              (if (= 0
                     (shell-command
                      (concat
                       "rm TAGS && find . ! -readable -prune -o -type f -name \"*."
                       extension
                       "\" -print -exec etags --append {} \\;")))
                  (message "%s file created with .%s files."
                           (expand-file-name tags-default-file-name tags-directory)
                           extension)
                (message "%s file creation failed."
                         (expand-file-name tags-default-file-name tags-directory))))
          (message "%s file not found." tags-default-file-name)))
    (message "%s is not a valid extension." extension)))

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

(defun etags-xref-find-advice (orig-fun &rest args)
  (condition-case nil
      (apply orig-fun args)
    (error
     (let ((xref-backend-functions '(etags--xref-backend)))
       (call-interactively orig-fun)))))

(advice-add 'xref-find-apropos :around 'etags-xref-find-advice)
(advice-add 'xref-find-references :around 'etags-xref-find-advice)

(advice-add 'xref-find-definitions :around 'etags-xref-find-advice)
(advice-add 'xref-find-definitions-other-frame :around 'etags-xref-find-advice)
(advice-add 'xref-find-definitions-other-window :around 'etags-xref-find-advice)



;;;;;;;;;;
;; Keys ;;
;;;;;;;;;;
(defhydra hydra-xref (:foreign-keys run :hint nil)
  ("M-," #'xref-pop-marker-stack "pop")
  ("M-'" #'xref-find-references "ref")
  ("M-a" #'xref-find-apropos "apropos")
  ("M-." #'xref-find-definitions "def")
  ("M-W" #'xref-find-definitions-other-window "def win")
  ("M-F" #'xref-find-definitions-other-frame "def frame")
  ("M-s" #'tags-search "search")
  ("M-t" #'xref-query-replace-in-results "repl results")
  ("M-r" #'tags-query-replace "repl")
  ("M-c" #'fileloop-continue "cont")
  ("M-p" #'pop-tag-mark "pop tag")
  ("M-l" #'list-tags "list")
  ("M-q" nil "quit"))

(global-set-key (kbd "M-Ç") 'hydra-xref/body)


(provide 'etags-config)
;;; etags-config.el ends here
