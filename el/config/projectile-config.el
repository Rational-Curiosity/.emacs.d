;;; projectile-config.el --- Configure projectile

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'projectile
;;   ;; after semantic
;;   (require 'projectile-config))
;; Never
;; (require 'projectile-config)

;;; Code:

(message "Importing projectile-config")
;;;;;;;;;;;;;;;;
;; Projectile ;;
;;;;;;;;;;;;;;;;

;;;;;;;;;;
;; Bugs ;;
;;;;;;;;;;
;; (eval-after-load "projectile"
;;   '(progn
;;      (defun projectile-get-other-files (current-file project-file-list &optional flex-matching)
;;        "Narrow to files with the same names but different extensions.
;; Returns a list of possible files for users to choose.

;; With FLEX-MATCHING, match any file that contains the base name of current file"
;;        (let* ((file-ext-list (projectile-associated-file-name-extensions current-file))
;;               (fulldirname  (if (file-name-directory current-file)
;;                                 (file-name-directory current-file) "./"))
;;               (dirname  (file-name-nondirectory (directory-file-name fulldirname)))
;;               (filename (projectile--file-name-sans-extensions current-file))
;;               (file-list (mapcar (lambda (ext)
;;                                    (if flex-matching
;;                                        (concat ".*" filename ".*" "\." ext "\\'")
;;                                      (concat "^" filename
;;                                              (unless (equal ext "")
;;                                                (concat  "\." ext))
;;                                              "\\'")))
;;                                  file-ext-list))
;;               (candidates (-filter (lambda (project-file)
;;                                      (string-match filename project-file))
;;                                    project-file-list))
;;               (candidates
;;                (-flatten (mapcar
;;                           (lambda (file)
;;                             (-filter (lambda (project-file)
;;                                        (string-match file
;;                                                      (concat (file-name-base project-file)
;;                                                              (unless (equal (file-name-extension project-file) nil)
;;                                                                (concat  "\." (file-name-extension project-file))))))
;;                                      candidates))
;;                           file-list)))
;;               (candidates
;;                (-sort (lambda (file _)
;;                         (let ((candidate-dirname (condition-case nil
;;                                                      (file-name-nondirectory (directory-file-name (file-name-directory file)))
;;                                                    (error nil))
;;                                                  ))
;;                           (unless (equal fulldirname (file-name-directory file))
;;                             (equal dirname candidate-dirname))))
;;                       candidates)))
;;          candidates))
;;      (defun projectile-get-ext-command()
;;        projectile-generic-command)
;;      (defun bug-projectile-get-other-files (orig-fun &rest args)
;;        (if (projectile-project-p)
;;            (apply orig-fun args)
;;          (list (file-name-nondirectory (ff-other-file-name)))))
;;      (advice-add 'projectile-get-other-files :around #'bug-projectile-get-other-files)))
;;;;;;;;;;
;;;;;

(setq-default projectile--mode-line "")
(setq projectile-mode-line-prefix ""
      projectile-dynamic-mode-line nil
      projectile-globally-ignored-file-suffixes
      '(".o" ".d" ".crt" ".key" ".txt" "~")
      ;; projectile-indexing-method 'native
      ;; projectile-enable-caching nil
      ;; projectile-file-exists-remote-cache-expire nil
      ;; projectile-require-project-root nil
      projectile-find-dir-includes-top-level t  ;; el bug
      projectile-project-root-files-top-down-recurring
      '("Makefile" "makefile" "CMakeLists.txt" "makefile.linux")
      projectile-project-root-files-functions
      '(projectile-root-local
        projectile-root-top-down-recurring
        projectile-root-bottom-up
        projectile-root-top-down)
      ;; projectile-switch-project-action 'counsel-projectile
      projectile-completion-system 'default
      projectile-mode-line-function (lambda () (concat "[" (projectile-project-name) "]")))

(defun projectile-enable-which-key-integration ()
  (which-key-add-major-mode-key-based-replacements
    major-mode
    "C-c p 4" "other window"
    "C-c p 5" "other frame"
    "C-c p s" "search"
    "C-c p x" "run"))
(add-hook 'projectile-mode-hook #'projectile-enable-which-key-integration)

(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "M-g M-a") 'projectile-ripgrep)
(define-key projectile-mode-map (kbd "M-g M-f") 'projectile-find-file)


(provide 'projectile-config)
;;; projectile-config.el ends here
