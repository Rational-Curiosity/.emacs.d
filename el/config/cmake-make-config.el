;;; cmake-make-config.el --- cmake and make utils

;;; Commentary:

;; Usage:
;; (require 'cmake-make-config)

;;; Code:

(message "Importing cmake-make-config")
;; cmake-make begin
(require 's)
(require 'cmake-mode)

;;;;;;;;;;;;;;;;;;;;;;;
;; Smart compilation ;;
;;;;;;;;;;;;;;;;;;;;;;;
(require 'ede)
(defun make-targets (&optional file)
  "Get the targets of the Makefile FILE."
  (butlast (split-string (shell-command-to-string (if file (concat "targets " file) "targets")) "\n")))

(defun compilation-execute (&optional prefix)
  "Execute compilation command replacing $p $n $b.  PREFIX ignored."
  (interactive)
  (let ((bufferpathname (or buffer-file-name ""))
        (buffername (or (buffer-name) ""))
        (bufferbasename (or (file-name-base) ""))
        (command (read-shell-command "Shell command: " prefix nil nil)))
    (compilation-start (concat "LC_ALL=C " (replace-regexp-in-string "\$p" (concat "\"" bufferpathname "\"")
                     (replace-regexp-in-string "\$n" (concat "\"" buffername "\"")
                       (replace-regexp-in-string "\$b" (concat "\"" bufferbasename "\"")
                         command)))))))

(defun recursive-find-to-root (file-list directory)
  "Search FILE-LIST in paths from DIRECTORY to root."
  (let ((dir directory))
      (cl-block nested-while
        (while (not (string-equal dir "/"))
          (progn
            (dolist (element file-list)
              (let ((path-file (concat dir element)))
                (message path-file)
                (when (file-exists-p path-file)
                  (cl-return-from nested-while path-file))))
            (setq dir (concat (expand-file-name (concat dir "..")) "/"))))
        (error "Project file not found"))))

(defun cmake-make-path-file (&optional directory)
  "Find Makefile or CMakelists.txt path starting in DIRECTORY."
  (cond
   (directory
    (recursive-find-to-root '("CMakeLists.txt" "Makefile" "makefile") directory))
   ((ede-current-project)
      (oref (ede-current-project) file))
   ;; ((projectile-project-root)
   ;;  (let ((project-root (projectile-project-root)))
   ;;    (dolist (element '("CMakeLists.txt" "Makefile" "makefile"))
   ;;      (let ((path-file (concat project-root element)))
   ;;        (when (file-exists-p path-file)
   ;;          (return path-file))))))
   (t
    (recursive-find-to-root '("CMakeLists.txt" "Makefile" "makefile") default-directory))))
          
(defun cmake-make-conf ()
  "Get current compilation mode.  Examples: debug, release..."
  (cond
   ((ede-current-project)
    (oref (ede-current-project) configuration-default))
   (t "debug")))

(defun make-completing-read (file last)
  "Read target with completion.  Targets for completing are in FILE.  LAST is last selection."
  (let ((selection (completing-read-multiple "Select target: " (make-targets file) nil nil (s-replace " " "," last))))
    (if (listp selection)
        (mapconcat 'identity selection " ")
      selection)))

(defvar make-target-last "")
(defun compile-cmake-make (&optional directory)
  "Compile with cmake or make inside DIRECTORY."
  (interactive
   (cond
    ((equal current-prefix-arg '(4))
     (list
      (read-directory-name "Initial directory: ")))))
  (let* ((path-file (cmake-make-path-file directory))
         (path (file-name-directory path-file))
         (file (file-name-nondirectory path-file))
         (conf (cmake-make-conf)))
    (cond
     ((string-equal file "CMakeLists.txt")
      (let ((dir (concat path conf "/")))
        (unless (file-exists-p dir)
          (make-directory dir))
        (let ((default-directory dir))
          (when (file-newer-than-file-p "../CMakeLists.txt" "CMakeCache.txt")
            (progn
              (when (file-exists-p "CMakeCache.txt") (delete-file "CMakeCache.txt"))
              (when (file-exists-p "Makefile") (delete-file "Makefile"))
              (when (file-exists-p "cmake_install.cmake") (delete-file "cmake_install.cmake"))
              (when (file-exists-p "CTestTestfile.cmake") (delete-file "CTestTestfile.cmake"))
              (when (file-exists-p "DartConfiguration.tcl") (delete-file "DartConfiguration.tcl"))
              (when (file-exists-p "CMakeFiles") (delete-directory "CMakeFiles" t))
              (when (file-exists-p "Testing") (delete-directory "Testing" t))))
          (unless (file-exists-p "CMakeCache.txt")
            (cond ((string-equal (downcase conf) "debug")
                   (compilation-execute "cmake -DCMAKE_BUILD_TYPE=Debug .."))
                  ((string-equal (downcase conf) "release")
                   (compilation-execute "cmake -DCMAKE_BUILD_TYPE=Release .."))
                  (t (compilation-execute "cmake .."))))
          (set 'make-target-last (make-completing-read file make-target-last))
          (compilation-execute (concat "make -k " make-target-last " ")))))
     ((string-equal (downcase (substring file 0 8)) "makefile")
      (let ((default-directory path))
        (set 'make-target-last (make-completing-read file make-target-last))
        (compilation-execute (concat "make -k -f " file " " make-target-last " ")))))))

;; Teclas
(with-eval-after-load 'cc-mode
  (define-key c++-mode-map (kbd "C-c m c") 'compile-cmake-make)
  (define-key c-mode-map (kbd "C-c m c") 'compile-cmake-make))


(provide 'cmake-make-config)
;;; cmake-make-config.el ends here
