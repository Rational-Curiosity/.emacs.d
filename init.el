;;; init.el --- Initial loaded file

;;; Commentary:

;; Compile with
;; `emacs -batch -f batch-byte-compile emacs.el'
;; and rename
;; `mv emacs.elc init.elc'

;;; Code:

(package-initialize)

(require 'cl-lib)
(eval-when-compile
  (require 'cl))

(eval-and-compile
  (let ((default-directory "~/.emacs.d/el"))
    (normal-top-level-add-subdirs-to-load-path))
  (require 'config-lib))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(package-selected-packages
   (quote
    (ac-php-core ag anaconda-mode android-mode async auctex avy bash-completion bind-key clang-format cmake-font-lock cmake-mode company company-anaconda company-c-headers company-php company-rtags company-tern crm-custom csharp-mode csv-mode ctable cursor-chg cyphejor dash dash-functional deferred docker docker-tramp epl expand-region f figlet fish-mode flycheck flycheck-haskell flycheck-julia flycheck-popup-tip flymake flymake-lua format-all free-keys git-commit gnuplot gnuplot-mode go-mode goto-chg graphviz-dot-mode haskell-mode hide-comnt hierarchy highlight-indent-guides highlight-thing ht htmlize hydra ido-at-point ido-completing-read+ js2-mode js2-refactor json-mode json-navigator json-reformat json-snatcher let-alist lua-mode lv magit magit-popup markdown-mode markdown-mode+ memoize multiple-cursors ob-dart ob-go org org-agenda-property org-bullets org-plus-contrib org-super-agenda org-trello ox-gfm ox-rst ox-twbs php-mode pkg-info plantuml-mode popup projectile protobuf-mode pythonic rainbow-delimiters rebox2 request request-deferred rich-minority rtags s seq smart-mode-line smartparens smartscan smex sphinx-doc sphinx-frontend srefactor stickyfunc-enhance string-inflection tablist tern transient transpose-frame undo-tree vdiff vimish-fold virtualenvwrapper vlf web-completion-data which-key with-editor xahk-mode xcscope xref-js2 xterm-color yasnippet yasnippet-snippets))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq initial-buffer-choice nil
      inhibit-startup-screen t
      initial-major-mode 'fundamental-mode
      visible-bell t
      ;; avoids warnings
      ad-redefinition-action 'accept)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        ;;
;;        Packages        ;;
;;                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package-config)
(require 'bind-key)

(require 'undo-tree)
(global-undo-tree-mode)
(setq undo-tree-mode-lighter ""
;;       undo-tree-visualizer-diff t
;;       undo-tree-visualizer-timestamps t
;;       undo-tree-visualizer-relative-timestamps t
 )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               ;;
;;   Configuration files         ;;
;;                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; first of all
(load "config" t)

(load-all-in-directory "~/.emacs.d/el/bugs/")
;; [ <Always required>

(require 'ido-config)

(when (load "company" t)
  (require 'company-extensions-config))

(require 'hydra-config)

(require 'random-functions)

(require 'misc-config)

(require 'theme-config)

(require 'typing-config)

(require 'avy-config)

(require 'vimish-fold-config)

(require 'highlight-config)

(require 'bookmark+-config)

(require 'which-key-config)

(require 'rect-config)

(require 'regions-config)

(require 'term-config)

(with-eval-after-load 'shell
  (require 'shell-config))

(with-eval-after-load 'esh-mode
  (require 'eshell-config))

(require 'tramp-config)

(require 'eww-config)

(require 'menu-config)

(require 'frames-windows-buffers-config)

(require 'prettify-symbols-config)

(require 'comment-config)

(require 'rotate-text-config)

(require 'multiple-cursors-config)

(require 'smartscan-config)

(require 'nxml-config)

(require 'diff-config)

(require 'vlf-config)

(require 'version-control-config)

;; csv-mode
(add-to-list 'auto-mode-alist '("\\.tsv\\'" . csv-mode))
;; not working with with-eval-after-load 'csv-mode
(require 'csv-config)

(require 'minimap-config)

(require 'figlet-config)
;; ] <Always required>


;; [ <Not always required>
(with-eval-after-load 'ispell
  (require 'ispell-config))
;; Highlight thing
(add-hook 'prog-mode-hook #'highlight-thing-mode)
(with-eval-after-load 'highlight-thing
  (require 'highlight-thing-config))
;; highlight-indent-guides (started inside typing-config when emacs server)
;; poor performance and annoying cursor movement
;; (add-hook 'prog-mode-hook #'highlight-indent-guides-mode)
(with-eval-after-load 'highlight-indent-guides
  (require 'highlight-indent-guides-config))
;; rainbow-delimiters-mode (before smartparens-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(with-eval-after-load 'rainbow-delimiters
  (require 'rainbow-delimiters-config))
;; smartparens-mode
(add-hook 'prog-mode-hook #'smartparens-mode)
(add-hook 'prog-mode-hook #'show-smartparens-mode)
(add-hook 'org-mode-hook #'smartparens-mode)
(add-hook 'org-mode-hook #'show-smartparens-mode)
(with-eval-after-load 'smartparens
  (require 'smartparens-custom-config))

;;;;;;;;;;;;;
;; Systems ;;
;;;;;;;;;;;;;
(with-eval-after-load 'magit-popup
  (require 'magit-popup-config)
  (require 'docker-config))

(with-eval-after-load 'transient
  (require 'transient-config))
;;;;;;;;;;;;;;;;;
;; Programming ;;
;;;;;;;;;;;;;;;;;
;; flycheck-mode
(add-hook 'prog-mode-hook #'flycheck-mode)
(add-hook 'flycheck-mode-hook #'flycheck-haskell-setup)
(with-eval-after-load 'flycheck
  (require 'flycheck-config)
  (require 'srefactor-config)
  (when (load "flycheck-popup-tip" t)
    (setq flycheck-popup-tip-error-prefix "")
    (add-hook 'flycheck-mode-hook #'flycheck-popup-tip-mode)))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-hook 'prog-mode-hook #'projectile-mode)
(with-eval-after-load 'projectile
  ;; after semantic
  (require 'projectile-config))
(add-hook 'prog-mode-hook (lambda ()
                            (require 'ede)
                            (ede-minor-mode)))
(with-eval-after-load 'ede
  (require 'ede-config))
;; Snippets
(add-hook 'prog-mode-hook #'yas-minor-mode)
(add-hook 'org-mode-hook #'yas-minor-mode)
(with-eval-after-load 'yasnippet
  (require 'yasnippet-config))
;; [ elisp-mode
(setq eldoc-minor-mode-string "")
;; last hook then first loaded
(add-hook 'prog-mode-hook #'semantic-mode)
(with-eval-after-load 'semantic
  (require 'semantic-config)
  ;; stickfunc improved
  (require 'stickyfunc-enhance)
  (require 'gud-config)
  (require 'speedbar-config)
  (with-current-buffer "*scratch*"
    (lisp-interaction-mode)))
;; ]
;; cmake-mode
(setq auto-mode-alist
      (append '(;;("CMakeLists\\.txt\\'" . cmake-mode) ; por defecto
                ;;("\\.cmake\\'" . cmake-mode) ; por defecto
                ("[Mm]akefile\\." . makefile-mode))
              auto-mode-alist))
;; cmake highlight
(autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)
(add-hook 'cmake-mode-hook #'cmake-font-lock-activate)
;; flymake-mode
;; thanks to: stackoverflow.com/questions/6110691/is-there-a-way-to-make-flymake-to-compile-only-when-i-save
(with-eval-after-load 'flymake
  (defun flymake-after-change-function (start stop len)
    "Start syntax check for current buffer if it isn't already running."
    ;; Do nothing, don't want to run checks until I save.
    ))

;; [ cc-mode
(defun c-c++-config ()
  ;; run only once
  (remove-hook 'c-mode-hook 'c-c++-config)
  (remove-hook 'c++-mode-hook 'c-c++-config)
  (require 'c-c++-config)
  ;; Después de semantic
  ;; Después de ede-projects-config
  (require 'cmake-make-config)
  ;; rtags: Jumping and Completion (after semantic)
  (when (load "rtags" t)
    (require 'rtags-config)
    (when (executable-find "rdm")
      (add-hook 'c-mode-hook   #'rtags-start-process-unless-running)
      (add-hook 'c++-mode-hook #'rtags-start-process-unless-running))))
(add-hook 'c-mode-hook   'c-c++-config)
(add-hook 'c++-mode-hook 'c-c++-config)
;; ]

;; loads only when necessary
(with-eval-after-load 'rst
  (require 'rst-config))

(with-eval-after-load 'markdown-mode
  (message "Importing markdown-mode+")
  (require 'markdown-mode+))

;; [ python
(setq python-shell-interpreter "python3")
(with-eval-after-load 'python
  (require 'semantic/wisent/python)
  (require 'python-config)
  (add-hook 'python-mode-hook #'detect-python-project-version)
  ;; ANACONDA
  (when (load "anaconda-mode" t)
    (require 'anaconda-config)
    (add-hook 'python-mode-hook #'anaconda-mode)
    (add-hook 'python-mode-hook #'anaconda-eldoc-mode)))
(with-eval-after-load 'virtualenvwrapper
  (require 'virtualenvwrapper-config))
;; ]


;; [ javascript
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))
(with-eval-after-load 'js
  (when (load "js2-mode" t)
    ;; (add-hook 'js-mode-hook #'js2-minor-mode)
    (add-hook 'js2-mode-hook #'js2-refactor-mode)
    ;; Better imenu
    (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
    (add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode))
    (require 'javascript-config)
    (when (load "xref-js2" t)
      (add-hook 'js2-mode-hook (lambda ()
                                 (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))
    (when (and (executable-find "tern") (load "tern" t))
      (add-hook 'js2-mode-hook #'tern-mode))))
;; ]

;; [ php
(with-eval-after-load 'php-mode
  (require 'php-config))
;; ]

;; TODO: implementar la función python-integrated-mode dentro de python-integrated.el
(autoload 'python-integrated-mode "python-integrated" "Python everywhere" t)
(add-to-list 'auto-mode-alist '("\\.py\\." . python-integrated-mode))
(require 'python-integrated)

(add-to-list 'auto-mode-alist '("\\.ptx\\'" . latex-mode))
(with-eval-after-load 'latex
  (require 'latex-config))

;; Enable plantuml-mode for PlantUML files
;; (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
(with-eval-after-load 'plantuml-mode
  (require 'plantuml-config))

;; [ org
(defcustom org-replace-disputed-keys t
  "")
(with-eval-after-load 'org
  (require 'org-config)
  (require 'org-super-agenda-config)
  (require 'org-appt))
(add-hook 'org-mode-hook #'org-super-agenda-mode)
;; ]

(with-eval-after-load 'android-mode
  (require 'android-config))


;; (add-hook 'c-mode-common-hook 'cscope-minor-mode)
;; (with-eval-after-load 'cscope-minor-mode
;;   (require 'xcscope-config))

;; <ahk> AutoHotKey programming
(add-to-list 'auto-mode-alist '("\\.ahk\\'" . xahk-mode))

;; [ <flyspell>
;; Desactivado por incómodo desplazamiento del cursor
;;(require 'flyspell-lazy)

(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))

(dolist (hook '(c++-mode-hook
                c-mode-hook
                lisp-mode-hook
                emacs-lisp-mode-hook))
;;  (add-hook hook #'flyspell-lazy-mode)
  (add-hook hook (lambda () (flyspell-prog-mode))))

(with-eval-after-load 'flyspell
  (require 'flyspell-config))
;; ] <flyspell>
;; ] <Not always required>

;; ---------- ;;
;; Hide modes ;;
;; ---------- ;;
;; Último de todos
(require 'mode-line-config)

(require 'machine-config)

(defun argument--all (switch)
  "Command line arg `--all'.  SWITCH ignored."
  (require 'semantic)
  (require 'ede)
  (require 'projectile)
  (require 'cc-mode)
  (require 'latex)
  (require 'org)
  (require 'yasnippet)
  (require 'flycheck)
  (require 'rtags))
(add-to-list 'command-switch-alist '("--all" . argument--all))
(defun argument--agenda (switch)
  "Command line arg `--agenda'.  SWITCH ignored."
  (require 'org))
(add-to-list 'command-switch-alist '("--agenda" . argument--agenda))
;; Usage: emacs --diff file/dir1 file/dir2
(defun argument--diff (switch)
  "Command line arg `--diff'.  SWITCH ignored."
  (let ((arg1 (pop command-line-args-left))
        (arg2 (pop command-line-args-left))
        (arg3 (pop command-line-args-left)))
    (cond
     ((and
       (file-directory-p arg1)
       (file-directory-p arg2))
      (ediff-directories arg1 arg2 arg3))
     ((and
       (file-exists-p arg1)
       (file-exists-p arg2))
      (ediff-files arg1 arg2))
     (t
      (message "Files or directories required")))))
(add-to-list 'command-switch-alist '("--diff" . argument--diff))
;; Usage: emacs --debug-on-entry el-file func-name
(defun argument--debug-on-entry (switch)
  "Command line arg `--debug-on-entry'.  SWITCH ignored."
  (let ((arg1 (pop command-line-args-left))
        (arg2 (pop command-line-args-left)))
    (eval `(with-eval-after-load ,arg1
             (message "Debugging: %s::%s" ,arg1
                      (debug-on-entry (intern ,arg2)))))))
(add-to-list 'command-switch-alist '("--debug-on-entry" . argument--debug-on-entry))

;;; init.el ends here
