;;; init.el --- Initial loaded file

;;; Commentary:

;; Emacs buind from source
;; `./configure --with-cairo --with-x-toolkit=no CFLAGS='-O3'`

;; Compile .el files with
;; `emacs -batch -f batch-byte-compile emacs.el`
;; and rename
;; `mv emacs.elc init.elc``

;;; Code:

(add-hook 'after-init-hook
          `(lambda ()
             (setq gc-cons-percentage ,gc-cons-percentage
                   gc-cons-threshold ,gc-cons-threshold)) t)
(setq gc-cons-percentage 0.6
      gc-cons-threshold 100000000)

;; emacs 27 avoids (package-initialize)

(require 'cl-lib)
;; Package cl is deprecated
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
   '("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default))
 '(package-selected-packages
   '(ace-window ag anaphora async auctex avy bash-completion bookmark+ bui cmake-font-lock cmake-mode company company-lsp company-posframe cyphejor dap-mode dash dash-functional deferred docker docker-tramp edit-server ein elfeed emms epl expand-region exwm f figlet flycheck flyspell-correct flyspell-correct-helm free-keys git-commit gnuplot gnuplot-mode go-mode goto-chg graphviz-dot-mode guess-language haskell-mode helm helm-ag helm-company helm-core helm-etags-plus helm-exwm helm-flycheck helm-flyspell helm-lsp helm-org helm-org-rifle helm-projectile helm-swoop helm-tramp helm-unicode helm-xref hide-comnt highlight hl-line+ hl-todo ht htmlize hydra imenu-anywhere json-mode json-reformat json-snatcher let-alist link-hint lsp-java lsp-mode lsp-treemacs lsp-ui lua-mode lv magit magit-todos map markdown-mode markdown-mode+ memoize mini-modeline multiple-cursors ob-async org org-agenda-property org-brain org-noter org-plus-contrib org-ql org-super-agenda org-superstar ov ox-gfm ox-mediawiki ox-rst ox-twbs pcre2el peg pfuture php-mode pkg-info plantuml-mode polymode popup posframe projectile protobuf-mode psession rainbow-delimiters rebox2 request request-deferred rust-mode s smartparens smartscan spinner stickyfunc-enhance string-inflection swap-regions tablist thingatpt+ transient transpose-frame treemacs ts undo-tree vdiff vimish-fold virtualenvwrapper vlf web-mode websocket which-key with-editor xahk-mode xelb xterm-color yasnippet yasnippet-snippets))
 '(safe-local-variable-values
   '((eval set 'org-agenda-files
           (list
            (file-name-directory
             (buffer-file-name)))))))

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
      history-delete-duplicates t
      debugger-bury-or-kill nil
      ;; avoids warnings
      ad-redefinition-action 'accept)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        ;;
;;        Packages        ;;
;;                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package-config)

(require 'mode-line-config)

(require 'undo-tree)
(setq undo-tree-mode-lighter ""
;;       undo-tree-visualizer-diff t
;;       undo-tree-visualizer-timestamps t
;;       undo-tree-visualizer-relative-timestamps t
      )
(global-undo-tree-mode)

(require 'psession)
(defun psession-restore-last-winconf())
(assq-delete-all 'psession--save-buffers-alist psession-object-to-save-alist)
(assq-delete-all 'psession--winconf-alist psession-object-to-save-alist)
(psession-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               ;;
;;   Configuration files         ;;
;;                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; first of all
(load "config" t)

(load-all-in-directory "~/.emacs.d/el/bugs/")
;; [ <Always required>

;; (require 'modal-config)

(when (load "helm" t)
  (require 'helm-extensions-config))

(when (load "company" t)
  (require 'company-extensions-config))

;; after (require 'mode-line-config)
(require 'ace-window-config)

(require 'hydra-config)

(require 'random-functions)

(require 'misc-config)

(require 'theme-config)

(require 'typing-config)

(require 'caps-modes)

(require 'avy-config)

(require 'vimish-fold-config)

(require 'highlight-config)

(require 'bookmark+-config)

(require 'which-key-config)

(require 'rect-config)

(require 'regions-config)

(require 'term-config)

(require 'tramp-config)

(require 'eww-config)

(require 'menu-config)

(require 'frames-windows-buffers-config)

(require 'prettify-symbols-config)

(require 'comment-config)

(require 'thing-cmds-autoloads)

(require 'rotate-text-config)

(require 'multiple-cursors-config)

(require 'multiple-windows)

(require 'nxml-config)

(require 'diff-config)

(require 'vlf-config)

(require 'version-control-config)

(require 'minimap-config)

(require 'figlet-config)

(require 'language-tools)
(language-tools-config)
;; ] <Always required>


;; [ <Not always required>
(with-eval-after-load 'shell
  (require 'shell-config))

(with-eval-after-load 'esh-mode
  (require 'eshell-config))

;; csv-mode
(require 'csv-mode-autoloads)
(with-eval-after-load 'csv-mode
  (require 'csv-config))

;; ispell
(with-eval-after-load 'ispell
  (require 'ispell-config))

;; HideShow
(add-hook 'prog-mode-hook #'hs-minor-mode)
(with-eval-after-load 'hideshow
  (require 'hideshow-config))

;; Smartscan
(add-hook 'prog-mode-hook #'smartscan-mode)
(with-eval-after-load 'smartscan
  (require 'smartscan-config))

;; rainbow-delimiters-mode (before smartparens-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(with-eval-after-load 'rainbow-delimiters
  (with-current-buffer "*scratch*"
    (lisp-interaction-mode))
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
  (require 'magit-popup-config))


(dolist (el-file '(docker docker-container))
  (with-eval-after-load el-file
    (require 'docker-config)))

(with-eval-after-load 'transient
  (require 'transient-config))

;;;;;;;;;;;;;;;;;
;; Programming ;;
;;;;;;;;;;;;;;;;;
(with-eval-after-load 'compile
  (require 'compile-config))
(with-eval-after-load 'etags
  (require 'etags-config))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(with-eval-after-load 'ede
  (require 'ede-config))
;; Snippets
(add-hook 'prog-mode-hook #'yas-minor-mode)
(add-hook 'org-mode-hook #'yas-minor-mode)
(with-eval-after-load 'yasnippet
  (require 'yasnippet-config))
;; projectile
(add-hook 'prog-mode-hook #'projectile-mode)
(with-eval-after-load 'projectile
  ;; after semantic
  (require 'projectile-config))
;; [ elisp-mode
(setq eldoc-minor-mode-string "")
;; last hook then first loaded
(add-hook 'emacs-lisp-mode-hook #'semantic-mode)
(add-hook 'lisp-mode-hook #'semantic-mode)
(with-eval-after-load 'semantic
  (require 'semantic-config)
  ;; stickfunc improved
  (require 'stickyfunc-enhance)
  (require 'gud-config)
  (require 'speedbar-config))
;; ]
;; cmake-mode
(setq auto-mode-alist
      (nconc '(;;("CMakeLists\\.txt\\'" . cmake-mode) ; por defecto
               ;;("\\.cmake\\'" . cmake-mode) ; por defecto
               ("[Mm]akefile\\." . makefile-mode))
             auto-mode-alist))
;; cmake highlight
(autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)
(add-hook 'cmake-mode-hook #'cmake-font-lock-activate)

;; flymake
(add-hook 'emacs-lisp-mode-hook #'flymake-mode)
(with-eval-after-load 'flymake
  (require 'flymake-config))

;; flycheck
(with-eval-after-load 'flycheck
  (require 'flycheck-config))

;; lsp
(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-ui-mode)
  (add-hook 'lsp-after-open-hook #'lsp-enable-imenu)
  (require 'lsp-config))

;; dap
(with-eval-after-load 'dap-mode
  (require 'dap-config))

;; [ cc-mode
(add-hook 'c-mode-hook   #'lsp-deferred)
(add-hook 'c++-mode-hook #'lsp-deferred)
(defun c-c++-config ()
  ;; run only once
  (remove-hook 'c-mode-hook 'c-c++-config)
  (remove-hook 'c++-mode-hook 'c-c++-config)
  (require 'c-c++-config)
  ;; After semantic
  ;; After ede-projects-config
  (require 'cmake-make-config))
(add-hook 'c-mode-hook   'c-c++-config)
(add-hook 'c++-mode-hook 'c-c++-config)
;; ]

;; [ rust
(add-hook 'rust-mode-hook #'lsp-deferred)
;; ]

;; [ lua-mode
(with-eval-after-load 'lua-mode
  (require 'lua-config))
;; ]

;; [ python
(add-hook 'python-mode-hook #'lsp-deferred)
(setq python-shell-interpreter "python3")
(with-eval-after-load 'python
  ;;  (require 'semantic/wisent/python)
  (require 'python-config)
  ;;  (add-hook 'python-mode-hook #'detect-python-project-version)
  (with-eval-after-load 'dap-mode
    (require 'dap-python)))


(with-eval-after-load 'virtualenvwrapper
  (require 'virtualenvwrapper-config))
;; ]

;; [ java
(add-hook 'java-mode-hook (lambda ()
                            (when (require 'lsp-java nil t)
                              (lsp-deferred))))
(defun load-once-java-stuff ()
  (with-eval-after-load 'dap-mode
    (require 'dap-java))
  (remove-hook 'java-mode-hook 'load-once-java-stuff))
(add-hook 'java-mode-hook 'load-once-java-stuff)
;; ]

;; [ javascript
(defun lsp-deferred-cond ()
  (unless (derived-mode-p 'ein:ipynb-mode)
    (lsp-deferred)))
(add-hook 'js-mode-hook #'lsp-deferred-cond)
(with-eval-after-load 'js
  (with-eval-after-load 'dap-mode
    (require 'dap-firefox)
    (dap-firefox-setup)))
;; ]

;; [ php
(add-hook 'php-mode-hook #'lsp-deferred)
(with-eval-after-load 'php-mode
  (with-eval-after-load 'dap-mode
    (require 'dap-php))
  (require 'php-config))
;; ]

;; [ web
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(add-to-list 'auto-mode-alist '("\\.tpl\\'" . web-mode))
(with-eval-after-load 'web-mode
  (require 'web-config))
;; ]

;; loads only when necessary
(with-eval-after-load 'rst
  (require 'rst-config))

(with-eval-after-load 'markdown-mode
  (add-hook 'markdown-mode-hook #'smartparens-mode)
  (message "Importing markdown-mode+")
  (require 'markdown-mode+))

;; TODO: implementar la función python-integrated-mode dentro de python-integrated.el
;(autoload 'python-integrated-mode "python-integrated" "Python everywhere" t)
;(add-to-list 'auto-mode-alist '("\\.py\\." . python-integrated-mode))
;(require 'python-integrated)

(add-to-list 'auto-mode-alist '("\\.ptx\\'" . latex-mode))
(with-eval-after-load 'latex
  (require 'latex-config))

;; Enable plantuml-mode for PlantUML files
;; (add-to-list 'auto-mode-alist '("\\.plantuml\\'" . plantuml-mode))
(with-eval-after-load 'plantuml-mode
  (require 'plantuml-config))

;; ein
(with-eval-after-load 'ein-core
  (require 'ein-config))

;; [ org
(defvar org-replace-disputed-keys t)
(with-eval-after-load 'org
  (require 'org-config)
  (require 'org-super-agenda-config)
  (require 'org-appt)
  (require 'gitlab-api)
  (require 'redmine-api))
(add-hook 'org-mode-hook #'org-super-agenda-mode)
(with-eval-after-load 'org-brain
  (require 'org-brain-config))
;; ]

;; <ahk> AutoHotKey programming
(add-to-list 'auto-mode-alist '("\\.ahk\\'" . xahk-mode))

;; [ <elfeed>
(global-set-key (kbd "C-x w") 'elfeed)
(with-eval-after-load 'elfeed
  (require 'elfeed-config))
;; ]

;; [ <emms>
(with-eval-after-load 'emms
  (require 'emms-config))
;; ]

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

(with-eval-after-load 'edit-server
  (require 'edit-server-config))
;; ] <flyspell>
;; ] <Not always required>

;; ---------- ;;
;; Hide modes ;;
;; ---------- ;;
;; Last config file
(require 'machine-config)

;; Usage: emacs --exwm
;; first of all in command-switch-alist
(defun argument--exwm (switch)
  "Command line arg `--exwm'.  SWITCH ignored."
  (require 'exwm-startup-config))
(add-to-list 'command-switch-alist '("--exwm" . argument--exwm))

(defun argument--all (switch)
  "Command line arg `--all'.  SWITCH ignored."
  (require 'semantic)
  (require 'ede)
  (require 'cc-mode)
  (require 'latex)
  (require 'org)
  (require 'yasnippet))
(add-to-list 'command-switch-alist '("--all" . argument--all))
(defun argument--agenda (switch)
  "Command line arg `--agenda'.  SWITCH ignored."
  (require 'org))
(add-to-list 'command-switch-alist '("--agenda" . argument--agenda))
(defun argument--edit-server (switch)
  "Command line arg `--edit-server'.  SWITCH ignored."
  (require 'edit-server)
  (edit-server-start))
(add-to-list 'command-switch-alist '("--edit-server" . argument--edit-server))
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

(put 'list-timers 'disabled nil)
;;; init.el ends here
