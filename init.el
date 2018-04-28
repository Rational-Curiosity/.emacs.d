;;; init.el --- Initial loaded file

;;; Commentary:

;; Compile with
;; `emacs -batch -f batch-byte-compile emacs.el'
;; and rename
;; `mv emacs.elc init.elc'
;;
;; Keys:
;;
;; C-h m                     - Lista todos los atajos de teclado del modo activo.
;;
;; M-m                       - Comienzo del texto de la línea.
;; C-M-f                     - Salta al final del paréntesis
;; C-M-b                     - Salta al comienzo del paréntesis
;;
;; M-=                       - Cuenta líneas, palabras y caracteres en la región.
;; C-c v                     - La ventana actual crece la mitad de la siguiente.
;; C-/  ó  C-_               - Undo.
;; M-;                       - Comentar/Descomentar región.
;; C-x TAB                   - Indentado de la región con las flechas.
;; C-u n C-x TAB             - Indenta n espacios en la región, n puede ser negativo.
;; C-x r m                   - Crear un marcador de línea.
;; C-x r b                   - Saltar a un marcador de línea.
;; C-x r l                   - Listar marcadores de línea.
;; M-x bookmark-delete       - Borrar marcador de línea.
;; C-x r k                   - Corta rectángulo.
;; C-x r y                   - Pega rectángulo.
;; C-x r o                   - Añade rectángulo de espacios.
;; C-x C-+ + +...            - Zoom in.
;; C-x C-- - -...            - Zoom out.
;;
;; C-x n n                   - Edita sólo la región.
;; C-x n w                   - Edita todo el documento.
;;
;; C-x (                     - Comienzo de macro
;; C-x )                     - Final de macro
;; C-x e                     - Ejecuta último macro
;; C-u <n> C-x e             - Ejecuta último macro n veces
;; C-x C-e                   - Ejecuta el código Lisp que precede al cursor
;; C-x C-k n                 - Nombra el último macro
;; C-c !                     - Ejecuta el comando en maxima.
;; C-c &                     - Recupera el comando ejecutado.
;; C-u C-c &                 - Recupera el comando ejecutado con la salida en latex.
;; C-c $                     - Muestra todas las imágenes de las salidas.
;; C-c [                     - Añade {maxima maxima}
;; C-c ]                     - Añade {latex latex}
;;
;; ORG MODE
;; <TAB>                     - Expandir o contraer en nivel del cursor.
;; S-<TAB>                   - Expandir o contraer todos los niveles.
;; C-c C-e l o               - Exportar .org a latex y visualizar.
;; C-c C-e # default         - Generar cabecera de configuraciones y título.
;; C-c C-c                   - Marca casilla [ ] o inserta un tag o evalua código.
;; S-RArrow S-LArrow         - Rotar tipos ej: TODO -> DONE ->
;; C-c C-t KEY               - Poner tipo ej: KEY es t se pone a TODO(t)
;; C-c >                     - Inicia calendario.
;; C-c <                     - Inserta fecha.
;; C-c .                     - Inserta fecha y hora.
;; C-c !                     - Inserta fecha y hora inactiva.
;; C-u C-c .                 - Inserta fecha y hora abreviadas.
;; C-u C-c !                 - Inserta fecha y hora inactiva abreviadas.
;; S-Up S-Down               - +1 -1 en los campos del cursor.
;; C-c C-d                   - Inserta DEADLINE.
;; C-c C-s                   - Inserta SCHEDULED.
;; C-c C-x C-l               - Previsualizar la fórmula latex.
;; C-u C-c C-x C-l           - Previsualizar todas las fórmulas de la sección.
;; C-u C-u C-c C-x C-l       - Previsualizar todas las fórmulas del documento.
;; C-c C-v e                 - Evalua entorno src.
;; C-c '                     - Edita el entorno src en modo programación.
;;
;; ORG MODE :: TABLES
;; C-c |                     - Convierte la región selecionada en una tabla.
;; C-c C-c                   - Realinea la tabla.
;; <TAB>                     - Salta al siguiente campo y realinea la tabla.
;; S-<TAB>                   - Salta al campo anterior y realinea la tabla.
;; <RET>                     - Salta a la siguiente fila o la crea.
;; M-a                       - Cursor al principio de la celda.
;; M-e                       - Cursor al final de la celda.
;; M-flechas                 - Mueve la fila o columna con el cursor.
;; M-S- <right> o <down>     - Añade columna o fila.
;; M-S- <left> o <up>        - Elimina columna o fila.
;; C-c -                     - Inserta una línea separadora horizontal.
;; C-c C-x d                 - Inserta un bloque drawer
;;
;; C-x * q                   - Cálculo rápido (C-y para pegar en buffer).
;;
;; M-x set-variable          - Asignar valor a variable.
;; M-x describe-variable     - Ver descripción y valor de la variable.
;;
;; M-x load-theme            - Cargar un tema de color.
;; M-x disable-theme         - Desactiva un tema de color.

;; |----------+------------------|
;; | Sequence | Expands to       |
;; |----------+------------------|
;; | <s       | #+BEGIN_SRC      |
;; | <e       | #+BEGIN_EXAMPLE  |
;; | <q       | #+BEGIN_QUOTE    |
;; | <v       | #+BEGIN_VERSE    |
;; | <V       | #+BEGIN_VERBATIM |
;; | <c       | #+BEGIN_CENTER   |
;; | <l       | #+BEGIN_LaTeX    |
;; | <L       | #+LaTeX          |
;; | <h       | #+BEGIN_HTML     |
;; | <H       | #+HTML           |
;; | <a       | #+BEGIN_ASCII    |
;; | <A       | #+ASCII:         |
;; | <i       | #+INDEX:         |
;; | <I       | #+INCLUDE:       |
;; |----------+------------------|

;;; Code:

;; Start emacs server
;; (require 'server)
;; (unless (server-running-p)
;;   (server-start))

;; server-visit-hook is run every time the server is called (when call emacsclient).
;; server-done-hook Hook run when done editing a buffer for the Emacs server.

;; Cargaremos las configuraciones desde otros ficheros
;; Se evalua al compilar y cuando se arranca el programa

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(require 'cl-lib)
(eval-when-compile
  (require 'cl))

(eval-and-compile
  (let ((default-directory "~/.emacs.d/el"))
    (normal-top-level-add-subdirs-to-load-path))
  (require 'config-lib))

;;(modify-frame-parameters nil '((wait-for-wm . nil)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (org org-plus-contrib pyvenv csharp-mode hierarchy json-reformat json-snatcher xahk-mode json-navigator json-mode company-lua flymake-lua lua-mode ht cursor-chg org-super-agenda company-emacs-eclim eclim memoize highlight-indent-guides flymake ox-gfm smart-mode-line undo-tree yasnippet-snippets yasnippet vlf smartscan highlight-thing f hide-comnt vimish-fold avy thing-cmds protobuf-mode csv-mode markdown-mode+ gnuplot gnuplot-mode sphinx-doc sphinx-frontend deferred request request-deferred org-trello ox-rst plantuml-mode stickyfunc-enhance org-agenda-property ox-twbs markdown-mode async auctex bind-key cmake-mode company dash epl let-alist pkg-info popup projectile rich-minority s seq web-completion-data flycheck helm helm-core irony elpy highlight-indentation find-file-in-project ivy company-go dart-mode go-mode ob-dart ob-go srefactor free-keys rtags company-irony-c-headers company-irony helm-flyspell helm-gtags which-key transpose-frame string-inflection smooth-scrolling smartparens rebox2 rainbow-delimiters org-bullets multiple-cursors minimap hydra htmlize helm-projectile helm-flycheck helm-company helm-bind-key helm-ag graphviz-dot-mode figlet expand-region emacs-cl dash-functional company-web company-shell company-c-headers company-auctex cmake-font-lock clang-format bash-completion android-mode ag))))

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
;;        Paquetes        ;;
;;                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package-config)
(require 'helm-bind-key) ; no está mostrando las teclas de los submodos
(require 'bind-key)

(require 'undo-tree)
(global-undo-tree-mode)
;; (setq undo-tree-visualizer-diff t
;;       undo-tree-visualizer-timestamps t
;;       undo-tree-visualizer-relative-timestamps t)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               ;;
;;   Configuration files         ;;
;;                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; first of all
(load "config" t)

;; [ <Always required>
(require 'helm-extensions-config)

(require 'company-extensions-config)

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

(require 'shell-config)

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

(require 'ediff-config)

(require 'vlf-config)

(require 'version-control-config)

;; csv-mode
(add-to-list 'auto-mode-alist '("\\.tsv\\'" . csv-mode))
;; not working with with-eval-after-load 'csv-mode
(require 'csv-config)

;; [ Poor performance
;; (require 'sublimity-config)
;; <xor>
(require 'minimap-config)
;; ]
;; ] <Always required>

(require 'figlet-config)


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
;; flycheck-mode
(add-hook 'prog-mode-hook #'flycheck-mode)
(with-eval-after-load 'flycheck
  (require 'flycheck-config))
;; elisp-mode
(with-eval-after-load 'flycheck  ;; trick
  (with-current-buffer "*scratch*"
    (lisp-interaction-mode))
  (require 'semantic-config)
  (require 'srefactor-config))
;; [ <c-c++> Programación en c y c++
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-hook 'c-mode-common-hook #'projectile-mode)
(require 'ede)
(add-hook 'c-mode-common-hook #'ede-minor-mode)
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
;; cc-mode
(with-eval-after-load 'cc-mode
  (require 'c-c++-config)
  (require 'gud-config)
  (require 'semantic-config)
  ;; stickfunc improved
  (require 'stickyfunc-enhance)
  (require 'speedbar-config)
  (require 'srefactor-config)
  ;; Después de semantic
  ;; Después de ede-projects-config
  (require 'cmake-make-config)
  ;; [[ Despues de semantic
  (require 'projectile-config)
  (require 'ede-config)
  ;; [ rtags: Jumping around code
  (when (executable-find "rdm")
    (add-hook 'c-mode-hook   #'rtags-start-process-unless-running)
    (add-hook 'c++-mode-hook #'rtags-start-process-unless-running))
  (with-eval-after-load 'rtags
    (require 'rtags-config))
  ;; ]
  ;; [ irony: autocomplete code
  (when (file-exists-p (locate-user-emacs-file "irony/bin/irony-server"))
    (add-hook 'c++-mode-hook  #'irony-mode)
    (add-hook 'c-mode-hook    #'irony-mode)
    (add-hook 'objc-mode-hook #'irony-mode)
    (with-eval-after-load 'irony
      (require 'irony-config)))
  ;; ]
  ;; ]]
  )
;; poor performance
;; (add-hook 'c-mode-common-hook #'highlight-indentation-mode)
;; ] <c-c++>
;; java
(with-eval-after-load 'eclim
  (require 'eclim-config))

;; Snippets
(add-hook 'prog-mode-hook #'yas-minor-mode)
(add-hook 'org-mode-hook #'yas-minor-mode)
(with-eval-after-load 'yasnippet
  (require 'yasnippet-config))

;; Realmente solo se carga cuando se necesita
(with-eval-after-load 'rst
  (require 'rst-config))

(with-eval-after-load 'markdown-mode
  (message "Importing markdown-mode+")
  (require 'markdown-mode+))

;; Bloque python: solo se carga cuando se necesita
(add-hook 'python-mode-hook #'sphinx-doc-mode)
(with-eval-after-load 'sphinx-doc
  (require 'sphinx-doc-config))
(with-eval-after-load 'python
  (require 'semantic-config)
  ;; stickfunc improved
  (require 'stickyfunc-enhance)
  (require 'python-config))
;; packages elpy-mode and python-mode are incompatible
;; among other things for duplicate features
(add-hook 'python-mode-hook #'elpy-mode)
(add-hook 'pyvenv-post-activate-hooks 'elpy-rpc--disconnect)
(with-eval-after-load 'elpy
  (require 'elpy-env-config))


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

(defvar org-replace-disputed-keys t)
(with-eval-after-load 'org
  (require 'org-config)
  (require 'org-super-agenda-config)
  (require 'org-appt))
(unless (getenv "JOB_FOLDER")
  (add-hook 'org-mode-hook #'org-super-agenda-mode))

(with-eval-after-load 'android-mode
  (require 'android-config))


;; (add-hook 'c-mode-common-hook 'cscope-minor-mode)
;; (with-eval-after-load 'cscope-minor-mode
;;   (require 'xcscope-config))

;; <ahk> Programación en AutoHotKey
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
;; Usage: emacs -diff file/dir1 file/dir2
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

;;; init.el ends here
