;;; typing-config.el --- Multiple typing configurations

;;; Commentary:

;; Usage:
;; (require 'typing-config)

;;; Code:

(setq column-number-mode t)

(setq-default truncate-lines t)
;; Cambia todas las preguntas yes-or-no-p por y-or-n-p
(fset 'yes-or-no-p 'y-or-n-p)
;; Deshabilita insertar una nueva linea al final de los ficheros
;; para que las plantillas de 'yasnippet' no añadan nueva liena
(set 'mode-require-final-newline nil)

(require 'config-lib)
;;;;;;;;;;;;;;;;;;;
;; Coding system ;;
;;;;;;;;;;;;;;;;;;;

(set-default-coding-systems 'utf-8)
(set-clipboard-coding-system 'utf-8)

(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8
      unibyte-display-via-language-environment t
      default-process-coding-system '(utf-8-unix . utf-8-unix)
      file-name-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8-unix)

(eval-and-when-daemon frame
  (with-selected-frame frame
    (unless window-system
      (set-keyboard-coding-system 'utf-8))))

(set-selection-coding-system 'utf-8)
(set-next-selection-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8-unix)
(set-language-environment 'UTF-8)
;;(set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix) ; error: No process
;;(add-to-list 'auto-coding-regexp-alist '("^\xEF\xBB\xBF" . utf-8) t)
;;(add-to-list 'auto-coding-regexp-alist '("\\`\240..." . latin-1))
;;(add-to-list 'process-coding-system-alist '("gud" . utf-8))

;; Toggle coding systems
(defun toggle-buffer-coding-system ()
  (interactive)
  (if (or (equal buffer-file-coding-system 'utf-8-unix)
          (equal buffer-file-coding-system 'utf-8))
      (progn
        (set-buffer-file-coding-system 'iso-8859-1-unix)
        (set-keyboard-coding-system 'iso-8859-1-unix)
        (when (get-buffer-process (current-buffer))
          (set-buffer-process-coding-system 'iso-8859-1-unix 'iso-8859-1-unix)))
    (progn
      (set-buffer-file-coding-system 'utf-8-unix)
      (set-keyboard-coding-system 'utf-8)
      (when (get-buffer-process (current-buffer))
        (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix)))))
;; Acentos
(require 'iso-transl)

;; Busca caracteres no representables
(defun find-next-unsafe-char (&optional coding-system)
  "Find the next character in the buffer that cannot be encoded by
coding-system. If coding-system is unspecified, default to the coding
system that would be used to save this buffer. With prefix argument,
prompt the user for a coding system."
  (interactive "Zcoding-system: ")
  (if (stringp coding-system) (setq coding-system (intern coding-system)))
  (if coding-system nil
    (setq coding-system
          (or save-buffer-coding-system buffer-file-coding-system)))
  (let ((found nil) (char nil) (csets nil) (safe nil))
    (setq safe (coding-system-get coding-system 'safe-chars))
    ;; some systems merely specify the charsets as ones they can encode:
    (setq csets (coding-system-get coding-system 'safe-charsets))
    (save-excursion
      ;;(message "zoom to <")
      (let ((end  (point-max))
            (here (point    ))
            (char  nil))
        (while (and (< here end) (not found))
          (setq char (char-after here))
          (if (or (eq safe t)
                  (< char ?\177)
                  (and safe  (aref safe char))
                  (and csets (memq (char-charset char) csets)))
              nil ;; safe char, noop
            (setq found (cons here char)))
          (setq here (1+ here))) ))
    (and found (goto-char (1+ (car found))))
    found))

;; from ascii to utf8
(defun ascii-to-utf8-forward (pos)
  (interactive (list (point)))
  (dolist (map '(("\\\240" . "á")
                 ("\\\202" . "é")
                 ("\\\241" . "í")
                 ("\\\242" . "ó")
                 ("\\\243" . "ú")
                 ("\\\244" . "ñ")
                 ("\\\245" . "Ñ")
                 ("\\\265" . "Á")
                 ("\\\220" . "É")
                 ("\\\326" . "Í")
                 ("\\\340" . "Ó")
                 ("\\\351" . "Ú")
                 ("\\\204" . "ä")
                 ("\\\211" . "ë")
                 ("\\\213" . "ï")
                 ("\\\224" . "ö")
                 ("\\\201" . "ü")
                 ("\\\216" . "Ä")
                 ("\\\323" . "Ë")
                 ("\\\330" . "Ï")
                 ("\\\231" . "Ö")
                 ("\\\232" . "Ü")))
    (goto-char pos)
    (while (re-search-forward (car map) nil t 1)
      (replace-match (cdr map)))))

;; remove latin1 characters
(defun remove-tildes (string)
  (dolist (map '(("á" . "a")
                 ("é" . "e")
                 ("í" . "i")
                 ("ó" . "o")
                 ("ú" . "u")
                 ("ñ" . "n")
                 ("Ñ" . "N")
                 ("Á" . "A")
                 ("É" . "E")
                 ("Í" . "I")
                 ("Ó" . "O")
                 ("Ú" . "U")
                 ("ä" . "a")
                 ("ë" . "e")
                 ("ï" . "i")
                 ("ö" . "o")
                 ("ü" . "u")
                 ("Ä" . "A")
                 ("Ë" . "E")
                 ("Ï" . "I")
                 ("Ö" . "O")
                 ("Ü" . "U")) string)
    (set 'string (replace-regexp-in-string (car map) (cdr map) string nil t))))

;;;;;;;;;;;;;;;;;
;; Indentation ;;
;;;;;;;;;;;;;;;;;

;; Only spaces without tabs
(setq-default indent-tabs-mode nil
              tab-width 4
              sh-indent-for-case-label 0
              sh-indent-for-case-alt '+)
(setq c-default-style "linux"
      tab-width 4
      indent-tabs-mode nil
      c-basic-offset 4
      python-indent-offset 4
      js-indent-level 4)
(c-set-offset 'innamespace '0)
(c-set-offset 'inextern-lang '0)
(c-set-offset 'inline-open '0)
(c-set-offset 'label '*)
(c-set-offset 'case-label '0)
(c-set-offset 'access-label '/)
;; Line numbers
(add-hook 'prog-mode-hook #'linum-mode)
(setq linum-format "%d")
;; Show new line and tab characters
(with-eval-after-load 'whitespace
  (setcar (cdr (assq 'whitespace-mode minor-mode-alist)) "")
  (setcar (cdr (assq 'global-whitespace-mode minor-mode-alist)) "")
  (setcar (cdr (assq 'global-whitespace-newline-mode minor-mode-alist)) "")
  (setcar (cdr (assq 'whitespace-newline-mode minor-mode-alist)) ""))

(add-hook 'prog-mode-hook #'(lambda ()
                              (interactive)
                              ;; use make-local-variable with all variables
                              (setq-default tab-width 4)
                              (setq tab-width 4)
                              (cond
                               ;; ‘mode-name’
                               ;; Usually a string, but can use any of the constructs for
                               ;; ‘mode-line-format’, which see.
                               ;; Format with ‘format-mode-line’ to produce a string value.
                               ;; Don't use ‘string-equal’ to compare
                               ((or (equal mode-name "C++/l")
                                    (equal mode-name "C++//l"))
                                (set (make-local-variable 'whitespace-line-column) 100))
                               ((or
                                 (equal mode-name "Py")
                                 (equal mode-name "Python"))
                                (set (make-local-variable 'whitespace-line-column) 79))
                               ((equal mode-name "Emacs-Lisp")
                                (set (make-local-variable 'whitespace-line-column) 100))
                               ;;(t (set (make-local-variable 'whitespace-line-column) 80))
                               )
                              (set (make-local-variable 'whitespace-style)
                                   '(face tab newline tab-mark newline-mark))
                              (whitespace-mode)))
(add-hook 'csv-mode-hook #'whitespace-mode)
(setq whitespace-style '(face tab newline tab-mark newline-mark))

(defun whitespace-toggle-lines-tail ()
  (interactive)
  (if (bound-and-true-p whitespace-mode)
      (call-interactively #'whitespace-mode))
  (if (memq 'lines-tail whitespace-style)
      (setq whitespace-style (delq 'lines-tail whitespace-style))
    (push 'lines-tail whitespace-style))
  (call-interactively #'whitespace-mode))


;; ·  183   MIDDLE DOT
;; ¶  182   PILCROW SIGN
;; ↵  8629  DOWNWARDS ARROW WITH CORNER LEFTWARDS
;; ↩  8617 LEFTWARDS ARROW WITH HOOK
;; ⏎  9166 RETURN SYMBOL
;; ▷  9655  WHITE RIGHT POINTING TRIANGLE
;; ▶  9654  BLACK RIGHT-POINTING TRIANGLE
;; → 8594  RIGHTWARDS ARROW
;; ↦ 8614  RIGHTWARDS ARROW FROM BAR
;; ⇤ 8676  LEFTWARDS ARROW TO BAR
;; ⇥ 8677  RIGHTWARDS ARROW TO BAR
;; ⇨ 8680  RIGHTWARDS WHITE ARROW
(eval-and-when-daemon frame
  (setq whitespace-display-mappings
        '(;; (space-mark   ?\     [? ]) ;; use space not dot
          (newline-mark 10   [8629 10] [36 10])
          (tab-mark     9    [8676 32 8677 32] [92 9]))))
(defun whitespace-toggle-marks ()
  (interactive)
  (if (bound-and-true-p whitespace-mode)
      (call-interactively #'whitespace-mode))
  (if (member '(newline-mark 10   [36 10]) whitespace-display-mappings)
      (setq whitespace-display-mappings
            '((newline-mark 10   [8629 10] [36 10])
              (tab-mark     9    [8676 32 8677 32] [92 9])))
    (setq whitespace-display-mappings
          '((newline-mark 10   [36 10])
            (tab-mark     9    [46 95 46 32]))))
  (call-interactively #'whitespace-mode))

(defmacro save-line (&rest body)
  `(let* ((origin (point))
         (line (count-lines 1 origin)))
    ,@body
    (unless (= line (count-lines 1 (point)))
      (goto-char origin))))

;; unescape line
(defun join-lines-unescaping-new-lines ()
  (interactive)
  (save-excursion
    (end-of-line)
    (while (char-equal (char-before) ?\\)
      (left-char)
      (delete-char 2)
      (when (char-equal (char-after) ? )
        (fixup-whitespace))
      (end-of-line))))

(defun break-line-escaping-new-lines ()
  (interactive)
  (save-excursion
    (while (= (1+ whitespace-line-column)
              (move-to-column (1+ whitespace-line-column)))
      (left-char)
      (save-line
       (backward-word))
      (let ((column (current-column)))
        (when (or (< column (/ whitespace-line-column 2))
                  (> column (- whitespace-line-column 2)))
          (move-to-column (- whitespace-line-column 2))))
      (insert "\\")
      (call-interactively #'newline))))

;; Mostrar parentesis (ya lo hace show-smartparents-mode)
;;(show-paren-mode)
;; Narrow enabled
(put 'narrow-to-region 'disabled nil)

;; Graba cada 10 caracteres introducidos a #<file-name>#
(setq auto-save-interval 10
      ;; o cada 10 segundos
      auto-save-timeout 10)

;; No lo activamos por ser muy engorroso
;;(put 'scroll-left 'disabled nil)

;;;;;;;;;;
;; Font ;;
;;;;;;;;;;
;; set a default font
;; $(sudo fc-cache -rfv)
(eval-and-when-daemon frame
  (when (display-graphic-p frame)
    (with-selected-frame frame
      (cond
       ((member "Iosevka Term" (font-family-list)) ;; Iosevka case
        (set-face-attribute 'default nil
                            :family "Iosevka Term"
                            :height 100
                            :foundry "unknown"
                            :weight 'light
                            :slant 'normal
                            :width 'normal))
       ((member "-outline-Iosevka Term Light-light-normal-normal-mono-*-*-*-*-c-*-iso8859-1" (x-list-fonts "*" nil (selected-frame)))
        (set-face-attribute 'default nil
                            :font "-outline-Iosevka Term Light-light-normal-normal-mono-*-*-*-*-c-*-iso8859-1"
                            :height 100))
       ((member "-outline-Unifont-normal-normal-normal-*-*-*-*-*-p-*-iso8859-1" (x-list-fonts "*" nil (selected-frame)))
        (set-face-attribute 'default nil
                            :font "-outline-Unifont-normal-normal-normal-*-*-*-*-*-p-*-iso8859-1"
                            :height 100))
       (t ;; default case
        (set-face-attribute 'default nil
                            :height 100
                            :weight 'light
                            :slant 'normal
                            :width 'normal)))
      (cond
       ((member "DejaVu Sans Mono monospacified for Iosevka Term Light"
                (font-family-list))
        (set-fontset-font "fontset-default" '(#x2100 . #x230F)
                          (font-spec :family "DejaVu Sans Mono monospacified for Iosevka Term Light"))
        (set-fontset-font "fontset-default" '(#x25A0 . #x25FF)
                          (font-spec :family "DejaVu Sans Mono monospacified for Iosevka Term Light"))
        (set-fontset-font "fontset-default" '(#x2692 . #x26A0)
                          (font-spec :family "DejaVu Sans Mono monospacified for Iosevka Term Light")))
       ((member "-outline-DejaVu Sans Mono monospacified -normal-normal-normal-mono-*-*-*-*-c-*-iso8859-1" (x-list-fonts "*" nil (selected-frame)))
        (set-fontset-font "fontset-default" '(#x2100 . #x230F)
                          (font-spec :name "-outline-DejaVu Sans Mono monospacified -normal-normal-normal-mono-*-*-*-*-c-*-iso8859-1"))
        (set-fontset-font "fontset-default" '(#x25A0 . #x25FF)
                          (font-spec :name "-outline-DejaVu Sans Mono monospacified -normal-normal-normal-mono-*-*-*-*-c-*-iso8859-1"))
        (set-fontset-font "fontset-default" '(#x2692 . #x26A0)
                          (font-spec :name "-outline-DejaVu Sans Mono monospacified -normal-normal-normal-mono-*-*-*-*-c-*-iso8859-1")))
       ((member "-outline-Unifont-normal-normal-normal-*-*-*-*-*-p-*-iso8859-1" (x-list-fonts "*" nil (selected-frame)))
        (set-fontset-font "fontset-default" '(#x2100 . #x230F)
                          (font-spec :name "-outline-Unifont-normal-normal-normal-*-*-*-*-*-p-*-iso8859-1"))
        (set-fontset-font "fontset-default" '(#x25A0 . #x25FF)
                          (font-spec :name "-outline-Unifont-normal-normal-normal-*-*-*-*-*-p-*-iso8859-1"))
        (set-fontset-font "fontset-default" '(#x2692 . #x26A0)
                          (font-spec :name "-outline-Unifont-normal-normal-normal-*-*-*-*-*-p-*-iso8859-1"))))
      ;; (unless (or (equal "unspecified-bg" (face-background 'default nil 'default))
      ;;             (equal "unspecified-fg" (face-foreground 'default nil 'default)))
      ;;   (add-hook 'prog-mode-hook #'highlight-indent-guides-mode))
      )))
;;;;;;;;;;;;
;; Moving ;;
;;;;;;;;;;;;
(require 'move-thing)
(move-thing-mode t)

;;;;;;;;;;;
;; Copia ;;
;;;;;;;;;;;
(setq yank-excluded-properties t)

(defun duplicate-region (arg beg end &optional orig)
  "Duplicates ARG times region from BEG to END."
  (let ((origin (or orig end))
        (neg (> 0 arg))
        (argument (abs arg))
        (region (buffer-substring-no-properties beg end)))
    (if neg
        (dotimes (i argument)
          (goto-char end)
          (newline)
          (insert region)
          (setq end (point)))
      (dotimes (i argument)
        (goto-char end)
        (insert region)
        (setq end (point)))
      (set 'origin (- origin argument)))
    (goto-char (+ origin (* (length region) argument) argument))))

(defun duplicate-rectangle-region (arg beg end)
  (let ((region (sort (list beg end) '<)))
    (let ((rectangle (extract-rectangle (cl-first region)
                                        (cl-second region)))
          (bounds (extract-rectangle-bounds (cl-first region)
                                            (cl-second region))))
      (cond
       ((or (= end (cdr (car bounds)))
            (= end (cdr (car (last bounds)))))
        (dotimes (i arg)
            (goto-char (car region))
            (insert-rectangle rectangle)))
       ((= end (car (car bounds)))
        (let ((column (current-column))
              (lines (length bounds))
              backward-lines)
          (setq backward-lines (- 1 (* 2 lines))
                lines (- 1 lines))
          (forward-line lines)
          (dotimes (i arg)
            (unless (= 0 (forward-line backward-lines))
              (error "Not enough lines above"))
            (move-to-column column)
            (insert-rectangle rectangle))
          (forward-line lines)
          (move-to-column column)))
       ((= end (car (car (last bounds))))
        (let ((column (current-column)))
          (dotimes (i arg)
            (let ((line (line-number-at-pos)))
              (forward-line)
              (if (= line (line-number-at-pos))
                  (insert "\n")))
            (move-to-column column)
            (insert-rectangle rectangle))
          (move-to-column column)))))))

(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (if (use-region-p)
      (if rectangle-mark-mode
          (duplicate-rectangle-region arg (mark) (point))
        (let ((region (sort (list (mark) (point)) '<)))
          (duplicate-region arg
                            (cl-first region)
                            (cl-second region)
                            (point))))
    (duplicate-region (- arg)
                      (line-beginning-position)
                      (line-end-position)
                      (point))))
;;;;;;;;;;;
;; Ratón ;;
;;;;;;;;;;;

(require 'ffap)
(global-set-key [S-mouse-3] 'ffap-at-mouse)
(global-set-key [C-S-mouse-3] 'ffap-menu)
(global-set-key "\C-xf" 'find-file-at-point)

;; (global-set-key "\C-x\C-f" 'find-file-at-point)
;; (global-set-key "\C-x\C-r" 'ffap-read-only)
;; (global-set-key "\C-x\C-v" 'ffap-alternate-file)

;; (global-set-key "\C-x4f"   'ffap-other-window)
;; (global-set-key "\C-x5f"   'ffap-other-frame)
;; (global-set-key "\C-x4r"   'ffap-read-only-other-window)
;; (global-set-key "\C-x5r"   'ffap-read-only-other-frame)

;; (global-set-key "\C-xd"    'dired-at-point)
;; (global-set-key "\C-x4d"   'ffap-dired-other-window)
;; (global-set-key "\C-x5d"   'ffap-dired-other-frame)
;; (global-set-key "\C-x\C-d" 'ffap-list-directory)

;; (add-hook 'gnus-summary-mode-hook 'ffap-gnus-hook)
;; (add-hook 'gnus-article-mode-hook 'ffap-gnus-hook)
;; (add-hook 'vm-mode-hook 'ffap-ro-mode-hook)
;; (add-hook 'rmail-mode-hook 'ffap-ro-mode-hook))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                        ;;
;;     Nuevas teclas      ;;
;;                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;
;; Idiomas español e inglés

;; [ Liberamos la combinación de teclas M-f
;; (global-unset-key (kbd "M-f"))
;; copiado y pegado
;; (global-unset-key (kbd "M-w"))
;; (global-unset-key (kbd "C-w"))
;; (global-unset-key (kbd "C-y"))
;; (global-unset-key (kbd "C-c"))
;; (global-unset-key (kbd "C-x"))
;; (global-unset-key (kbd "C-v"))
;; ;; ]
;; ;; [ Teclas de copiado y pegado universales
;; (global-set-key (kbd "C-c") 'kill-ring-save)
;; (global-set-key (kbd "C-x") 'kill)
;; (global-set-key (kbd "C-c") 'yank)
;; ]
;;;;;;;


;; Activa imath-mode
;(global-set-key (kbd "M-n M-m") 'imath-mode)


;; Bookmark handling
;;
;; (global-set-key (kbd "<C-f5>") '(lambda () (interactive) (progn (message "Bookmark f5 added") (bookmark-set "BookMark_f5"))))
;; (global-set-key (kbd "<f5>") '(lambda () (interactive) (bookmark-jump "BookMark_f5")))
;; (global-set-key (kbd "<C-f6>") '(lambda () (interactive) (progn (message "Bookmark f6 added") (bookmark-set "BookMark_f6"))))
;; (global-set-key (kbd "<f6>") '(lambda () (interactive) (bookmark-jump "BookMark_f6")))
;; (global-set-key (kbd "<C-f7>") '(lambda () (interactive) (progn (message "Bookmark f7 added") (bookmark-set "BookMark_f7"))))
;; (global-set-key (kbd "<f7>") '(lambda () (interactive) (bookmark-jump "BookMark_f7")))
;; (global-set-key (kbd "<C-f8>") '(lambda () (interactive) (progn (message "Bookmark f8 added") (bookmark-set "BookMark_f8"))))
;; (global-set-key (kbd "<f8>") '(lambda () (interactive) (bookmark-jump "BookMark_f8")))

(define-key key-translation-map (kbd "<f8> u <down>") (kbd "↓"))
(define-key key-translation-map (kbd "<f8> u <left>") (kbd "←"))
(define-key key-translation-map (kbd "<f8> u <right>") (kbd "→"))
(define-key key-translation-map (kbd "<f8> u <up>") (kbd "↑"))
(define-key key-translation-map (kbd "<f8> u TAB") (kbd "↹"))
(define-key key-translation-map (kbd "<f8> u RET") (kbd "↵"))
(define-key key-translation-map (kbd "<f8> u a") (kbd "α"))
(define-key key-translation-map (kbd "<f8> u A") (kbd "Α"))
(define-key key-translation-map (kbd "<f8> u b") (kbd "β"))
(define-key key-translation-map (kbd "<f8> u B") (kbd "Β"))
(define-key key-translation-map (kbd "<f8> u g") (kbd "γ"))
(define-key key-translation-map (kbd "<f8> u G") (kbd "Γ"))
(define-key key-translation-map (kbd "<f8> u d") (kbd "δ"))
(define-key key-translation-map (kbd "<f8> u D") (kbd "Δ"))
(define-key key-translation-map (kbd "<f8> u e") (kbd "ε"))
(define-key key-translation-map (kbd "<f8> u E") (kbd "Ε"))
(define-key key-translation-map (kbd "<f8> u z") (kbd "ζ"))
(define-key key-translation-map (kbd "<f8> u Z") (kbd "Ζ"))
(define-key key-translation-map (kbd "<f8> u h") (kbd "η"))
(define-key key-translation-map (kbd "<f8> u H") (kbd "Η"))
(define-key key-translation-map (kbd "<f8> u q") (kbd "θ"))
(define-key key-translation-map (kbd "<f8> u Q") (kbd "Θ"))
(define-key key-translation-map (kbd "<f8> u i") (kbd "ι"))
(define-key key-translation-map (kbd "<f8> u I") (kbd "Ι"))
(define-key key-translation-map (kbd "<f8> u k") (kbd "κ"))
(define-key key-translation-map (kbd "<f8> u K") (kbd "Κ"))
(define-key key-translation-map (kbd "<f8> u l") (kbd "λ"))
(define-key key-translation-map (kbd "<f8> u L") (kbd "Λ"))
(define-key key-translation-map (kbd "<f8> u m") (kbd "μ"))
(define-key key-translation-map (kbd "<f8> u M") (kbd "Μ"))
(define-key key-translation-map (kbd "<f8> u n") (kbd "ν"))
(define-key key-translation-map (kbd "<f8> u N") (kbd "Ν"))
(define-key key-translation-map (kbd "<f8> u p") (kbd "π"))
(define-key key-translation-map (kbd "<f8> u P") (kbd "Π"))
(define-key key-translation-map (kbd "<f8> u r") (kbd "ρ"))
(define-key key-translation-map (kbd "<f8> u R") (kbd "Ρ"))
(define-key key-translation-map (kbd "<f8> u s") (kbd "σ"))
(define-key key-translation-map (kbd "<f8> u S") (kbd "Σ"))
(define-key key-translation-map (kbd "<f8> u t") (kbd "τ"))
(define-key key-translation-map (kbd "<f8> u T") (kbd "Τ"))
(define-key key-translation-map (kbd "<f8> u y") (kbd "υ"))
(define-key key-translation-map (kbd "<f8> u Y") (kbd "Υ"))
(define-key key-translation-map (kbd "<f8> u f") (kbd "φ"))
(define-key key-translation-map (kbd "<f8> u F") (kbd "Φ"))
(define-key key-translation-map (kbd "<f8> u x") (kbd "χ"))
(define-key key-translation-map (kbd "<f8> u X") (kbd "Χ"))
(define-key key-translation-map (kbd "<f8> u v") (kbd "Ψ"))
(define-key key-translation-map (kbd "<f8> u V") (kbd "ψ"))
(define-key key-translation-map (kbd "<f8> u w") (kbd "ω"))
(define-key key-translation-map (kbd "<f8> u W") (kbd "Ω"))
(define-key key-translation-map (kbd "<f8> u *") (kbd "×"))
(define-key key-translation-map (kbd "<f8> u /") (kbd "÷"))
(define-key key-translation-map (kbd "<f8> u .") (kbd "…"))
(define-key key-translation-map (kbd "<f8> u +") (kbd "∞"))
(define-key key-translation-map (kbd "<f8> u =") (kbd "≠"))
(define-key key-translation-map (kbd "<f8> u -") (kbd "±"))
(define-key key-translation-map (kbd "<f8> u 0") (kbd "ℵ"))
(define-key key-translation-map (kbd "<f8> u \\") (kbd "∀"))
(define-key key-translation-map (kbd "<f8> u !") (kbd "∃"))
(define-key key-translation-map (kbd "<f8> u |") (kbd "∄"))
(define-key key-translation-map (kbd "<f8> u º") (kbd "∅"))
(define-key key-translation-map (kbd "<f8> u /") (kbd "∈"))
(define-key key-translation-map (kbd "<f8> u %") (kbd "∝"))
(define-key key-translation-map (kbd "<f8> u ç") (kbd "⊆"))
(define-key key-translation-map (kbd "<f8> u Ç") (kbd "⊂"))
(define-key key-translation-map (kbd "<f8> u ñ") (kbd "⊇"))
(define-key key-translation-map (kbd "<f8> u Ñ") (kbd "⊃"))

;;;;;;;;;;;;;;;;
;; Movimiento ;;
;;;;;;;;;;;;;;;;
(defvar horizontal-alt 15)

(defun forward-alt ()
  (interactive)
  (forward-char horizontal-alt))

(defun backward-alt ()
  (interactive)
  (backward-char horizontal-alt))

(defun hscroll-right ()
  (interactive)
  (right-char (min
               horizontal-jump
               (- (save-excursion
                    (end-of-line)
                    (current-column))
                  (current-column)))))

(defun hscroll-left ()
  (interactive)
  (left-char (min
              horizontal-jump
              (- (current-column)
                 (save-excursion
                   (beginning-of-line)
                   (current-column))))))

(require 'string-inflection)


;;;;;;;;;;;;
;; Prompt ;;
;;;;;;;;;;;;
(advice-add 'read-from-minibuffer :around #'message-inhibit-advice)


;;;;;;;;;;;;;;;;;;;;;;
;; Goto last change ;;
;;;;;;;;;;;;;;;;;;;;;;
(require 'goto-chg)

(global-set-key [(control ?.)] 'goto-last-change)
(global-set-key [(control ?,)] 'goto-last-change-reverse)
;;;;;;;;;;
;; Keys ;;
;;;;;;;;;;
(bind-keys
 ("<f1> E"              . manual-entry)
 ("C-M-º"               . indent-region)
 ("M-s º"               . indent-region)
 ("M-A"                 . align-regexp)
 ("M-C"                 . string-inflection-all-cycle)
 ("<f7> d"              . toggle-debug-on-error)
 ("<f7> s"              . toggle-enable-multibyte-characters)
 ("<f7> c"              . toggle-buffer-coding-system)
 ("<f7> w"              . toggle-truncate-lines)
 ("<f7> l"              . whitespace-toggle-lines-tail)
 ("<f7> RET"            . whitespace-toggle-marks)
 ("M-s c b"             . backward-kill-word)
 ("C-<left>"            . left-word)
 ("C-<right>"           . right-word)
 ("S-<backspace>"       . backward-kill-sexp)
 ("M-s s b"             . backward-kill-sexp)
 ("M-s DEL"             . backward-kill-sexp)
 ("S-<delete>"          . kill-sexp)
 ("M-s <deletechar>"    . kill-sexp)
 ("C-*"                 . duplicate-current-line-or-region)
 ("M-s *"               . duplicate-current-line-or-region)
 ("M-s SPC"             . set-mark-command)
 ("M-SPC"               . fixup-whitespace)
 ("M-s a SPC"           . fixup-whitespace)
 ("C-S-<backspace>"     . kill-whole-line)
 ("M-s c s b"     . kill-whole-line)
 ("M-s <insertchar>"    . kill-whole-line)
 ("<M-dead-circumflex>" . delete-indentation)
 ("S-<next>"            . scroll-other-window)
 ("S-<prior>"           . scroll-other-window-down)
 ("C-<next>"            . hscroll-right)
 ("M-s <next>"          . hscroll-right)
 ("C-<prior>"           . hscroll-left)
 ("M-s <prior>"         . hscroll-left)
 ("M-<right>"           . forward-alt)
 ("M-<left>"            . backward-alt)
 ("C-ñ"                 . find-next-unsafe-char))

;; Usa el clipboard del sistema
;; (global-set-key [(shift delete)] 'clipboard-kill-region)
;; (global-set-key [(control insert)] 'clipboard-kill-ring-save)
;; (global-set-key [(shift insert)] 'clipboard-yank)


(provide 'typing-config)
;;; typing-config.el ends here
