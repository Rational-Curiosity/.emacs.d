;;; typing-config.el --- Multiple typing configurations

;;; Commentary:

;; Usage:
;; (require 'typing-config)

;;; Code:
(require 'config-lib)

(set-face-attribute 'line-number-current-line nil
                    :foreground "#ebdb34"
                    :bold t)

(setq-default
 ;; t -   long lines go away from window width, ei, continuation lines
 ;; nil - soft wrap lines
 truncate-lines t
 ;; ignore case searching
 case-fold-search t)
;; [ Demasiado agresivo, mejor con ido
;; Cambia todas las preguntas yes-or-no-p por y-or-n-p
;; (fset 'yes-or-no-p 'y-or-n-p)
;; ]
;; Option 2
;; (defun yes-or-no-p (prompt)
;;   (interactive)
;;   (pcase (downcase (read-string (concat prompt "(yes, no) ")))
;;     ("y" t)
;;     ("ye" t)
;;     ("yes" t)
;;     (_ nil)))
(defun yes-or-no-p (prompt)
  (interactive)
  (string-equal
   "yes"
   (completing-read prompt '("yes" "no") nil t nil nil "no")))

(defun insert-utf8 (&optional name)
  (interactive)
  (let ((utf8-hash-table (ucs-names)))
    (insert (gethash (completing-read
                      "Unicode character name: "
                      (hash-table-keys utf8-hash-table)
                      nil t)
                     utf8-hash-table))))

(setq jit-lock-defer-time 0.0
      jit-lock-stealth-time 2.0
      echo-keystrokes 0.5
      column-number-mode t
      isearch-lazy-count t
      isearch-allow-scroll 'unlimited
      ;; mark-ring
      set-mark-command-repeat-pop t
      mark-ring-max 32
      global-mark-ring-max 128
      ;; Deshabilita insertar una nueva linea al final de los ficheros
      ;; para que las plantillas de 'yasnippet' no añadan nueva liena
      mode-require-final-newline nil)
;;;;;;;;;;;;;;;
;; Mark ring ;;
;;;;;;;;;;;;;;;
(require 'move-thing)

(defun push-mark--pre-command ()
  (when (and (null mark-active)
             (gethash this-original-command mt-movement-commands)
             (not (gethash last-command mt-movement-commands)))
    (push-mark nil t)))
(add-hook 'pre-command-hook 'push-mark--pre-command)

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
      (let ((process (get-buffer-process (current-buffer))))
        (set-buffer-file-coding-system 'iso-8859-1-unix)
        (set-keyboard-coding-system 'iso-8859-1-unix)
        (when process
          (set-process-coding-system process 'iso-8859-1-unix 'iso-8859-1-unix)))
    (let ((process (get-buffer-process (current-buffer))))
      (set-buffer-file-coding-system 'utf-8-unix)
      (set-keyboard-coding-system 'utf-8)
      (when process
        (set-process-coding-system process 'utf-8-unix 'utf-8-unix)))))
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
(defun ascii-to-utf8-forward (beg end)
  (interactive (list (point) (point-max)))
  (save-excursion
    (let ((case-fold-search nil))
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
        (goto-char beg)
        (while (search-forward (car map) end t 1)
          (replace-match (cdr map) t t))))))

(defun utf8-fix-wrong-ascii (beg end)
  (interactive (list (point) (point-max)))
  (save-excursion
    (let ((case-fold-search nil))
      (dolist (map '(("›" . "â\x0080º") ;; \200
                     ("🐜" . "ð\x009f\x0090\x009c"))) ;; \237\220\234
        (goto-char beg)
        (while (search-forward (cdr map) end t 1)
          (replace-match (car map) t t))))))

(defun utf8-fix-wrong-latin (beg end)
  (interactive (list (point) (point-max)))
  (save-excursion
    (let ((case-fold-search nil))
      (dolist (map '(("À" . "Ã€")
                     ("Â" . "Ã‚")
                     ("Ã" . "Ãƒ")
                     ("Ä" . "Ã„")
                     ("Å" . "Ã…")
                     ("Æ" . "Ã†")
                     ("Ç" . "Ã‡")
                     ("È" . "Ãˆ")
                     ("É" . "Ã‰")
                     ("Ê" . "ÃŠ")
                     ("Ë" . "Ã‹")
                     ("Ì" . "ÃŒ")
                     ("Î" . "ÃŽ")
                     ("Ñ" . "Ã‘")
                     ("Ò" . "Ã’")
                     ("Ó" . "Ã“")
                     ("Ô" . "Ã”")
                     ("Õ" . "Ã•")
                     ("Ö" . "Ã–")
                     ("×" . "Ã—")
                     ("Ø" . "Ã˜")
                     ("Ù" . "Ã™")
                     ("Ú" . "Ãš")
                     ("Û" . "Ã›")
                     ("Ü" . "Ãœ")
                     ("Þ" . "Ãž")
                     ("ß" . "ÃŸ")
                     ("á" . "Ã¡")
                     ("â" . "Ã¢")
                     ("ã" . "Ã£")
                     ("ä" . "Ã¤")
                     ("å" . "Ã¥")
                     ("æ" . "Ã¦")
                     ("ç" . "Ã§")
                     ("è" . "Ã¨")
                     ("é" . "Ã©")
                     ("ê" . "Ãª")
                     ("ë" . "Ã«")
                     ("ì" . "Ã¬")
                     ("í" . "Ã­")
                     ("î" . "Ã®")
                     ("ï" . "Ã¯")
                     ("ð" . "Ã°")
                     ("ñ" . "Ã±")
                     ("ò" . "Ã²")
                     ("ó" . "Ã³")
                     ("ô" . "Ã´")
                     ("õ" . "Ãµ")
                     ("ö" . "Ã¶")
                     ("÷" . "Ã·")
                     ("ø" . "Ã¸")
                     ("ù" . "Ã¹")
                     ("ú" . "Ãº")
                     ("û" . "Ã»")
                     ("ü" . "Ã¼")
                     ("ý" . "Ã½")
                     ("þ" . "Ã¾")
                     ("ÿ" . "Ã¿")
                     ("Á" . "Ã")))
        (goto-char beg)
        (while (search-forward (cdr map) end t 1)
          (replace-match (car map) t t))))))

;; remove latin1 characters
(defun remove-tildes (string)
  (let ((case-fold-search t))
    (dolist (map '(("á" . "a")
                   ("é" . "e")
                   ("í" . "i")
                   ("ó" . "o")
                   ("ú" . "u")
                   ("ñ" . "n")
                   ("ä" . "a")
                   ("ë" . "e")
                   ("ï" . "i")
                   ("ö" . "o")
                   ("ü" . "u")) string)
      (set 'string (replace-regexp-in-string (car map) (cdr map) string nil t)))))

;;;;;;;;;;;;;
;; Control ;;
;;;;;;;;;;;;;
(when (executable-find "setxkbmap")
  (defun xkb-swap-ctrl-caps (&optional arg)
    (interactive "P")
    (if arg
        (start-process " *setxkbmap" nil
                       "setxkbmap" "-option")
      (start-process " *setxkbmap" nil
                     "setxkbmap" "-option" "ctrl:swapcaps")))
  (defun xkb-swap-ctrl-win (&optional arg)
    (interactive "P")
    (if arg
        (start-process " *setxkbmap" nil
                       "setxkbmap" "-option")
      (start-process " *setxkbmap" nil
                     "setxkbmap" "-option" "ctrl:swap_lwin_lctl"))))

(when (executable-find "xkbcomp")
  (defun xkb-swap-ralt-ctrl (&optional arg)
    (interactive "P")
    (let ((xkb-path (expand-file-name "~/.emacs.d/cache/xkb")))
     (if arg
         (start-process " *xkbcomp" nil
                        "xkbcomp" (concat "-I" xkb-path)
                        (concat xkb-path "/keymap/kbd")
                        (getenv "DISPLAY"))
       (start-process " *xkbcomp" nil
                      "xkbcomp" (concat "-I" xkb-path)
                      (concat xkb-path "/keymap/kbd_swap_ralt_ctrl")
                      (getenv "DISPLAY")))))
  (when (display-graphic-p)
    (xkb-swap-ralt-ctrl)))

;;;;;;;;;;;;;;;;;
;; Indentation ;;
;;;;;;;;;;;;;;;;;
;; Only spaces without tabs
(setq-default indent-tabs-mode nil
              tab-width 4
              sh-indent-for-case-label 0
              sh-indent-for-case-alt '+)
;; long lines
(global-so-long-mode 1)
;; Line numbers
(require 'display-line-numbers)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(setq line-number-display-limit-width 500
      display-line-numbers-width-start t
      display-line-numbers-grow-only t
      display-line-numbers-type 'visual
      ;; styles
      c-default-style "linux"
      tab-width 4
      indent-tabs-mode nil
      c-basic-offset 4
      python-indent-offset 4
      js-indent-level 4)

(defvar display-line-type-selected-last-buffer nil)
(defun display-line-type-by-selected (window)
  (when (buffer-live-p display-line-type-selected-last-buffer)
    (with-current-buffer display-line-type-selected-last-buffer
      (setq display-line-numbers t))
    (setq display-line-type-selected-last-buffer nil))
  (when (and display-line-numbers-mode
             (derived-mode-p 'prog-mode))
    (unless (eq display-line-numbers 'visual)
          (setq display-line-numbers 'visual))
    (setq display-line-type-selected-last-buffer (current-buffer))))
(add-hook 'window-selection-change-functions #'display-line-type-by-selected)

(c-set-offset 'innamespace '0)
(c-set-offset 'inextern-lang '0)
(c-set-offset 'inline-open '0)
(c-set-offset 'label '*)
(c-set-offset 'case-label '0)
(c-set-offset 'access-label '/)

(define-key indent-rigidly-map (kbd "M-f") #'indent-rigidly-right-to-tab-stop)
(define-key indent-rigidly-map (kbd "M-b") #'indent-rigidly-left-to-tab-stop)
(define-key indent-rigidly-map (kbd "C-f") #'indent-rigidly-right)
(define-key indent-rigidly-map (kbd "C-b") #'indent-rigidly-left)

(defun whitespace-case-mode-configuration ()
  (interactive)
  (cond
   ;; ‘mode-name’
   ;; Usually a string, but can use any of the constructs for
   ;; ‘mode-line-format’, which see.
   ;; Format with ‘format-mode-line’ to produce a string value.
   ;; Don't use ‘string-equal’ to compare
   ((derived-mode-p 'c-mode)
    (set (make-local-variable 'whitespace-line-column) 100)
    (setq tab-width 4))
   ((derived-mode-p 'python-mode)
    (set (make-local-variable 'whitespace-line-column) 79)
    (setq tab-width 4))
   ((derived-mode-p 'emacs-lisp-mode)
    (set (make-local-variable 'whitespace-line-column) 100)
    (set (make-local-variable 'whitespace-display-mappings)
         '((newline-mark 10   [8629 10] [36 10])
           (tab-mark     9    [8676 32 32 32 32 32 8677 32] [92 9])))
    (setq tab-width 8))
   ((derived-mode-p 'json-mode)
    (setq tab-width 2))
   (t
    (setq tab-width 4)))
  (whitespace-mode))
(add-hook 'prog-mode-hook #'whitespace-case-mode-configuration)
(add-hook 'csv-mode-hook #'whitespace-mode)

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
;; ↩  8617  LEFTWARDS ARROW WITH HOOK
;; ⏎  9166  RETURN SYMBOL
;; ▷  9655  WHITE RIGHT POINTING TRIANGLE
;; ▶  9654  BLACK RIGHT-POINTING TRIANGLE
;; →  8594  RIGHTWARDS ARROW
;; ↦  8614  RIGHTWARDS ARROW FROM BAR
;; ⇤  8676  LEFTWARDS ARROW TO BAR
;; ⇥  8677  RIGHTWARDS ARROW TO BAR
;; ⇨  8680  RIGHTWARDS WHITE ARROW
(eval-and-when-daemon frame
  (setq whitespace-style '(face
                           tab
                           newline
                           tab-mark
                           newline-mark
                           ;; spaces
                           )
        ;; whitespace-space-regexp "\\( \\{2,\\}\\)"
        ;; whitespace-hspace-regexp "\\(\xA0\\{2,\\}\\)"
        whitespace-display-mappings
        '(;; (space-mark   ?\    [?.])
          ;; (space-mark   ?\xA0 [?_])
          (newline-mark 10    [8629 10] [36 10])
          (tab-mark     9     [8676 32 8677 32] [92 9]))))
(with-eval-after-load 'whitespace
  (setcar (cdr (assq 'whitespace-mode minor-mode-alist)) nil)
  (setcar (cdr (assq 'global-whitespace-mode minor-mode-alist)) nil)
  (setcar (cdr (assq 'global-whitespace-newline-mode minor-mode-alist)) nil)
  (setcar (cdr (assq 'whitespace-newline-mode minor-mode-alist)) nil)
  (set-face-attribute 'whitespace-space nil
                      :foreground 'unspecified
                      :background "grey40")
  (set-face-attribute 'whitespace-hspace nil
                      :foreground 'unspecified
                      :background "grey50"))

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
       ((member "Hack" (font-family-list))
        (set-face-attribute 'default nil
                            :family "Hack"
                            :height 90
                            :foundry "unknown"
                            :weight 'regular
                            :slant 'normal
                            :width 'normal)
        (message "Monospace font: Hack Family"))
       ((member "DejaVu Sans Mono" (font-family-list))
        (set-face-attribute 'default nil
                            :family "DejaVu Sans Mono"
                            :height 100
                            :foundry "unknown"
                            :weight 'regular
                            :slant 'normal
                            :width 'normal)
        (message "Monospace font: DejaVu Sans Mono Family"))
       ((member "Iosevka Term" (font-family-list)) ;; Iosevka case
        (set-face-attribute 'default nil
                            :family "Iosevka Term"
                            :height 100
                            :foundry "unknown"
                            :weight 'light
                            :slant 'normal
                            :width 'normal)
        (message "Monospace font: Iosevka Term Family"))
       ((member "-outline-Iosevka Term Light-light-normal-normal-mono-*-*-*-*-c-*-iso8859-1"
                (x-list-fonts "*" nil (selected-frame)))
        (set-face-attribute 'default nil
                            :font "-outline-Iosevka Term Light-light-normal-normal-mono-*-*-*-*-c-*-iso8859-1"
                            :height 100)
        (message "Monospace font: Iosevka Term Light"))
       ((member "-outline-Unifont-normal-normal-normal-*-*-*-*-*-p-*-iso8859-1"
                (x-list-fonts "*" nil (selected-frame)))
        (set-face-attribute 'default nil
                            :font "-outline-Unifont-normal-normal-normal-*-*-*-*-*-p-*-iso8859-1"
                            :height 100)
        (message "Monospace font: Unifont"))
       (t ;; default case
        (message "Monospace font not found")
        (set-face-attribute 'default nil
                            :height 100
                            :weight 'light
                            :slant 'normal
                            :width 'normal)))
      ;; [ Iosevka 3.0.0 supports unicode
      ;; (let ((font-spec-args
      ;;        (cond
      ;;         ((member "DejaVu Sans Mono monospacified for Iosevka Term Light"
      ;;                  (font-family-list))
      ;;          '(:family "DejaVu Sans Mono monospacified for Iosevka Term Light"))
      ;;         ((member "-outline-DejaVu Sans Mono monospacified -normal-normal-normal-mono-*-*-*-*-c-*-iso8859-1"
      ;;                  (x-list-fonts "*" nil (selected-frame)))
      ;;          '(:name "-outline-DejaVu Sans Mono monospacified -normal-normal-normal-mono-*-*-*-*-c-*-iso8859-1"))
      ;;         ((member "-outline-Unifont-normal-normal-normal-*-*-*-*-*-p-*-iso8859-1"
      ;;                  (x-list-fonts "*" nil (selected-frame)))
      ;;          '(:name "-outline-Unifont-normal-normal-normal-*-*-*-*-*-p-*-iso8859-1")))))
      ;;   (if (null font-spec-args)
      ;;       (message "Monospace utf-8 font not found.")
      ;;     (dolist (range '((#x2100 . #x230F)
      ;;                        (#x2380 . #x23F3)
      ;;                        (#x2420 . #x2424)
      ;;                        (#x25A0 . #x25FF)
      ;;                        (#x2610 . #x2613)
      ;;                        (#x2692 . #x26A0)
      ;;                        (#x26D2 . #x26D4)
      ;;                        (#x2709 . #x270C)))
      ;;         (set-fontset-font "fontset-default" range
      ;;                           (apply 'font-spec font-spec-args)))
      ;;     (message "Monospace utf-8 font: %s" (or (plist-get font-spec-args :family)
      ;;                                             (plist-get font-spec-args :name)))))
      ;; ]
      )))

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

;;;;;;;;;;;;;;;;;;;;
;; Big movements  ;;
;;;;;;;;;;;;;;;;;;;;
(defun window-width-without-margin (&optional window pixelwise)
  (- (window-width window pixelwise)
     hscroll-margin
     (if display-line-numbers
         (if (numberp display-line-numbers-width)
             display-line-numbers-width
           3)
       0)))

(defvar recenter-horizontal-last-op nil)
(defvar recenter-horizontal-positions '(middle left right))

(defun recenter-horizontal (&optional arg)
  "Make the ARG or point horizontally centered in the window."
  (interactive "P")
  (setq arg (or arg (current-column))
        recenter-horizontal-last-op (if (eq this-command last-command)
                                        (car (or (cdr (member
                                                       recenter-horizontal-last-op
                                                       recenter-horizontal-positions))
                                                 recenter-horizontal-positions))
                                      (car recenter-horizontal-positions)))
  (pcase recenter-horizontal-last-op
    ('middle
     (let ((mid (/ (window-width-without-margin) 2)))
       (if (< mid arg)
           (set-window-hscroll (selected-window)
                               (- arg mid)))))
    ('left
     (set-window-hscroll (selected-window) arg))
    ('right
     (let ((width (window-width-without-margin)))
       (if (< width arg)
           (set-window-hscroll (selected-window)
                               (- arg width)))))))

(defvar horizontal-alt 15)

(defun forward-alt ()
  (interactive)
  (forward-char horizontal-alt))

(defun backward-alt ()
  (interactive)
  (backward-char horizontal-alt))

(defun hscroll-right (arg)
  (interactive "p")
  (let ((width (window-width-without-margin))
        (col (current-column)))
    (let ((pos (max 0 (* (+ (/ col width) arg) width))))
      (move-to-column (+ pos hscroll-margin 1))
      (set-window-hscroll (selected-window) pos))))

(defun hscroll-left (arg)
  (interactive "p")
  (hscroll-right (- arg)))

;;;;;;;;;;;;
;; Prompt ;;
;;;;;;;;;;;;
;(advice-add 'read-from-minibuffer :around #'message-inhibit-advice)

(defvar minibuffer-try-this-command nil
  "Store current this-command on minibuffer")

(defun pre-minibuffer-try ()
  (setq minibuffer-try-this-command this-command
        this-command (lambda () (interactive)
                       (condition-case _
                           (call-interactively minibuffer-try-this-command)
                         (text-read-only (goto-char (minibuffer-prompt-end)))
                         (beginning-of-buffer (goto-char (minibuffer-prompt-end)))
                         (end-of-buffer (goto-char (point-max)))))))
(defun post-minibuffer-try ()
  (setq this-command minibuffer-try-this-command
        minibuffer-try-this-command nil))

(defun minibuffer-try-add-hooks ()
  (add-hook 'pre-command-hook 'pre-minibuffer-try)
  (add-hook 'post-command-hook 'post-minibuffer-try))
(defun minibuffer-try-remove-hooks ()
  (remove-hook 'post-command-hook 'post-minibuffer-try)
  (remove-hook 'pre-command-hook 'pre-minibuffer-try))

(add-hook 'minibuffer-setup-hook 'minibuffer-try-add-hooks)
(add-hook 'minibuffer-exit-hook 'minibuffer-try-remove-hooks)

;;;;;;;;;;;;;;;
;; Kill ring ;;
;;;;;;;;;;;;;;;
(defun kill-ring-insert ()
  (interactive)
  (let ((to_insert (completing-read "Yank: "
                                    (cl-delete-duplicates kill-ring :test #'equal)
                                    nil t)))
    (when (and to_insert (region-active-p))
      ;; the currently highlighted section is to be replaced by the yank
      (delete-region (region-beginning) (region-end)))
    (insert to_insert)))

;;;;;;;;;;;;
;; Cycles ;;
;;;;;;;;;;;;
(require 'rotate-text)
(require 'string-inflection)

(defun rotate-or-inflection (arg)
  (interactive (list (if (consp current-prefix-arg)
                         -1
                       (prefix-numeric-value current-prefix-arg))))
  (condition-case nil
      (rotate-text arg)
    (error (string-inflection-all-cycle))))

;;;;;;;;;;
;; Sexp ;;
;;;;;;;;;;
(defun sp-or-forward-sexp (&optional arg)
  (interactive "^p")
  (if (fboundp 'sp-forward-sexp)
      (sp-forward-sexp arg)
    (forward-sexp arg)))

(defun sp-or-backward-sexp (&optional arg)
  (interactive "^p")
  (if (fboundp 'sp-backward-sexp)
      (sp-backward-sexp arg)
    (backward-sexp arg)))

(defun sp-or-backward-kill-sexp (&optional arg)
  (interactive "^p")
  (if (fboundp 'sp-backward-sexp)
      (let ((opoint (point)))
        (sp-backward-sexp arg)
        (kill-region opoint (point)))
    (backward-kill-sexp arg)))

;;;;;;;;;;;;;;;;;;;;;
;; Keyboard macros ;;
;;;;;;;;;;;;;;;;;;;;;
(defun select-kbd-macro ()
  (interactive)
  (unless (kmacro-ring-empty-p)
    (let* ((ring-alist (mapcar (lambda (ring-item)
                                 (cons (format-kbd-macro (car ring-item))
                                       (car ring-item)))
                               kmacro-ring))
           (kbd-macro (cdr (assoc (completing-read
                                   "Select kbd macro: "
                                   ring-alist nil t nil nil
                                   (format-kbd-macro last-kbd-macro)) ring-alist))))
      (when kbd-macro
        (cl-delete-if (lambda (ring-item) (equal kbd-macro (car ring-item))) kmacro-ring)
        (kmacro-push-ring)
        (setq last-kbd-macro kbd-macro)))))

;;;;;;;;;;
;; Keys ;;
;;;;;;;;;;
;; (global-set-key (kbd "M-y") #'kill-ring-insert)
(global-set-key (kbd "<f1> E") #'manual-entry)
(global-set-key (kbd "C-M-º") #'indent-region)
(global-set-key (kbd "M-s º") #'indent-region)
(global-set-key (kbd "C-x <C-tab>") #'align-regexp)
;; (global-set-key (kbd "ŧ") #'rotate-text)                            ;; AltGr-t
(define-key prog-mode-map (kbd "C-c C-f") #'rotate-text)
(define-key prog-mode-map (kbd "C-c C-b") #'rotate-text-backward)
(global-set-key (kbd "ħ") #'pulse-momentary-highlight-current-line) ;; AltGr-h
;; (global-set-key (kbd "→") #'string-inflection-all-cycle)            ;; AltGr-i
(define-key prog-mode-map (kbd "C-c C-u") #'string-inflection-all-cycle)
(global-set-key (kbd "½") #'query-replace-regexp)                   ;; AltGr-5
(global-set-key (kbd "ð") #'kill-sexp)                              ;; AltGr-d
(global-set-key (kbd "ĸ") #'kill-whole-line)                        ;; AltGr-k
(global-set-key (kbd "“") #'scroll-other-window)                    ;; AltGr-v
(global-set-key (kbd "”") 'sp-or-backward-sexp)                     ;; AltGr-b
(global-set-key (kbd "đ") 'sp-or-forward-sexp)                      ;; AltGr-f
(global-set-key (kbd "€") 'end-of-defun)                            ;; AltGr-e
(global-set-key (kbd "æ") 'beginning-of-defun)                      ;; AltGr-a
(global-set-key (kbd "<f7> d") #'toggle-debug-on-error)
(global-set-key (kbd "<f7> b") #'toggle-enable-multibyte-characters)
(global-set-key (kbd "<f7> c") #'toggle-buffer-coding-system)
(global-set-key (kbd "<f7> i") #'toggle-case-fold-search)
(global-set-key (kbd "<f7> l") #'whitespace-toggle-lines-tail)
(global-set-key (kbd "<f7> RET") #'whitespace-toggle-marks)
(global-set-key (kbd "M-s k w") #'backward-kill-word)
(global-set-key (kbd "M-n") #'forward-paragraph)
(global-set-key (kbd "M-p") #'backward-paragraph)
(global-set-key (kbd "C-<left>") #'left-word)
(global-set-key (kbd "C-<right>") #'right-word)
(global-set-key (kbd "<S-delete>") #'kill-sexp)
(global-set-key (kbd "M-s s") #'swap-regions)
(global-set-key (kbd "<C-M-backspace>") #'sp-or-backward-kill-sexp)
;; (global-set-key (kbd "M-s DEL") #'backward-kill-sexp)
;; (global-set-key (kbd "S-<delete>") #'kill-sexp)
(global-set-key [?\C-x ?u] #'undo-propose)
(global-set-key (kbd "M-s <deletechar>") #'kill-sexp)
(global-set-key (kbd "C-*") #'duplicate-current-line-or-region)
(global-set-key (kbd "M-s *") #'duplicate-current-line-or-region)
(global-set-key (kbd "M-s SPC") #'set-mark-command)
(global-set-key (kbd "M-SPC") #'fixup-whitespace)
(global-set-key (kbd "M-s a SPC") #'fixup-whitespace)
(global-set-key (kbd "C-S-<backspace>") #'kill-whole-line)
(global-set-key (kbd "M-s k l") #'kill-whole-line)
(global-set-key (kbd "M-s <insertchar>") #'kill-whole-line)
(global-set-key (kbd "<M-dead-circumflex>") #'delete-indentation)
(global-set-key (kbd "S-<next>") #'scroll-other-window)
(global-set-key (kbd "S-<prior>") #'scroll-other-window-down)
(global-set-key (kbd "C-<next>") #'hscroll-right)
(global-set-key (kbd "M-s <next>") #'hscroll-right)
(global-set-key (kbd "C-x >") #'hscroll-right)
(global-set-key (kbd "C-<prior>") #'hscroll-left)
(global-set-key (kbd "M-s <prior>") #'hscroll-left)
(global-set-key (kbd "C-x <") #'hscroll-left)
(global-set-key (kbd "M-<right>") #'forward-alt)
(global-set-key (kbd "M-<left>") #'backward-alt)
(global-set-key (kbd "C-ñ") 'find-next-unsafe-char)
(global-set-key (kbd "C-x C-k C-i") 'select-kbd-macro)
(global-set-key (kbd "C-x M-l") 'recenter-horizontal)
;; Case
(global-set-key (kbd "M-c") #'capitalize-dwim)
(global-set-key (kbd "M-l") #'downcase-dwim)
(global-set-key (kbd "M-u") #'upcase-dwim)
;; thingatpt+
(global-set-key (kbd "M-s m") #'mark-things)
(global-set-key (kbd "M-s c") #'cycle-thing-region)
(global-set-key (kbd "M-s l") #'mark-enclosing-list)
(global-set-key (kbd "M-s f") #'mark-enclosing-list-forward)
(global-set-key (kbd "M-s b") #'mark-enclosing-list-backward)
(global-set-key (kbd "M-s n") #'next-visible-thing-repeat)
(global-set-key (kbd "M-s p") #'previous-visible-thing-repeat)
(global-set-key (kbd "C-c l") #'display-line-numbers-mode)

(global-set-key (kbd "M-g f") #'find-dired)

(define-key minibuffer-local-map (kbd "C-c C-l") 'helm-minibuffer-history)

;; Usa el clipboard del sistema
;; (global-set-key [(shift delete)] 'clipboard-kill-region)
;; (global-set-key [(control insert)] 'clipboard-kill-ring-save)
;; (global-set-key [(shift insert)] 'clipboard-yank)


(provide 'typing-config)
;;; typing-config.el ends here
