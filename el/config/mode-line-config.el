;;; mode-line-config.el --- Configure mode-line

;;; Commentary:

;; utf8 symbols for modes
;; - abbrev-mode      "ⓐ"
;; - company-mode     "Ⓒ"
;; - flyspell-mode    "ⓕ"
;; - helm-mode        "Ⓗ"
;; - helm-cscope-mode "ⓢ"
;; - helm-gtags-mode  "ⓣ"
;; - yas-minor-mode   "ⓨ"
;; - undo-tree-mode   "ⓤ"

;;; Code:

(require 'mini-modeline)
(setcar (cdr (assq 'mini-modeline-mode minor-mode-alist)) "")

;;;;;;;;;;;
;; Faces ;;
;;;;;;;;;;;
(defface mode-line-correct
  '((t :foreground "green4" :inherit (mode-line)))
  "Correct" :group 'mode-line)
(defface mode-line-notready
  '((t :foreground "brown4" :inherit (mode-line)))
  "Correct" :group 'mode-line)
(defface mode-line-warning
  '((t :foreground "yellow4" :inherit (mode-line)))
  "Warning" :group 'mode-line)
(defface mode-line-error
  '((t :foreground "red4" :inherit (mode-line)))
  "Error" :group 'mode-line)

(defface mode-line-outside-modified
  '((t :inherit (mode-line) :foreground "#ffffff" :background "#c82829"))
  "Outside modified" :group 'mode-line)

(defface mode-line-modified
  '((t :inherit (mode-line) :foreground "Red" :weight bold))
  "Modified" :group 'mode-line)

(defface mode-line-read-only
  '((t :inherit (mode-line) :foreground "Yellow"))
  "Read only" :group 'mode-line)

(defface mode-line-not-modified
  '((t :inherit (mode-line) :foreground "Green"))
  "Not modified" :group 'mode-line)

(defface mode-line-coding-system
  '((t :inherit (mode-line) :foreground "SaddleBrown"))
  "Coding system" :group 'mode-line)

(defface mode-line-eol
  '((t :inherit (mode-line) :foreground "DarkOliveGreen"))
  "End of line" :group 'mode-line)

(defface mode-line-column
  '((t :inherit (mode-line) :foreground "White" :weight bold))
  "Column number" :group 'mode-line)

(defface mode-line-percentage
  '((t :inherit (mode-line) :foreground "DodgerBlue"))
  "Percentage" :group 'mode-line)

(defface mode-line-battery
  '((t :inherit (mode-line) :foreground "Blue"))
  "Column number" :group 'mode-line)

(defface mode-line-major-mode
  '((t :inherit (mode-line) :foreground "DarkBlue" :weight ultra-bold))
  "Major mode" :group 'mode-line)

(defface mode-line-project-name
  '((t :inherit (mode-line) :foreground "SaddleBrown" :weight bold))
  "Project name" :group 'mode-line)

(defface mode-line-abbrev-mode
  '((t :inherit (mode-line) :foreground "light slate blue" :weight bold))
  "Project name" :group 'mode-line)

;;;;;;;;;;;;;;;;;;;;;;
;; Helpers packages ;;
;;;;;;;;;;;;;;;;;;;;;;
(when (load "cyphejor" t)
  (setq
   cyphejor-rules
   '(:upcase
     ("fundamental" "∅")
     ("bookmark"    "→")
     ("buffer"      "β")
     ("diff"        "Δ")
     ("dired"       "δ")
     ("emacs"       "ε")
     ("help"        "?")
     ("csv"         ",")
     ("tsv"         "↹")
     ("inferior"    "i" :prefix)
     ("interaction" "i" :prefix)
     ("interactive" "i" :prefix)
     ("lisp"        "λ" :postfix)
     ("menu"        "▤" :postfix)
     ("mode"        "")
     ("package"     "↓")
     ("python"      "π")
     ("c"           "ȼ")
     ("org"         "Ω")
     ("eshell"      "ε∫" :postfix)
     ("shell"       "∫" :postfix)
     ("text"        "ξ")
     ("web"         "ω")
     ("wdired"      "↯δ")
     ("fish"        "φ")
     ("nim"         "ℵ")))
  (cyphejor-mode))
(with-eval-after-load 'abbrev
  (setcar (cdr (assq 'abbrev-mode minor-mode-alist)) (propertize "A"
                                                                 'face
                                                                 'mode-line-abbrev-mode)))
(with-eval-after-load 'compile
  (setcar (cdr (assq 'compilation-shell-minor-mode minor-mode-alist)) "Cs")
  (setcar (cdr (assq 'compilation-minor-mode minor-mode-alist)) "Cp"))
(with-eval-after-load 'autorevert
  (setq auto-revert-mode-text "Ar"))

;;;;;;;;;;;;;;;;;;;;;;
;; Sort minor modes ;;
;;;;;;;;;;;;;;;;;;;;;;
(defun mode-line-sort-minors ()
  (interactive)
  (dolist (minor '(abbrev-mode yas-minor-mode company-mode caps-lock-show-mode))
    (let ((pos (cl-position-if (lambda (x) (eq minor (car x))) minor-mode-alist)))
      (when pos
       (setcdr (last minor-mode-alist) (list (elt minor-mode-alist pos)))
       (setq minor-mode-alist
             (remove-nth-element pos minor-mode-alist))))))
(dolist (package '("abbrev" "yasnippet" "company"))
  (with-eval-after-load package
    (mode-line-sort-minors)))

;;;;;;;;;;;;;;;;;;;;;;;
;; Battery mode line ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; (require 'battery)
;; (bug-check-function-bytecode
;;  'battery-update
;;  "CIUGAAggGcbHCZ5BIUAayAuDJgAKp4MmAAoMWIMmAMkLCSKCJwDKywqnhTQACg1YhTQAzM3OJRYQKs8ghw==")
;; (eval-and-when-daemon frame
;;   (display-battery-mode)
;;   (setq battery-mode-line-format "%p%L")
;;   (defun battery-update ()
;;     "Update battery status information in the mode line."
;;     (let ((data (and battery-status-function (funcall battery-status-function))))
;;       (let ((percentage (car (read-from-string (cdr (assq ?p data)))))
;;             (supplier (cdr (assq ?L data)))
;;             percentage-str
;;             percentage-face)
;;         (if (numberp percentage)
;;             (setq percentage-str (int-to-string (truncate percentage))
;;                   percentage-face (if (<= percentage battery-load-critical)
;;                                       '(:foreground "red")
;;                                     `(:foreground ,(format "#%02i%02i00"
;;                                                            (- 100 percentage)
;;                                                            (- percentage 1)))))
;;           (setq percentage-str percentage
;;                 percentage-face '(:foreground "yellow")))
;;         (setq battery-mode-line-string
;;               (propertize (concat
;;                            percentage-str
;;                            (cond ((string-equal supplier "AC")
;;                                   (if (display-graphic-p) "🔌" ":"))
;;                                  ((string-equal supplier "BAT")
;;                                   (if (display-graphic-p) "🔋" "!"))
;;                                  ((string-equal supplier "N/A")
;;                                   "?")
;;                                  (t supplier)))
;;                           'font-lock-face
;;                           percentage-face
;;                           'help-echo "Battery status information"))))
;;     (force-mode-line-update)))

;;;;;;;;;;;;;;;;;;;;;;
;; Define mode line ;;
;;;;;;;;;;;;;;;;;;;;;;
(setq eol-mnemonic-unix "LF"
      eol-mnemonic-dos "CRLF"
      eol-mnemonic-mac "CR")

(setq-default
 ;; #     #
 ;; ##   ## #    # #      ######
 ;; # # # # #    # #      #
 ;; #  #  # #    # #      #####
 ;; #     # #    # #      #
 ;; #     # #    # #      #
 ;; #     #  ####  ###### ######
 mode-line-mule-info
 `(""
   (current-input-method
    (:propertize ("" current-input-method-title)
                 help-echo (concat
                            ,(purecopy "Current input method: ")
                            current-input-method
                            ,(purecopy "\n\
mouse-2: Disable input method\n\
mouse-3: Describe current input method"))
                 local-map ,mode-line-input-method-map
                 mouse-face mode-line-highlight))
   ,(propertize
     "%z"
     'face 'mode-line-coding-system
     'help-echo 'mode-line-mule-info-help-echo
     'mouse-face 'mode-line-highlight
     'local-map mode-line-coding-system-map)
   (:propertize
    (:eval (mode-line-eol-desc))
    face mode-line-eol))
 ;; #     #
 ;; ##   ##  ####  #####  # ###### # ###### #####
 ;; # # # # #    # #    # # #      # #      #    #
 ;; #  #  # #    # #    # # #####  # #####  #    #
 ;; #     # #    # #    # # #      # #      #    #
 ;; #     # #    # #    # # #      # #      #    #
 ;; #     #  ####  #####  # #      # ###### #####
 mode-line-modified
 '(:eval
   (cond
    ((not (or (and (buffer-file-name) (file-remote-p buffer-file-name))
              (verify-visited-file-modtime (current-buffer))))
     (propertize "M" 'face 'mode-line-outside-modified
                 'help-echo "Modified outside Emacs!\nRevert first!"))
    ((buffer-modified-p)
     (propertize (if buffer-read-only
                     "R"
                   "×")
                 'face 'mode-line-modified
                 'help-echo (if (and (buffer-file-name) (not (file-remote-p buffer-file-name)))
                                (format-time-string
                                 "Modified on %T %Y-%m-%d."
                                 (nth 5 (file-attributes (buffer-file-name))))
                              "Buffer Modified")
                 'local-map '(keymap (mode-line keymap (mouse-1 . save-buffer)))))
    (buffer-read-only (propertize "R"
                                  'face 'mode-line-read-only
                                  'help-echo "Read-Only Buffer"))
    (t ""
       ;; (propertize "-" 'face 'mode-line-not-modified)
       )))
 ;; ######
 ;; #     #  ####   ####  # ##### #  ####  #    #
 ;; #     # #    # #      #   #   # #    # ##   #
 ;; ######  #    #  ####  #   #   # #    # # #  #
 ;; #       #    #      # #   #   # #    # #  # #
 ;; #       #    # #    # #   #   # #    # #   ##
 ;; #        ####   ####  #   #   #  ####  #    #
 mode-line-position
 `((:eval
    (if (memq display-line-numbers '(relative visual))
        ":%c "
      ,(concat (propertize "%l" 'face 'mode-line-column)
               ":%c "))
     'local-map mode-line-column-line-number-mode-map
     'mouse-face 'mode-line-highlight
     'help-echo "Line number and Column number\n\
mouse-1: Display Line and Column Mode Menu")
   (:propertize
    mode-line-percent-position
    face mode-line-percentage
    local-map ,mode-line-column-line-number-mode-map
    mouse-face mode-line-highlight
    ;; XXX needs better description
    help-echo "Size indication mode\n\
mouse-1: Display Line and Column Mode Menu")
   (size-indication-mode
    ,(propertize
        " %I"
        'local-map mode-line-column-line-number-mode-map
        'mouse-face 'mode-line-highlight
        ;; XXX needs better description
        'help-echo "Size indication mode\n\
mouse-1: Display Line and Column Mode Menu")))
 ;; #     #
 ;; ##   ##  ####  #####  ######  ####
 ;; # # # # #    # #    # #      #
 ;; #  #  # #    # #    # #####   ####
 ;; #     # #    # #    # #           #
 ;; #     # #    # #    # #      #    #
 ;; #     #  ####  #####  ######  ####
 mode-line-modes
 (let ((recursive-edit-help-echo "Recursive edit, type C-M-c to get out"))
   (list (propertize "%[" 'help-echo recursive-edit-help-echo)
         " "
         `(:propertize ("" mode-name)
                       face mode-line-major-mode
                       help-echo "Major mode\n\
mouse-1: Display major mode menu\n\
mouse-2: Show help for major mode\n\
mouse-3: Toggle minor modes"
                       mouse-face mode-line-highlight
                       local-map ,mode-line-major-mode-keymap)
         '("" mode-line-process)
         `(:propertize ("" minor-mode-alist)
                       mouse-face mode-line-highlight
                       help-echo "Minor mode\n\
mouse-1: Display minor mode menu\n\
mouse-2: Show help for minor mode\n\
mouse-3: Toggle minor modes"
                       local-map ,mode-line-minor-mode-keymap)
         (propertize "%n" 'help-echo "mouse-2: Remove narrowing from buffer"
                     'mouse-face 'mode-line-highlight
                     'local-map (make-mode-line-mouse-map
                                 'mouse-2 #'mode-line-widen))
         (propertize "%]" 'help-echo recursive-edit-help-echo)
         " "))
 ;;  ####  #####   ####  #    # #####   ####
 ;; #    # #    # #    # #    # #    # #
 ;; #      #    # #    # #    # #    #  ####
 ;; #  ### #####  #    # #    # #####       #
 ;; #    # #   #  #    # #    # #      #    #
 ;;  ####  #    #  ####   ####  #       ####
 ;; Pre Filename
 mode-line-pre-filename
 '("%e"
   mode-line-position
   ;; mode-line-front-space  ;; display-graphic-p
   mode-line-mule-info
   mode-line-client
   mode-line-modified
   mode-line-remote
   ;; mode-line-frame-identification
   )
 ;; Post Filename
 mode-line-post-filename
 '("%e"
   mode-line-modes
   mode-line-misc-info
   ;; mode-line-end-spaces
   )
 ;; #     #                         #
 ;; ##   ##  ####  #####  ######    #       # #    # ######
 ;; # # # # #    # #    # #         #       # ##   # #
 ;; #  #  # #    # #    # #####     #       # # #  # #####
 ;; #     # #    # #    # #         #       # #  # # #
 ;; #     # #    # #    # #         #       # #   ## #
 ;; #     #  ####  #####  ######    ####### # #    # ######
 mode-line-vc nil
 ;; mode-line-format  ;; -  without mini-modeline
 mini-modeline-r-format  ;; +  with mimi-modeline
 `("%e"
   ,@(cdr mode-line-pre-filename)
   ;; (:eval (mode-line-buffer-identification-shorten))  ;; - without mini-modeline
   (:eval (mini-modeline-buffer-identification-shorten))  ;; + with mini-modeline
   (:eval mode-line-vc)
   ,@(cdr mode-line-post-filename)))

;; [ mini modeline options
(setq mini-modeline-truncate-p nil
      mini-modeline-echo-duration 5)
(mini-modeline-mode t)  ;; + with mini-modeline
;; ]

(defun mode-line-abbreviate-file-name ()
  (let ((name (buffer-file-name)))
    (if name
        (abbreviate-file-name name))))

(defun abbrev-string-try (string len abbrev-len)
  (let ((old "")
        (regexp (concat "\\(\\([A-Za-z]\\{"
                        (int-to-string abbrev-len)
                        "\\}\\)[A-Za-z]+\\).*\\'")))
    (while (and (< len (length string))
                (not (string-equal string old)))
      (setq old string
            string (replace-regexp-in-string regexp "\\2" string nil nil 1))))
  string)

(defun shorten-path (path len abbrev-len)
  (let ((path-old "")
        (filename (file-name-nondirectory path))
        (regexp (concat "\\(\\([A-Za-z]\\{"
                        (int-to-string abbrev-len)
                        "\\}\\)[A-Za-z]+\\).*\\'")))
    (while (and (< len (length path))
                (not (string-equal path path-old))
                (string-equal filename (file-name-nondirectory path)))
      (setq path-old path
            path (replace-regexp-in-string regexp "\\2" path nil nil 1))))
  (if (< len (length path))
      (let ((len/2 (max 1 (/ len 2))))
        (concat
         (substring path 0 (- len/2 1))
         "…"
         (substring path (- len/2))))
    path))

(defvar mode-line-path-abbrev-len 2)
(defvar mode-line-vc-abbrev-len 2)

(defun mini-modeline-buffer-identification-shorten ()
  (setq mode-line-vc (or vc-mode ""))
  (let ((total-len (frame-total-cols))
        (pre-len (string-width (format-mode-line mode-line-pre-filename)))
        (post-len (string-width (format-mode-line mode-line-post-filename)))
        (vc-len (string-width mode-line-vc))
        (file-path (or (mode-line-abbreviate-file-name) (buffer-name))))
    (let* ((len (- total-len pre-len vc-len post-len))
           (final-file-path
            (if (<= (string-width file-path) len)
                file-path
              (setq file-path (let ((file-name (replace-regexp-in-string "\\`.*/" "" file-path)))
                                (concat (abbrev-string-try (replace-regexp-in-string "[^/]*\\'" "" file-path)
                                                           (- len (string-width file-name))
                                                           mode-line-path-abbrev-len)
                                        file-name)))
              (let ((file-path-len (string-width file-path)))
                (if (<= file-path-len len)
                    file-path
                  (if (< 0 vc-len)
                      (setq vc-len (- vc-len (- file-path-len len))
                            mode-line-vc (abbrev-string-try mode-line-vc
                                                            vc-len
                                                            mode-line-vc-abbrev-len)
                            len (+ vc-len (- file-path-len (string-width mode-line-vc)))))
                  (if (<= file-path-len len)
                      file-path
                    (setq file-path (let ((path (replace-regexp-in-string "[^/]*\\'" "" file-path)))
                                      (concat path
                                              (abbrev-string-try (replace-regexp-in-string "\\`.*/" "" file-path)
                                                                 (- len (string-width path))
                                                                 mode-line-path-abbrev-len))))
                    (if (<= (string-width file-path) len)
                        file-path
                      (let ((len/2 (max 1 (/ len 2))))
                        (concat
                         (substring file-path 0 (- len/2 1))
                         "…"
                         (substring file-path (- len/2)))))))))))
      final-file-path)))

(defun mode-line-buffer-identification-shorten ()
  (setq mode-line-vc (or vc-mode ""))
  (let ((total-len (window-total-width))
        (pre-len (string-width (format-mode-line mode-line-pre-filename)))
        (post-len (string-width (format-mode-line mode-line-post-filename)))
        (vc-len (string-width mode-line-vc))
        (file-path (or (mode-line-abbreviate-file-name) (buffer-name))))
    (let* ((len (- total-len pre-len vc-len post-len))
           (final-file-path
            (if (<= (string-width file-path) len)
                file-path
              (setq file-path (let ((file-name (replace-regexp-in-string "\\`.*/" "" file-path)))
                                (concat (abbrev-string-try (replace-regexp-in-string "[^/]*\\'" "" file-path)
                                                           (- len (string-width file-name))
                                                           mode-line-path-abbrev-len)
                                        file-name)))
              (let ((file-path-len (string-width file-path)))
                (if (<= file-path-len len)
                    file-path
                  (if (< 0 vc-len)
                      (setq vc-len (- vc-len (- file-path-len len))
                            mode-line-vc (abbrev-string-try mode-line-vc
                                                            vc-len
                                                            mode-line-vc-abbrev-len)
                            len (+ vc-len (- file-path-len (string-width mode-line-vc)))))
                  (if (<= file-path-len len)
                      file-path
                    (setq file-path (let ((path (replace-regexp-in-string "[^/]*\\'" "" file-path)))
                                      (concat path
                                              (abbrev-string-try (replace-regexp-in-string "\\`.*/" "" file-path)
                                                                 (- len (string-width path))
                                                                 mode-line-path-abbrev-len))))
                    (if (<= (string-width file-path) len)
                        file-path
                      (let ((len/2 (max 1 (/ len 2))))
                        (concat
                         (substring file-path 0 (- len/2 1))
                         "…"
                         (substring file-path (- len/2)))))))))))
      (format (concat "%-" (int-to-string len) "s") final-file-path))))

;;;;;;;;;;;;;;;;
;; Projectile ;;
;;;;;;;;;;;;;;;;
(with-eval-after-load 'projectile
  (defun projectile-mode-menu (event)
    (interactive "@e")
    (let ((minor-mode 'projectile-mode))
      (let* ((map (cdr-safe (assq minor-mode minor-mode-map-alist)))
             (menu (and (keymapp map) (lookup-key map [menu-bar]))))
        (if menu
            (popup-menu (mouse-menu-non-singleton menu))
          (message "No menu available")))))

  (defvar mode-line-projectile-mode-keymap
    (let ((map (make-sparse-keymap)))
      (define-key map [mode-line down-mouse-1] 'projectile-mode-menu)
      (define-key map [mode-line mouse-2] 'mode-line-minor-mode-help)
      (define-key map [mode-line down-mouse-3] mode-line-mode-menu)
      (define-key map [header-line down-mouse-3] mode-line-mode-menu)
      map) "\
Keymap to display projectile options.")

  (defun mode-line-perform-projectile-replacement (in)
    "If path IN is inside a project, use its name as a prefix."
    (let ((proj (projectile-project-p)))
      (if (stringp proj)
          (let* ((replacement (propertize
                               (funcall projectile-mode-line-function)
                               'face 'mode-line-project-name
                               'mouse-face 'mode-line-highlight
                               'help-echo "Minor mode\n\
mouse-1: Display minor mode menu\n\
mouse-2: Show help for minor mode\n\
mouse-3: Toggle minor modes"
                               'local-map mode-line-projectile-mode-keymap))
                 (short (replace-regexp-in-string
                         (concat "^" (regexp-quote (abbreviate-file-name proj)))
                         replacement
                         in)))
            (if (string= short in)
                (let* ((true-in (abbreviate-file-name (file-truename in)))
                       (true-short
                        (replace-regexp-in-string
                         (concat "^" (regexp-quote (abbreviate-file-name (file-truename proj))))
                         replacement true-in)))
                  (if (string= true-in true-short) in true-short))
              short))
        in)))

  (defun mode-line-abbreviate-file-name ()
    (let ((name (buffer-file-name)))
      (if name
          (mode-line-perform-projectile-replacement (abbreviate-file-name name))))))

;;;;;;;;;;;;;;;;;;;;;
;; Version control ;;
;;;;;;;;;;;;;;;;;;;;;
(with-eval-after-load 'vc-hooks
  (defun vc-mode-line-advice (&rest args)
    "Color `vc-mode'."
    (when (stringp vc-mode)
      (let ((noback (replace-regexp-in-string (format "^ %s" (vc-backend buffer-file-name)) " " vc-mode)))
        (setq vc-mode
              (propertize noback
                          'face (cond ((string-match "^ -" noback)    'mode-line-not-modified)
                                      ((string-match "^ [:@]" noback) 'mode-line-read-only)
                                      ((string-match "^ [!\\?]" noback) 'mode-line-modified)))))))
  (advice-add 'vc-mode-line :after 'vc-mode-line-advice))


(provide 'mode-line-config)
;;; mode-line-config.el ends here
