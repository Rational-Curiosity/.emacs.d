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

;; (require 'mini-modeline)                                       ;; + with mini-modeline
;; (setcar (cdr (assq 'mini-modeline-mode minor-mode-alist)) nil)  ;; + with mini-modeline

;;;;;;;;;;;
;; Faces ;;
;;;;;;;;;;;
(defface mode-line-correct
  '((t :foreground "green4"))
  "Correct" :group 'mode-line)
(defface mode-line-notready
  '((t :foreground "brown4"))
  "Correct" :group 'mode-line)
(defface mode-line-warning
  '((t :foreground "yellow4"))
  "Warning" :group 'mode-line)
(defface mode-line-error
  '((t :foreground "red4"))
  "Error" :group 'mode-line)

(defface mode-line-outside-modified
  '((t :foreground "#ffffff" :background "#c82829"))
  "Outside modified" :group 'mode-line)

(defface mode-line-modified
  '((t :foreground "Red" :weight bold))
  "Modified" :group 'mode-line)

(defface mode-line-read-only
  '((t :foreground "Yellow"))
  "Read only" :group 'mode-line)

(defface mode-line-not-modified
  '((t :foreground "Green"))
  "Not modified" :group 'mode-line)

(defface mode-line-coding-system
  '((t :foreground "SaddleBrown"))
  "Coding system" :group 'mode-line)

(defface mode-line-eol
  '((t  :foreground "DarkOliveGreen"))
  "End of line" :group 'mode-line)

(defface mode-line-column
  '((t :foreground "White" :weight bold))
  "Column number" :group 'mode-line)

(defface mode-line-percentage
  '((t :foreground "DodgerBlue"))
  "Percentage" :group 'mode-line)

(defface mode-line-battery
  '((t :foreground "DodgerBlue3"))
  "Column number" :group 'mode-line)

(defface mode-line-major-mode
  '((t :foreground "DarkBlue" :weight ultra-bold))
  "Major mode" :group 'mode-line)

(defface mode-line-project-name
  '((t :foreground "SaddleBrown" :weight bold))
  "Project name" :group 'mode-line)

(defface mode-line-abbrev-mode
  '((t :foreground "light slate blue" :weight bold))
  "Project name" :group 'mode-line)

;;;;;;;;;;;;;;;;;;;;;;
;; Helpers packages ;;
;;;;;;;;;;;;;;;;;;;;;;
(when (load "cyphejor" t)
  (setq cyphejor-rules
        '(:upcase
          ("bookmark"    "→")
          ("buffer"      "β")
          ("c"           "ȼ")
          ("csv"         ",")
          ("diff"        "Δ")
          ("dired"       "δ")
          ("elfeed"      "📰")
          ("emacs"       "ε")
          ("emms"        "♪")
          ("eshell"      "ε∫" :postfix)
          ("exwm"        "χ")
          ("fish"        "φ")
          ("fundamental" "∅")
          ("help"        "?")
          ("inferior"    "i" :prefix)
          ("interaction" "i" :prefix)
          ("interactive" "i" :prefix)
          ("lisp"        "λ" :postfix)
          ("menu"        "▤" :postfix)
          ("mode"        "")
          ("nim"         "ℵ")
          ("org"         "Ω")
          ("package"     "↓")
          ("python"      "π")
          ("rust"        "⚙")
          ("search"      "🔍")
          ("sh"          "$")
          ("shell"       "∫" :postfix)
          ("show"        "✓")
          ("text"        "ξ")
          ("tsv"         "↹")
          ("wdired"      "↯δ")
          ("web"         "ω")
          ("yaml"        "Ⲩ")
          ))
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

(defvar-local mode-line-cached nil)
(defvar-local mode-line-identification nil)

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
 ;; #     #                         #
 ;; ##   ##  ####  #####  ######    #       # #    # ######
 ;; # # # # #    # #    # #         #       # ##   # #
 ;; #  #  # #    # #    # #####     #       # # #  # #####
 ;; #     # #    # #    # #         #       # #  # # #
 ;; #     # #    # #    # #         #       # #   ## #
 ;; #     #  ####  #####  ######    ####### # #    # ######
 mode-line-format  ;; -  without mini-modeline
 ;; mini-modeline-r-format  ;; +  with mimi-modeline
 `("%e"
   mode-line-position
   ;; mode-line-front-space  ;; display-graphic-p
   mode-line-mule-info
   mode-line-client
   mode-line-modified
   mode-line-remote
   ;; mode-line-frame-identification
   (:eval (mode-line-buffer-identification-shorten))  ;; - without mini-modeline
   ;; (:eval (mini-modeline-buffer-identification-shorten))  ;; + with mini-modeline
   mode-line-modes
   mode-line-misc-info
   ;; mode-line-end-spaces
   ))

;; [ mini-modeline options
;; (setq mini-modeline-truncate-p nil
;;       mini-modeline-echo-duration 5)
;; (mini-modeline-mode t)  ;; + with mini-modeline
;; ]

(defun mode-line-abbreviate-file-name ()
  (let ((name (buffer-file-name)))
    (if name
        (abbreviate-file-name name))))

(defun abbrev-string-try (len string)
  (let ((old ""))
    (while (and (< len (string-width string))
                (not (string-equal string old)))
      (setq old string
            string (replace-regexp-in-string
                    "\\(\\([A-Za-z]\\{3\\}\\)[A-Za-z]+\\).*\\'"
                    "\\2"
                    string t nil 1))))
  string)

(defvar mode-line-filename-replacements
  `(("test"     . ,(propertize "T" 'face 'hi-red-b))
    ("config"   . ,(propertize "C" 'face 'hi-red-b))
    ("class"    . ,(propertize "C" 'face 'hi-green-b))
    ("object"   . ,(propertize "O" 'face 'hi-green-b))
    ("api"      . ,(propertize "A" 'face 'hi-green-b))
    ("util"     . ,(propertize "U" 'face 'hi-green-b))
    ("bug"      . ,(propertize "B" 'face 'hi-green-b))
    ("library"  . ,(propertize "L" 'face 'hi-green-b))
    ("librarie" . ,(propertize "L" 'face 'hi-green-b))
    ("invoice"  . ,(propertize "I" 'face 'hi-red-b))
    ("resource" . ,(propertize "R" 'face 'hi-red-b)))
  "Mode line file name replacements")

(defun abbrev-strings-try (len string &rest strings)
  (let ((i -1)
        (len-list (length strings))
        (len-short (- len (string-width string)))
        len-strings)
    (while (and (< (cl-incf i) len-list)
                (< len-short (setq len-strings (apply '+ (mapcar 'string-width strings)))))
      (let ((len-i (- (string-width (nth i strings)) (- len-strings len-short)))
            (old ""))
        (while (and (< len-i (string-width (nth i strings)))
                    (not (string-equal (nth i strings) old)))
          (setq old (nth i strings))
          (let ((istring (nth i strings)))
            (if (string-match "\\(\\([A-Za-z]\\{2\\}\\)[A-Za-z]+\\).*\\'"
                              istring)
                (setcar (nthcdr i strings)
                        (concat
                         (substring istring 0 (match-beginning 1))
                         (let ((isubtext (substring istring
                                                    (match-beginning 2)
                                                    (match-end 2))))
                         (propertize
                          isubtext
                          'face
                          `(:weight bold :slant italic
                                    :inherit ,(get-text-property 0 'face isubtext))
                          'help-echo (substring istring
                                                (match-beginning 1)
                                                (match-end 1))))
                         (substring istring (match-end 1)))))))))
    (if (< 0 (setq len-strings (- len (apply '+ (mapcar 'string-width strings)))))
        (let ((len-list (length mode-line-filename-replacements))
              (i -1))
          (while (and (< len-strings (string-width string))
                      (< i len-list))
            (let ((replacement (nth i mode-line-filename-replacements)))
              (let ((pos (string-match-p (car replacement) string)))
                (if pos
                    (setq string
                          (concat
                           (substring string 0 pos)
                           (cdr replacement)
                           (substring string (+ pos (length (car replacement))))))
                  (cl-incf i))))))))
  `(,string ,@strings))

(defun abbrev-string (len string)
  (if (< len (string-width string))
      (let ((len/2 (max 1 (/ len 2))))
        (concat
         (substring string 0 (- len/2 1))
         (propertize "…"
                     'face 'error
                     'help-echo (buffer-file-name))
         (substring string (- len/2))))
    string))

(defvar mode-line-mock nil)
(defun mode-line-buffer-identification-shorten ()
  (if mode-line-mock ""
    (let ((total-len (window-total-width))
          (final-name (or (mode-line-abbreviate-file-name) (buffer-name)))
          (vc-name (or vc-mode ""))
          (others-len (string-width (let ((mode-line-mock t))
                                     (format-mode-line mode-line-format)))))
      (if (equal mode-line-cached (list total-len others-len vc-name final-name))
          mode-line-identification
        (prog1
            (let ((len (- total-len others-len)))
              (if (< len (+ (string-width final-name) (string-width vc-name)))
                  (if (string-match "\\`\\(.*/\\)\\([^/]*\\)\\'" final-name)
                      (let ((dir-name (match-string 1 final-name))
                            (base-name (match-string 2 final-name)))
                        (if vc-mode
                            (let ((result (abbrev-strings-try len base-name dir-name vc-name)))
                              (setq vc-name (car (cdr (cdr result)))
                                    len (- len (string-width vc-name))
                                    final-name (abbrev-string len (concat (car (cdr result)) (car result)))))
                          (let ((result (abbrev-strings-try len base-name dir-name)))
                            (setq final-name (abbrev-string len (concat (car (cdr result)) (car result)))))))
                    (if vc-mode
                        (let ((result (abbrev-strings-try len "" final-name vc-name)))
                          (setq vc-name (car (cdr (cdr result)))
                                len (- len (string-width vc-name))
                                final-name (abbrev-string len (car (cdr result)))))
                      (setq final-name (abbrev-string len (abbrev-string-try len final-name)))))
                (setq len (- len (string-width vc-name))))
              (setq mode-line-identification (concat
                                              (format (concat "%-" (int-to-string (max len 0)) "s") final-name)
                                              vc-name)))
          (setq mode-line-cached (list total-len others-len vc-name final-name)))))))

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
                         in t t)))
            (if (string= short in)
                (let* ((true-in (abbreviate-file-name (file-truename in)))
                       (true-short
                        (replace-regexp-in-string
                         (concat "^" (regexp-quote (abbreviate-file-name (file-truename proj))))
                         replacement true-in t t)))
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
  (defun vc-mode-line-advice (file &optional backend)
    "Colorize and abbrev `vc-mode'."
    (when (stringp vc-mode)
      (let ((noback (replace-regexp-in-string
                     (concat "^ " (regexp-quote (symbol-name backend)))
                     " " vc-mode t t)))
        (setq vc-mode
              (propertize noback
                          'face (cl-case (elt noback 1)
                                  (?- 'mode-line-not-modified)
                                  ((?: ?@) 'mode-line-read-only)
                                  ((?! ?\\ ??) 'mode-line-modified)))))))
  (advice-add 'vc-mode-line :after 'vc-mode-line-advice))


(provide 'mode-line-config)
;;; mode-line-config.el ends here
