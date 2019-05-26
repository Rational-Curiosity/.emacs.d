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


;;;;;;;;;;;
;; Faces ;;
;;;;;;;;;;;
(defface mode-line-correct
  '((t :foreground "brown4" :inherit (mode-line)))
  "Correct" :group 'mode-line)
(defface mode-line-notready
  '((t :foreground "green4" :inherit (mode-line)))
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
     ("eshell"       "εsh" :postfix)
     ("shell"       "sh" :postfix)
     ("text"        "ξ")
     ("wdired"      "↯δ")
     ("fish"        "φ")
     ("nim"         "ℵ")))
  (cyphejor-mode))

(setcar (cdr (assq 'abbrev-mode minor-mode-alist)) "A")
(setq auto-revert-mode-text "Ar")

;;;;;;;;;;;;;;;;;;;;;;
;; Sort minor modes ;;
;;;;;;;;;;;;;;;;;;;;;;
(defun mode-line-sort-minors ()
  (interactive)
  (dolist (minor '(abbrev-mode yas-minor-mode company-mode))
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

(eval-and-when-daemon frame
  (display-battery-mode)
  (bug-check-function-bytecode
   'battery-update
   "CIUGAAggGcbHCZ5BIUAayAuDJgAKp4MmAAoMWIMmAMkLCSKCJwDKywqnhTQACg1YhTQAzM3OJRYQKs8ghw==")
  (setq battery-mode-line-format "%p")
  (defun battery-update ()
    "Update battery status information in the mode line."
    (let* ((data (and battery-status-function (funcall battery-status-function)))
           (percentage (car (read-from-string (cdr (assq ?p data))))))
      (setq battery-mode-line-string
            (propertize (if (and battery-mode-line-format
                                 (numberp percentage)
                                 (<= percentage battery-mode-line-limit))
                            (battery-format battery-mode-line-format data)
                          "")
                        'face
                        (if (and (numberp percentage)
                                 (<= percentage battery-load-critical))
                            'error
                          'mode-line-battery)
                        'help-echo "Battery status information")))
    (force-mode-line-update)))

;;;;;;;;;;;;;;;;;;;;;;
;; Define mode line ;;
;;;;;;;;;;;;;;;;;;;;;;
(setq eol-mnemonic-unix "LF"
      eol-mnemonic-dos "CRLF"
      eol-mnemonic-mac "CR")

(setq-default
 ;;
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
 ;;
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
 ;;
 mode-line-position
 `(,(propertize
     (concat (propertize "%l" 'face 'mode-line-column)
             ":%c ")
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
 ;;
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
   (vc-mode vc-mode)
   mode-line-modes
   mode-line-misc-info
   ;; mode-line-end-spaces
   )
 ;; Mode Line
 mode-line-format
 `("%e"
   ,@(cdr mode-line-pre-filename)
   (:eval (mode-line-buffer-identification-shorten))
   ,@(cdr mode-line-post-filename)))

(defun mode-line-abbreviate-file-name ()
  (let ((name (buffer-file-name)))
    (if name
        (abbreviate-file-name name))))

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

(defun mode-line-buffer-identification-shorten ()
  (let ((pre-filename (format-mode-line mode-line-pre-filename))
        (post-filename (format-mode-line mode-line-post-filename))
        (name (or (mode-line-abbreviate-file-name) (buffer-name))))
    (let* ((len (- (window-total-width)
                   (string-width pre-filename)
                   (string-width post-filename)))
           (len/2 (/ len 2)))
      (format (concat "%-" (int-to-string len) "s")
              (if (<= (string-width name) len)
                  name
                (concat
                 (substring-no-properties name 0 (- len/2 1))
                 "…"
                 (substring-no-properties name (- len/2))))))))


(provide 'mode-line-config)
;;; mode-line-config.el ends here
