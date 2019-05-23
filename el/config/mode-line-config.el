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
     ("shell"       "sh" :postfix)
     ("text"        "ξ")
     ("wdired"      "↯δ")
     ("fish"        "φ")
     ("nim"         "ℵ")))
  (cyphejor-mode))


(require 'rich-minority)

(defface mode-line-correct
  '((t :foreground "green4" :inherit (mode-line)))
  "Warnings" :group 'mode-line)
(defface mode-line-warning
  '((t :foreground "yellow4" :inherit (mode-line)))
  "Warnings" :group 'mode-line)
(defface mode-line-error
  '((t :foreground "red4" :inherit (mode-line)))
  "Warnings" :group 'mode-line)

(add-to-list 'rm-text-properties
             '("✓" 'face 'mode-line-correct))
(add-to-list 'rm-text-properties
             '("⚠" 'face 'mode-line-warning))
(add-to-list 'rm-text-properties
             '("⚐" 'face 'mode-line-error))

(setcar (cdr (assq 'abbrev-mode minor-mode-alist)) "A")
(setq auto-revert-mode-text "Ar"
      rm-blacklist '(" Projectile"))

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
;; [ <examples>
;; example of eval inside mode-line
;; (setq-default projectile-mode-line
;;  '(:eval (format "{%s}" (projectile-project-name))))

;; example of mode-line-format
;; (setq-default mode-line-format
;;               '("%e" mode-line-front-space
;;                 mode-line-mule-info
;;                 mode-line-client
;;                 mode-line-modified
;;                 mode-line-remote
;;                 mode-line-frame-identification
;;                 mode-line-buffer-identification
;;                 " " mode-line-position (vc-mode vc-mode)
;;                 " " mode-line-modes
;;                 mode-line-misc-info
;;                 mode-line-end-spaces))

;; example of variable format
;; (defun variable-name-width (frame)
;;   (set (make-local-variable 'sml/name-width)
;;        (min 55       ; maximum name length
;;             (max 17  ; minimum name length
;;                  (- (window-body-width) 47)))))
;; (add-hook 'window-size-change-functions 'variable-name-width)

;; example wait until make
;; (add-hook 'after-make-frame-functions
;;           (lambda (frame)
;;             (face-remap-add-relative
;;              'mode-line '((:background "dim gray") mode-line))))
;; ] <examples>

;; [ <smart-mode-line>
(require 'smart-mode-line)
(setq line-number-display-limit nil
      line-number-display-limit-width 2000
      sml/no-confirm-load-theme t
      sml/shorten-directory t
      sml/name-width '(17 . 55)
      sml/shorten-modes t
      sml/mode-width 'right
      ;; Ruta completa en la barra de título
      frame-title-format
      '((:eval (if buffer-file-name
                   (sml/replacer  buffer-file-name)
                 "%b"))))

;; [ Cycle mode justification
(require 'ring)
(defvar sml-mode-justification-ring nil)
(let ((justifications '(-27 -24 -21 -18 -15 -12 -9 -6 -3 0)))
  (setq sml-mode-justification-ring (make-ring (length justifications)))
  (dolist (elem justifications) (ring-insert sml-mode-justification-ring elem)))

(defun sml-cycle-mode-justification ()
  "Cycle smart mode line mode justifications in ring."
  (interactive)
  (let ((justification (ring-ref sml-mode-justification-ring -1)))
    (ring-insert sml-mode-justification-ring justification)
    (set 'sml/extra-filler justification)
    (message "sml/extra-filler %s" justification)))
;; ]


(defvar sml-theme-ring nil)
(eval-and-when-daemon frame
  (with-selected-frame frame
    (if (display-graphic-p frame)
        (progn
          ;; Set theme
          (setq sml/theme 'respectful)
          ;; Fill ring
          (let ((themes '(dark light respectful)))
            (setq sml-theme-ring (make-ring (length themes)))
            (dolist (elem themes) (ring-insert sml-theme-ring elem))))
      ;; Set theme
      (setq sml/theme 'light)
      (let ((themes '(respectful dark light)))
        (setq sml-theme-ring (make-ring (length themes)))
        (dolist (elem themes) (ring-insert sml-theme-ring elem))))
    (sml/setup)))

;; [ Cycle themes
(defun sml-cycle-themes ()
  "Cycle mode line themes in ring."
  (interactive)
  (let ((theme (ring-ref sml-theme-ring -1)))
    (ring-insert sml-theme-ring theme)
    (sml/apply-theme theme nil t)
    (message "%s mode line theme loaded" theme)))
;; ]
;; ] <smart-mode-line>

(bind-keys
 ("<f6> m" . sml-cycle-themes)
 ("<f6> j" . sml-cycle-mode-justification))



(provide 'mode-line-config)
;;; mode-line-config.el ends here
