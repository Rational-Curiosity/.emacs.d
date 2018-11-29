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

;; example of eval inside mode-line
;; (setq-default projectile-mode-line
;;  '(:eval (format "{%s}" (projectile-project-name))))

(require 'ring)
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

(setq projectile-mode-line-prefix ""
      auto-revert-mode-text "Ar"
      rm-blacklist
      '(" Server" " hl-p" " WK" " Fly" " company" " Helm" " Undo-Tree"
        " Abbrev" " yas" " ws" " Projectile" " drag" " ||" " Spnxd"
        " hlt" " Hi" " MMap"))

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
(require 'smart-mode-line)
(setq line-number-display-limit nil
      line-number-display-limit-width 2000
      sml/no-confirm-load-theme t
      sml/shorten-directory t
      sml/name-width '(17 . 55)
      sml/shorten-modes nil
      sml/mode-width 'right
      ;; Ruta completa en la barra de título
      frame-title-format
      '((:eval (if buffer-file-name
                   (sml/replacer  buffer-file-name)
                 "%b"))))

;; [ Cycle mode justification
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


;; example of variable format
;; (defun variable-name-width (frame)
;;   (set (make-local-variable 'sml/name-width)
;;        (min 55       ; maximum name length
;;             (max 17  ; minimum name length
;;                  (- (window-body-width) 47)))))
;; (add-hook 'window-size-change-functions 'variable-name-width)


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
    (sml/apply-theme theme :silent)
    (message "%s mode line theme loaded" theme)))
;; ]

;; (add-hook 'after-make-frame-functions
;;           (lambda (frame)
;;             (face-remap-add-relative
;;              'mode-line '((:background "dim gray") mode-line))))

(bind-keys
 ("<f6> m" . sml-cycle-themes)
 ("<f6> j" . sml-cycle-mode-justification))



(provide 'mode-line-config)
;;; mode-line-config.el ends here
