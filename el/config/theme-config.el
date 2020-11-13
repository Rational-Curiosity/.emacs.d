;;; theme-config.el --- Setup emacs theme

;;; Commentary:

;; Usage:
;; (require 'theme-config)

;;; Code:

;; (set 'custom-enabled-themes 'wheatgrass)
(load-theme 'wombat t)
(set-face-attribute 'mode-line nil :background "#003445")
(with-eval-after-load 'which-func
  (set-face-attribute 'which-func nil :foreground "#a040bb"))

;; (require 'cursor-chg)  ; Load this library
;; (change-cursor-mode 1) ; On for overwrite/read-only/input mode
;; (toggle-cursor-type-when-idle 1) ; On when idle
;; (setq curchg-idle-cursor-type 'hbar
;;       curchg-default-cursor-type 'bar
;;       curchg-overwrite/read-only-cursor-type 'box)
(add-to-list 'default-frame-alist '(cursor-color . "red"))
(setq blink-cursor-blinks 0)
(eval-and-when-daemon frame
  (blink-cursor-mode t))
;; [ Cycle themes
(require 'ring)
(defvar theme-ring nil)
(let ((themes '(whiteboard adwaita misterioso wombat)))
  (setq theme-ring (make-ring (length themes)))
  (dolist (elem themes) (ring-insert theme-ring elem)))

(defun cycle-themes ()
  "Cycle themes in ring."
  (interactive)
  (let ((theme (ring-ref theme-ring -1)))
    (ring-insert theme-ring theme)
    (load-theme theme)
    (message "%s theme loaded" theme)))
;; ]

;; [ transparency
(defun toggle-transparency ()
   (interactive)
   (let ((alpha (frame-parameter nil 'alpha)))
     (set-frame-parameter
      nil 'alpha
      (if (eql (cond ((numberp alpha) alpha)
                     ((numberp (cdr alpha)) (cdr alpha))
                     ;; Also handle undocumented (<active> <inactive>) form.
                     ((numberp (cadr alpha)) (cadr alpha)))
               100)
          '(90 . 75) '(100 . 100)))))

(set-frame-parameter (selected-frame) 'alpha '(90 . 75))
(add-to-list 'default-frame-alist '(alpha . (90 . 75)))
;; ]

(global-set-key (kbd "M-s M-c t") #'cycle-themes)
(global-set-key (kbd "M-s M-t t") #'toggle-transparency)


(provide 'theme-config)
;;; theme-config.el ends here
