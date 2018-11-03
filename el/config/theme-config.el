;;; theme-config.el --- Setup emacs theme

;;; Commentary:

;; Usage:
;; (require 'theme-config)

;;; Code:

;; (set 'custom-enabled-themes 'wheatgrass)
(load-theme 'wombat t)

(require 'cursor-chg)  ; Load this library
(change-cursor-mode 1) ; On for overwrite/read-only/input mode
(toggle-cursor-type-when-idle 1) ; On when idle
(setq curchg-idle-cursor-type 'hbar
      curchg-default-cursor-type 'bar
      curchg-overwrite/read-only-cursor-type 'box)

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

(bind-keys
 ("<f7> b" . cycle-themes))


(provide 'theme-config)
;;; theme-config.el ends here
