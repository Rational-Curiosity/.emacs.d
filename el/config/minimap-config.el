;;; minimap-config.el --- Configure minimap

;;; Commentary:

;; Usage:
;; (require 'minimap-config)

;; Code:

(require 'minimap)
(setcar (cdr (assq 'minimap-mode minor-mode-alist)) "")

;; (add-hook 'minimap-sb-mode-hook
;;           (lambda ()
;;             (setq mode-line-format nil)
;;             (set-window-fringes (minimap-get-window) 0 0 nil)))

;;(minimap-mode 1)

;;    ###    ########  ####
;;   ## ##   ##     ##  ##
;;  ##   ##  ##     ##  ##
;; ##     ## ########   ##
;; ######### ##         ##
;; ##     ## ##         ##
;; ##     ## ##        ####

;; (set-face-attribute 'minimap-font-face nil
;;                     :family "Iosevka Term")

(setq minimap-window-location 'right
      minimap-width-fraction 0.1
      minimap-minimum-width 10
      minimap-update-delay 0.1
      minimap-always-recenter nil
      minimap-recenter-type 'free
      minimap-display-semantic-overlays t
      minimap-tag-only nil
      minimap-hide-scroll-bar t
      minimap-hide-fringes t
      minimap-major-modes '(prog-mode text-mode))

(defun minimap-toggle ()
  "Toggle minimap for current buffer."
  (interactive)
  (if (minimap-get-window)
      (minimap-kill)
    (minimap-create)))
(global-set-key (kbd "<f7> m") 'minimap-toggle)


(provide 'minimap-config)
;;; minimap-config.el ends here
