;;; minimap-config.el --- Configure minimap

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'minimap
;;   (require 'minimap-config))
;; never
;; (require 'minimap-config)

;; Code:
(setq minor-mode-alist (assq-delete-all 'minimap-mode minor-mode-alist))

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

(unless (face-foreground 'minimap-current-line-face)
  (set-face-attribute 'minimap-current-line-face nil
                      :foreground "yellow"))

(setq minimap-window-location 'right
      minimap-width-fraction 0.0 ;; always minimum width 
      minimap-minimum-width 15
      minimap-update-delay 0.3
      minimap-always-recenter nil
      minimap-recenter-type 'relative
      minimap-display-semantic-overlays nil ;; heavy old parser
      minimap-tag-only nil
      minimap-hide-scroll-bar t
      minimap-hide-fringes t
      minimap-enlarge-certain-faces nil
      minimap-sync-overlay-properties '(invisible)
      minimap-major-modes '(prog-mode text-mode))

;; minimap version 1.2
;; (defun minimap-toggle ()
;;   "Toggle minimap for current buffer."
;;   (interactive)
;;   (if (minimap-get-window)
;;       (minimap-kill)
;;     (minimap-create)))
;; (global-set-key (kbd "M-s 7 m") 'minimap-toggle)


(provide 'minimap-config)
;;; minimap-config.el ends here
