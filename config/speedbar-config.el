;;; speedbar-config.el --- Configure speedbar

;;; Commentary:

;; Usage:
;; (require 'speedbar-config)

;;; Code:

(message "Importing speedbar-config")
(require 'speedbar)

;; (require 'eieio)
;; (require 'eieio-speedbar)
;; (require 'eieio-opt)
;; (require 'eieio-base)
;; (setq speedbar-frame-parameters
;;       '((minibuffer)
;;         (width . 40)
;;         (border-width . 0)
;;         (menu-bar-lines . 0)
;;         (tool-bar-lines . 0)
;;         (unsplittable . t)
;;         (left-fringe . 0)))
;; (setq speedbar-hide-button-brackets-flag t)
;; (setq speedbar-show-unknown-files t)
;; (setq speedbar-smart-directory-expand-flag t)

(defun my-speedbar-enable-images (&optional frame)
  "Enable images in FRAME."
  (interactive)
  (when (display-graphic-p)
    (setq ezimage-use-images t
          speedbar-use-images t)))
(my-speedbar-enable-images)
(add-hook 'after-make-frame-functions 'my-speedbar-enable-images)

(speedbar-enable-update)

(provide 'speedbar-config)
;;; speedbar-config.el ends here
