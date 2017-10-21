;;; highlight-indent-guides-config.el --- Configure highlight

;;; Commentary:

;;; Code:

(message "Importing highlight-indent-guides-config")
(setq highlight-indent-guides-method 'character
      ;; theme
      ;; highlight-indent-guides-auto-odd-face-perc 15
      ;; highlight-indent-guides-auto-even-face-perc 15
      ;; highlight-indent-guides-auto-character-face-perc 20
      ;; disable auto-theme
      highlight-indent-guides-auto-enabled nil)

(defun my-highlight-indent-guides-config (frame)
  (unless (display-graphic-p frame)
    (setq highlight-indent-guides-character ?\|)))

(my-highlight-indent-guides-config (selected-frame))
(add-hook 'after-make-frame-functions 'my-highlight-indent-guides-config)

(set-face-background 'highlight-indent-guides-odd-face "darkgray")
(set-face-background 'highlight-indent-guides-even-face "dimgray")
(set-face-foreground 'highlight-indent-guides-character-face "dimgray")

(provide 'highlight-indent-guides-config)
;;; highlight-indent-guides-config.el ends here
