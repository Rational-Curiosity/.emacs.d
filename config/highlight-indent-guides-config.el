;;; highlight-indent-guides-config.el --- Configure highlight

;;; Commentary:

;;; Code:

(message "Importing highlight-indent-guides-config")

(require 'config-lib)

(setq highlight-indent-guides-method 'character
      ;; theme
      highlight-indent-guides-auto-odd-face-perc 15
      highlight-indent-guides-auto-even-face-perc 15
      highlight-indent-guides-auto-character-face-perc 20
      ;; disable or enable auto-theme
      highlight-indent-guides-auto-enabled nil)

(with-daemon-after-frame frame
  (unless (display-graphic-p frame)
    (setq highlight-indent-guides-character ?\|)))

(set-face-background 'highlight-indent-guides-odd-face "darkgray")
(set-face-background 'highlight-indent-guides-even-face "dimgray")
(set-face-foreground 'highlight-indent-guides-character-face "dimgray")

(bind-keys
 ("<f7> i" . highlight-indent-guides-mode))


(provide 'highlight-indent-guides-config)
;;; highlight-indent-guides-config.el ends here
