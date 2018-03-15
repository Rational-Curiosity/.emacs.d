;;; bookmark+-config.el --- Configure bookmark+

;;; Commentary:

;;; Code:

(require 'bookmark+)

(setq bookmark-default-file "~/.emacs.d/cache/bookmarks"
      bmkp-auto-light-when-jump 'all-in-buffer
      bmkp-auto-light-when-set 'all-in-buffer
      bmkp-last-as-first-bookmark-file nil
      ;;bmkp-light-style-autonamed 'lfringe
      ;;bmkp-light-style-non-autonamed 'lfringe
      bmkp-bmenu-commands-file "~/.emacs.d/cache/bmk-bmenu-commands.el")

(set-face-background 'bmkp-light-non-autonamed "DarkSlateGray")
(set-face-background 'bmkp-light-autonamed "DimGray")

(provide 'bookmark+-config)
;;; bookmark+-config.el ends here