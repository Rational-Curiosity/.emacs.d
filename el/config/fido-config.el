;;; fido-config.el --- Configure fido

;;; Commentary:

;; Usage:

;;
;; (require 'fido-config)

;; sudo apt install fd-find
;; sudo apt install ripgrep
;;; Code:

(require 'icomplete)
(require 'icomplete-vertical)

(set-face-attribute 'icomplete-first-match nil :foreground "YellowGreen")

(setq icomplete-prospects-height 4)
(cond ((executable-find "fdfind")
       (setq fd-dired-program "fdfind"
             projectile-generic-command "fdfind . -0 --type f --color=never"))
      ((executable-find "fd-find")
       (setq fd-dired-program "fd-find"
             projectile-generic-command "fd-find . -0 --type f --color=never"))
      ((executable-find "fd")
       (setq fd-dired-program "fd")))

(rg-enable-default-bindings (kbd "M-g A"))

(defun insert-kill-ring-item ()
  "Insert item from kill-ring, selected with completion."
  (interactive)
  (icomplete-vertical-do
   (:separator 'dotted-line :height 20)
   (insert (completing-read "Yank: " kill-ring nil t))))

(global-set-key (kbd "M-y") 'insert-kill-ring-item)
(global-set-key (kbd "M-g f") 'fd-dired)
(global-set-key (kbd "M-g a") 'ripgrep-regexp)
(global-set-key (kbd "M-s O") 'multi-occur)

(fido-mode)


(provide 'fido-config)
;;; fido-config.el ends here
