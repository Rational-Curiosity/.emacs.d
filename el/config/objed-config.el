;;; objed-config.el --- Configure objed

;;; Commentary:

;; Usage:

;;
;; (require 'objed-config)

;; sudo apt install fd-find
;; sudo apt install ripgrep
;;; Code:

(require 'objed)

;; (add-hook 'prog-mode-hook 'objed-local-mode)

(setq objed-disabled-modes
      '(exwm-mode
        browse-kill-ring-mode
        completion-list-mode)
      objed-mode-line-format
      '(:eval (propertize
               (format "%s(%s)"
                       (symbol-name objed--object)
                       (char-to-string (aref (symbol-name objed--obj-state) 0)))
               'face 'objed-mode-line))
      objed-cursor-color "#ff8c00"
      objed-use-hl nil)

(push '(end-of-visual-line . line) objed-cmd-alist)
(push '(beginning-of-visual-line . line) objed-cmd-alist)

(with-eval-after-load 'which-key
  (which-key-add-key-based-replacements "=" "avy inside")
  (which-key-add-key-based-replacements "c" "choose type")
  (which-key-add-key-based-replacements "x" "additional op")
  (which-key-add-key-based-replacements "#" "avy other")
  (which-key-add-key-based-replacements "`" "forward boundary")
  (which-key-add-key-based-replacements "´" "backward boundary")
  (which-key-add-key-based-replacements "'" "user map")
  (which-key-add-key-based-replacements "-" "other user map"))

(defun objed-toggle-mode-activate ()
  (interactive)
  (if objed-mode
      (progn
        (objed-quit)
        (objed-mode -1)
        (message "objed-mode disabled"))
    (objed-mode 1)
    (objed-activate)
    (message "objed-mode enabled and activated")))

(global-set-key (kbd "M-SPC") 'objed-activate)
(global-set-key (kbd "M-s M-t o") 'objed-toggle-mode-activate)


(provide 'objed-config)
;;; objed-config.el ends here
