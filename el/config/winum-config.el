;;; winum-config.el --- Translation tools

;;; Commentary:

;; Usage:
;; after (require 'mode-line-config)
;; (require 'winum-config)

;;; Code:

(defun winum--define-keys (map)
  (define-key map (kbd "C-`") 'winum-select-window-by-number)
  (define-key map (kbd "M-0") 'winum-select-window-0-or-10)
  (define-key map (kbd "M-1") 'winum-select-window-1)
  (define-key map (kbd "M-2") 'winum-select-window-2)
  (define-key map (kbd "M-3") 'winum-select-window-3)
  (define-key map (kbd "M-4") 'winum-select-window-4)
  (define-key map (kbd "M-5") 'winum-select-window-5)
  (define-key map (kbd "M-6") 'winum-select-window-6)
  (define-key map (kbd "M-7") 'winum-select-window-7)
  (define-key map (kbd "M-8") 'winum-select-window-8)
  (define-key map (kbd "M-9") 'winum-select-window-9))

(defun winum--undefine-keys (map)
  (define-key map (kbd "C-`") nil)
  (define-key map (kbd "M-0") nil)
  (define-key map (kbd "M-1") nil)
  (define-key map (kbd "M-2") nil)
  (define-key map (kbd "M-3") nil)
  (define-key map (kbd "M-4") nil)
  (define-key map (kbd "M-5") nil)
  (define-key map (kbd "M-6") nil)
  (define-key map (kbd "M-7") nil)
  (define-key map (kbd "M-8") nil)
  (define-key map (kbd "M-9") nil))

(setq winum-keymap
    (let ((map (make-sparse-keymap)))
      (winum--define-keys map)
      map))

(require 'winum)

(set-face-attribute 'winum-face nil
                    :bold t
                    :foreground "red")

(setq winum-scope                       'global
      winum-reverse-frame-list          nil
      winum-auto-assign-0-to-minibuffer t
      winum-auto-setup-mode-line        t
      winum-format                      "%s"
      winum-mode-line-position          1)

(winum-mode)


(provide 'winum-config)
;;; winum-config.el ends here
