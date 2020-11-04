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
(require 'completing-read-at-point)
(require 'orderless)
;; this file overides completion-category-defaults
(require 'message)

(set-face-attribute 'icomplete-first-match nil :foreground "#cafd32")

(add-hook 'minibuffer-exit-hook
          #'orderless-remove-transient-configuration)

(setq icomplete-prospects-height 4
      icomplete-separator " · "
      ;; fido
      icomplete-tidy-shadowed-file-names t
      icomplete-show-matches-on-no-input t
      icomplete-hide-common-prefix nil
      completion-styles '(orderless)
      completion-flex-nospace nil
      completion-category-defaults nil
      completion-ignore-case t
      read-buffer-completion-ignore-case t
      read-file-name-completion-ignore-case t
      ;; orderless
      orderless-matching-styles '(orderless-regexp orderless-flex)
      orderless-style-dispatchers nil)

(cond ((executable-find "fdfind")
       (setq fd-dired-program "fdfind"
             projectile-generic-command "fdfind . -0 --type f --color=never"))
      ((executable-find "fd-find")
       (setq fd-dired-program "fd-find"
             projectile-generic-command "fd-find . -0 --type f --color=never"))
      ((executable-find "fd")
       (setq fd-dired-program "fd")))

(rg-enable-default-bindings (kbd "M-g A"))

;; Functions
(defun orderless-first-regexp (pattern index _total)
  (if (= index 0) 'orderless-regexp))

(defun orderless-first-literal (pattern index _total)
  (if (= index 0) 'orderless-literal))

(defun orderless-match-components-literal ()
  "Components match regexp for the rest of the session."
  (interactive)
  (if orderless-transient-matching-styles
      (orderless-remove-transient-configuration)
    (setq orderless-transient-matching-styles '(orderless-literal)
          orderless-transient-style-dispatchers '(ignore))))

(defun icomplete-vertical-kill-ring-insert (&optional arg)
  "Insert item from kill-ring, selected with completion."
  (interactive "*p")
  (if (or (eq last-command 'yank)
          (if (active-minibuffer-window)
              (setq last-command 'yank)))
      (yank-pop arg)
    (icomplete-vertical-do
        (:separator 'dotted-line :height 20)
      (let ((candidate
             (completing-read
              "Yank: "
              (lambda (string pred action)
                (if (eq action 'metadata)
                    '(metadata (display-sort-function . identity)
                               (cycle-sort-function . identity))
                  (complete-with-action action kill-ring string pred)))
              nil t)))
        (when (and candidate (region-active-p))
          ;; the currently highlighted section is to be replaced by the yank
          (delete-region (region-beginning) (region-end)))
        (insert candidate)))))

;; Keys
(with-eval-after-load 'simple
  (define-key minibuffer-local-shell-command-map (kbd "M-v")
    'switch-to-completions)
  (define-key read-expression-map (kbd "M-v") 'switch-to-completions))

(define-key minibuffer-local-completion-map (kbd "C-v")
  'orderless-match-components-literal)

(define-key icomplete-minibuffer-map (kbd "C-k") 'icomplete-fido-kill)
(define-key icomplete-minibuffer-map (kbd "C-d") 'icomplete-fido-delete-char)
(define-key icomplete-minibuffer-map (kbd "RET") 'icomplete-fido-ret)
(define-key icomplete-minibuffer-map (kbd "C-m") 'icomplete-fido-ret)
(define-key icomplete-minibuffer-map (kbd "DEL") 'icomplete-fido-backward-updir)
(define-key icomplete-minibuffer-map (kbd "M-j") 'icomplete-fido-exit)
(define-key icomplete-minibuffer-map (kbd "C-s") 'icomplete-forward-completions)
(define-key icomplete-minibuffer-map (kbd "C-r") 'icomplete-backward-completions)
(define-key icomplete-minibuffer-map (kbd "C-|") 'icomplete-vertical-toggle)
(define-key icomplete-fido-mode-map (kbd "C-|") 'icomplete-vertical-toggle)
(global-set-key (kbd "M-y") 'icomplete-vertical-kill-ring-insert)
(global-set-key (kbd "M-g f") 'fd-dired)
(global-set-key (kbd "M-g a") 'ripgrep-regexp)
(global-set-key (kbd "M-s O") 'multi-occur)
(global-set-key (kbd "<f12>") (lambda ()
                                (interactive)
                                (message "icomplete overlay: %s"
                                         (overlay-get icomplete-overlay 'after-string))))

(icomplete-mode)
(setq fido-mode t)
(completing-read-at-point-mode)


(provide 'fido-config)
;;; fido-config.el ends here
