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
(when (bug-check-function-bytecode
       'orderless-try-completion
       "wAUFBSOJhA0AwYIqAIlBhCcAwgYGBgYGBiNAAUBQiQFHQrIBgioABQNChw==")
  (defun orderless-try-completion (string table pred point &optional _metadata)
    "Complete STRING to unique matching entry in TABLE.
This uses `orderless-all-completions' to find matches for STRING
in TABLE among entries satisfying PRED.  If there is only one
match, it completes to that match.  If there are no matches, it
returns nil.  In any other case it \"completes\" STRING to
itself, without moving POINT.
This function is part of the `orderless' completion style."
    (let ((all (orderless-filter string table pred)))
      (cond
       ((null all) nil)
       ((null (cdr all))
        (let ((full (concat
                     (car (orderless--prefix+pattern string table pred))
                     (car all))))
          (cons full (length full))))
       (t
        (completion-flex-try-completion string table pred point))))))
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
          orderless-transient-style-dispatchers '(ignore)))
  (setq completion-all-sorted-completions nil)
  (icomplete-pre-command-hook)
  (icomplete-post-command-hook))

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

(defun icomplete-fido-return ()
  "Exit minibuffer or enter directory, like `ido-mode'."
  (interactive)
  (let* ((dir (and (eq (icomplete--category) 'file)
                   (file-name-directory (icomplete--field-string))))
         (current (car completion-all-sorted-completions))
         (probe (and dir current
                     (expand-file-name (directory-file-name current) dir))))
    (cond ((and probe (file-directory-p probe) (not (string= current "./")))
           (icomplete-force-complete))
          (t
           (minibuffer-force-complete-and-exit)))))

(defun completing-read-advice (orig-fun prompt collection &optional
                               predicate require-match initial-input
                               hist def inherit-input-method)
  (funcall orig-fun (replace-regexp-in-string
                     "\\(:? *\\)$"
                     (cl-case require-match
                       (nil "[∅]\\1")
                       (t "[✓]\\1")
                       ('confirm "[☑]\\1")
                       ('confirm-after-completion "[↻]\\1")
                       (_ "[⚠]\\1"))
                     prompt t)
           collection predicate require-match initial-input
           hist def inherit-input-method))
(advice-add 'completing-read :around 'completing-read-advice)

;; Keys
(with-eval-after-load 'simple
  (define-key minibuffer-local-shell-command-map (kbd "M-v")
    'switch-to-completions)
  (define-key read-expression-map (kbd "M-v") 'switch-to-completions))

(define-key minibuffer-local-completion-map (kbd "C-v")
  'orderless-match-components-literal)

(define-key icomplete-minibuffer-map (kbd "C-k") 'icomplete-fido-kill)
(define-key icomplete-minibuffer-map (kbd "C-d") 'icomplete-fido-delete-char)
(define-key icomplete-minibuffer-map (kbd "RET") 'icomplete-fido-return)
(define-key icomplete-minibuffer-map (kbd "DEL") 'icomplete-fido-backward-updir)
(define-key icomplete-minibuffer-map (kbd "C-j") 'icomplete-fido-exit)
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
