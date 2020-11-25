;;; fido-config.el --- Configure fido

;;; Commentary:

;; Usage:

;;
;; (require 'fido-config)

;; sudo apt install fd-find
;; sudo apt install ripgrep
;;; Code:

(require 'icomplete)
;; (when (bug-check-function-bytecode
;;        'icomplete-force-complete-and-exit
;;        "wyDEIFaEFAAIhBAACYQUAAqDFwDFIIfGIIc=")
;;   (defun icomplete-force-complete-and-exit ()
;;     "Complete the minibuffer with the longest possible match and exit.
;; Use the first of the matches if there are any displayed, and use
;; the default otherwise."
;;     (interactive)
;;     ;; This function is tricky.  The mandate is to "force", meaning we
;;     ;; should take the first possible valid completion for the input.
;;     ;; However, if there is no input and we can prove that that
;;     ;; coincides with the default, it is much faster to just call
;;     ;; `minibuffer-complete-and-exit'.  Otherwise, we have to call
;;     ;; `minibuffer-force-complete-and-exit', which needs the full
;;     ;; completion set and is potentially slow and blocking.  Do the
;;     ;; latter if:
;;     (if (and (null completion-cycling)
;;              (or
;;               ;; there's some input, meaning the default in off the table by
;;               ;; definition; OR
;;               (> (icomplete--field-end) (icomplete--field-beg))
;;               ;; there's no input, but there's also no minibuffer default
;;               ;; (and the user really wants to see completions on no input,
;;               ;; meaning he expects a "force" to be at least attempted); OR
;;               (and (not minibuffer-default)
;;                    icomplete-show-matches-on-no-input)
;;               ;; there's no input but the full completion set has been
;;               ;; calculated, This causes the first cached completion to
;;               ;; be taken (i.e. the one that the user sees highlighted)
;;               completion-all-sorted-completions))
;;         (minibuffer-force-complete-and-exit)
;;       ;; Otherwise take the faster route...
;;       (minibuffer-complete-and-exit))))
;; (when (bug-check-function-bytecode
;;        'icomplete--sorted-completions
;;        "CIagAMMgxCDAAgIiCYMjAAk7gyMAxCDDIFWDIwDFxkSCNQAKhTUACT+FNQDHIMg9hTUAyUPKy8oDOoOVAANAsgMEysvKAzqDggADQTqDggADQUCyAwYGAyGDdwADiYlBQaG2AswGCwYLBQYMQiOyAcqJsgOCeADLg4IAA0GyBIJFALaDibICP4myA4OVAANBsgSCOAABg50ABIKeAIm2h4c=")
;;   (defun icomplete--sorted-completions ()
;;     (or completion-all-sorted-completions
;;         (cl-loop
;;          with beg = (icomplete--field-beg)
;;          with end = (icomplete--field-end)
;;          with all = (completion-all-sorted-completions beg end)
;;          for fn in (cond ((and minibuffer-default
;;                                (stringp minibuffer-default) ; bug#38992
;;                                (= (icomplete--field-end) (icomplete--field-beg)))
;;                           ;; When we have a non-nil string default and
;;                           ;; no input whatsoever: we want to make sure
;;                           ;; that default is bubbled to the top so that
;;                           ;; `icomplete-force-complete-and-exit' will
;;                           ;; select it (do that even if the match
;;                           ;; doesn't match the completion perfectly.
;;                           `(,(lambda (comp)
;;                                (equal minibuffer-default comp))))
;;                          ((and fido-mode
;;                                (not minibuffer-default)
;;                                (eq (icomplete--category) 'file))
;;                           ;; `fido-mode' has some extra file-sorting
;;                           ;; semantics even if there isn't a default,
;;                           ;; which is to bubble "./" to the top if it
;;                           ;; exists.  This makes M-x dired RET RET go to
;;                           ;; the directory of current file, which is
;;                           ;; what vanilla Emacs and `ido-mode' both do.
;;                           `(,(lambda (comp)
;;                                (string= "./" comp)))))
;;          thereis (cl-loop
;;                   for l on all
;;                   while (consp (cdr l))
;;                   for comp = (cadr l)
;;                   when (funcall fn comp)
;;                   do (setf (cdr l) (cddr l))
;;                   and return
;;                   (completion--cache-all-sorted-completions beg end (cons comp all)))
;;          finally return all))))

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

;; Another functions override this variables, then
;; set every time enter minibuffer
(setq
 ;; icomplete
 icomplete-prospects-height 4
 icomplete-separator " · "
 ;; orderless
 orderless-matching-styles '(orderless-regexp)
 orderless-component-separator ",+"
 orderless-style-dispatchers nil)
(defun icomplete--fido-mode-setup ()
  "Setup `fido-mode''s minibuffer."
  (when (and icomplete-mode (icomplete-simple-completing-p))
    (use-local-map (make-composed-keymap icomplete-fido-mode-map
                                         (current-local-map)))
    (setq-local
     ;; fido
     icomplete-tidy-shadowed-file-names t
     icomplete-show-matches-on-no-input t
     icomplete-hide-common-prefix nil
     completion-styles '(orderless)
     completion-flex-nospace nil
     completion-category-defaults nil
     completion-ignore-case t
     read-buffer-completion-ignore-case t
     read-file-name-completion-ignore-case t)
    ;; [ fix bugs
    (when completion-cycling
      (funcall (prog1 completion-cycling (setq completion-cycling nil))))
    ;; ]
    ))

(cond ((executable-find "fdfind")
       (setq fd-dired-program "fdfind"
             projectile-generic-command "fdfind . -0 --type f --color=never"))
      ((executable-find "fd-find")
       (setq fd-dired-program "fd-find"
             projectile-generic-command "fd-find . -0 --type f --color=never"))
      ((executable-find "fd")
       (setq fd-dired-program "fd")))

(rg-enable-default-bindings (kbd "M-g a"))

;; Functions
(defun orderless-first-regexp (pattern index _total)
  (if (= index 0) 'orderless-regexp))

(defun orderless-first-literal (pattern index _total)
  (if (= index 0) 'orderless-literal))

(defun orderless-match-components-cycle ()
  "Components match regexp for the rest of the session."
  (interactive)
  (cl-case (car orderless-transient-matching-styles)
    ;; last in cycle
    (orderless-flex
     (orderless-remove-transient-configuration))
    ;; middle in cycle
    ;; first in cycle
    (otherwise
     (setq orderless-transient-matching-styles '(orderless-flex)
           orderless-transient-style-dispatchers '(ignore))))
  (completion--flush-all-sorted-completions)
  (icomplete-pre-command-hook)
  (icomplete-post-command-hook))

(defun nmcli-connect-vpn (up-down name)
  (interactive
   (list
    (completing-read "Choose up/down(up): " '("up" "down") nil t nil nil "up")
    (completing-read "Choose VPN:"
                     (split-string
                      (shell-command-to-string
                       "nmcli --colors no -t -f name con")
                      "\n" t)
                     nil t)))
  (shell-command
   (concat "nmcli --ask --colors no -t con " up-down " \"" name "\"")))

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

(defun completing-read-advice (orig-fun prompt collection &optional
                               predicate require-match initial-input
                               hist def inherit-input-method)
  (funcall orig-fun (replace-regexp-in-string
                     "\\(:? *\\)$"
                     (cl-case require-match
                       (nil "[>]\\1")
                       (t "[·]\\1")
                       (confirm "[!]\\1")
                       (confirm-after-completion "[`]\\1")
                       (otherwise "[.]\\1"))
                     prompt t)
           collection predicate require-match initial-input
           hist def inherit-input-method))
(advice-add 'completing-read :around 'completing-read-advice)
(with-eval-after-load 'crm
  (advice-add 'completing-read-multiple :around 'completing-read-advice))

(if (null (require 'noccur nil 'noerror)) ;; noccur--find-files noccur-project
    (message-color #("ERROR missing package `noccur'"
                     0 5 (face error)))
  (when (bug-check-function-bytecode
         'noccur--find-files
         "wyCDCQDEggoAxRjGxwgJIxrIyQohyiIqhw==")
    (require 'pcre2el) ;; rxt-elisp-to-pcre
    (if (executable-find "rg")
        (defun noccur--find-files (regexp)
          (split-string (shell-command-to-string
                         (concat
                          "rg --no-heading --color=never -lH \""
                          (rxt-elisp-to-pcre regexp) "\""))
                        "\n" t))
      (defun noccur--find-files (regexp)
        (let* ((listing-command (if (noccur--within-git-repository-p)
                                    "git ls-files -z"
                                  "find . -type f -print0"))
               (command (format "%s | xargs -0 grep -l \"%s\""
                                listing-command
                                (rxt-elisp-to-pcre regexp))))
          (split-string (shell-command-to-string command) "\n")))))

  (defun noccur-projectile (regexp &optional nlines)
    (interactive (occur-read-primary-args))
    (if (require 'projectile nil 'noerror) ;; projectile-ensure-project
                                           ;; projectile-project-root
        (noccur-project regexp nlines
                        (projectile-ensure-project
                         (projectile-project-root)))
      (error "Package `projectile' is not available"))))


;; Keys
(with-eval-after-load 'simple
  (define-key minibuffer-local-shell-command-map (kbd "M-v")
    'switch-to-completions)
  (define-key read-expression-map (kbd "M-v") 'switch-to-completions))

(define-key minibuffer-local-completion-map (kbd "C-v")
  'orderless-match-components-cycle)

(define-key icomplete-minibuffer-map (kbd "C-k") 'icomplete-fido-kill)
(define-key icomplete-minibuffer-map (kbd "C-d") 'icomplete-fido-delete-char)
(define-key icomplete-minibuffer-map (kbd "RET") 'icomplete-fido-ret)
(define-key icomplete-fido-mode-map (kbd "C-m") nil)
(define-key icomplete-minibuffer-map (kbd "DEL") 'icomplete-fido-backward-updir)
(define-key icomplete-minibuffer-map (kbd "C-j") 'icomplete-fido-exit)
(define-key icomplete-fido-mode-map (kbd "C-j") 'icomplete-fido-exit)
(define-key icomplete-fido-mode-map (kbd "M-j") nil)
(define-key icomplete-minibuffer-map (kbd "C-s") 'icomplete-forward-completions)
(define-key icomplete-minibuffer-map (kbd "C-r") 'icomplete-backward-completions)
(define-key icomplete-minibuffer-map (kbd "C-|") 'icomplete-vertical-toggle)
(define-key icomplete-fido-mode-map (kbd "C-|") 'icomplete-vertical-toggle)
(global-set-key (kbd "M-g M-a") 'noccur-projectile)
(global-set-key (kbd "M-g M-f") 'projectile-find-file)
(global-set-key (kbd "M-y") 'icomplete-vertical-kill-ring-insert)
(global-set-key (kbd "M-g f") 'fd-dired)
(global-set-key (kbd "M-s O") 'multi-occur)
(global-set-key (kbd "M-s M-o") 'noccur-project)
(global-set-key (kbd "M-s C-o") 'noccur-dired)
(global-set-key
 (kbd "<f12>")
 (lambda ()
   (interactive)
   (message "log: %s" (list completion-cycling minibuffer-default))))

(fido-mode)
(completing-read-at-point-mode)


(provide 'fido-config)
;;; fido-config.el ends here
