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

(setq objed-init-p-function
      (lambda ()
        (and
         (not (minibufferp))
         (not (and (bobp) (eobp)))
         ;; don't interfere with other special modes
         ;; like hydra
         (not overriding-terminal-local-map)
         (not objed--block-p)
         (not objed-disabled-p)
         (not (apply 'derived-mode-p objed-disabled-modes))
         ;; don't activate when completing the regular Emacs way
         (not (get-buffer-window "*Completions*" 0))
         ;; don't activate during a company completion
         (not (bound-and-true-p company-candidates))
         ;; FIXME: temp workaround for starting commit
         ;; message in insertion mode
         (not (eq last-command 'magit-commit-create))
         (or (memq  major-mode '(messages-buffer-mode help-mode))
             (not (derived-mode-p 'comint-mode 'special-mode 'dired-mode)))))
      objed-initial-object 'symbol
      objed-auto-init-on-buffer-change nil
      objed-disabled-modes
      '(exwm-mode
        browse-kill-ring-mode
        completion-list-mode
        outline-mode
        special-mode)
      objed-mode-line-format
      '(:eval (propertize
               (format "%s(%s)"
                       (symbol-name objed--object)
                       (char-to-string (aref (symbol-name objed--obj-state) 0)))
               'face 'objed-mode-line))
      objed-cursor-color "#ff8c00"
      objed-use-hl nil
      objed--dispatch-alist nil
      objed--dispatch-key-alist nil)


(setcdr objed-map nil)
;; common emacs keys
(define-key objed-map (kbd "C-g") 'objed-quit)
(define-key objed-map (kbd "C-?") 'objed-show-top-level)
(define-key objed-map (kbd "C-o") 'objed-open-line)
(define-key objed-map (kbd "C-x C-x") 'objed-exchange-point-and-mark)
;; TODO: birdview mode/scroll mode
(define-key objed-map (kbd "C-h k") 'objed-describe-key)
(define-key objed-map (kbd "C-h n") 'which-key-show-next-page-cycle)
(define-key objed-map (kbd "C-h p") 'which-key-show-previous-page-cycle)
(define-key objed-map (kbd "C-M-w") 'objed-append-mode)
;; todo: restore object state, too?
(define-key objed-map (kbd "C-~") 'objed-undo-in-object)
;; general movement
(define-key objed-map (kbd "C-f") (objed--call-and-switch right-char char))
(define-key objed-map (kbd "C-b") (objed--call-and-switch left-char char))
(define-key objed-map (kbd "C-S-f") 'objed-move-char-forward)
(define-key objed-map (kbd "C-S-b") 'objed-move-char-backward)
(define-key objed-map (kbd "M-f") 'objed-forward-word)
(define-key objed-map (kbd "M-b") 'objed-backward-word)
(define-key objed-map (kbd "M-S-f") 'objed-move-word-forward)
(define-key objed-map (kbd "M-S-b") 'objed-move-word-backward)
(define-key objed-map (kbd "C-M-f") (objed--call-and-switch
                               objed--forward-sexp sexp))
(define-key objed-map (kbd "C-M-b") (objed--call-and-switch
                               objed--backward-sexp sexp))
(define-key objed-map (kbd "C-M-S-f") 'objed-move-object-forward)
(define-key objed-map (kbd "C-M-S-b") 'objed-move-object-backward)
(define-key objed-map (kbd "C-p") (objed--call-and-switch
                             previous-line line
                             nil
                             (when (objed--point-in-periphery)
                               (back-to-indentation))))
(define-key objed-map (kbd "C-n") (objed--call-and-switch
                             next-line line
                             nil
                             (when (objed--point-in-periphery)
                               (back-to-indentation))))
(define-key objed-map (kbd "C-S-n") 'objed-move-line-forward)
(define-key objed-map (kbd "C-S-p") 'objed-move-line-backward)
(define-key objed-map (kbd "M-+") 'objed-backward-until-context)
(define-key objed-map (kbd "M-ç") 'objed-forward-until-context)
(define-key objed-map (kbd "C-,") 'objed-previous) ;; objed-current-or-previous-context
(define-key objed-map (kbd "C-.") 'objed-next) ;; objed-current-or-next-context
(define-key objed-map (kbd "<C-home>") 'objed-top-object)
(define-key objed-map (kbd "<C-end>") 'objed-bottom-object)
;; block expansions
(define-key objed-map (kbd "<home>") 'objed-beg-of-block)
(define-key objed-map (kbd "<end>") 'objed-end-of-block)
(define-key objed-map (kbd "C-M-o") 'objed-expand-block)
;; context expansions
(define-key objed-map (kbd "C-S-o") 'objed-expand-context)
                                        ;(define-key objed-map (kbd "") 'objed-current-or-previous-context)
                                        ;(define-key objed-map (kbd "") 'objed-current-or-next-context)
(define-key objed-map (kbd "M-SPC") 'objed-toggle-state)
(define-key objed-map (kbd "M-j") 'objed-toggle-side)
;; marking/unmarking
(define-key objed-map (kbd "C-ç") 'objed-mark)
;; mark upwards
(define-key objed-map (kbd "C-+") 'objed-toggle-mark-backward)
;; (define-key objed-map "M" 'objed-unmark-all)
(define-key objed-map (kbd "C-=") 'objed-extend)
;; TODO: second + include more
(define-key objed-map (kbd "C-M-+") 'objed-include-backward)
(define-key objed-map (kbd "C-M-ç") 'objed-include-forward)
;; basic edit ops
(define-key objed-map (kbd "C-w") 'objed-kill)
(define-key objed-map (kbd "M-w") 'objed-copy)
(define-key objed-map (kbd "C-S-d") 'objed-delete)
(define-key objed-map (kbd "C-y") 'objed-yank)
(define-key objed-map (kbd "C-\\")
  ;; dont exit
  (objed-define-op nil objed-indent ignore))
(define-key objed-map (kbd "M-;")
  (objed-define-op nil objed-comment-or-uncomment-region))
(define-key objed-map (kbd "C-$")
  (objed-define-op nil flyspell-region))
;; quote op
(define-key objed-map (kbd "M-)") 'objed-raise)
(define-key objed-map (kbd "M-(")
  (objed-define-op nil objed-electric-pair))
;; (define-key objed-map "\""
;;   (objed-define-op nil objed-electric))
;; direct object switches
(define-key objed-map (kbd "C-:") 'objed-goto-next-identifier)
(define-key objed-map (kbd "C-;") 'objed-goto-prev-identifier)
                                        ;(define-key objed-map "_" 'objed-toggle-indentifier-place)
;; prefix keys
(define-key objed-map (kbd "M-s x") 'objed-op-map)
(define-key objed-map (kbd "M-s c") 'objed-object-map)
;; for custom user object and op commands
                                        ;(define-key objed-map (kbd "C-c o '") 'objed-user-map)
                                        ;(define-key objed-map (kbd "C-c o -") 'objed-other-user-map)
(define-key objed-map (kbd "M-g C-o") 'objed-occur)
;; special commands
(define-key objed-map (kbd "M-*") 'objed-mark-more)
(define-key objed-map (kbd "C-S-u") 'objed-last)
;; zap to object, jump to objects with avy
(define-key objed-map (kbd "M-g o") 'objed-ace)
(define-key objed-map (kbd "M-g M-o") 'objed-ace)
;; swiper like object search
;; TODO: start query replace in current object,
;; or for all
(define-key objed-map (kbd "C-%") 'objed-replace)
(define-key objed-map (kbd "M-:") 'objed-eval-expression)
(define-key objed-map (kbd "C-&")
  (objed-define-op nil objed-pipe-region))
(define-key objed-map (kbd "C-|")
  (objed-define-op nil objed-ipipe))
                                        ;(define-key objed-map "!" 'objed-execute)
(define-key objed-map (kbd "<C-M-return>")
  'objed-insert-new-object)
;; move windows
(define-key objed-map (kbd "<s-left>") 'objed-move-window-line-left)
(define-key objed-map (kbd "<s-right>") 'objed-move-window-line-right)
(define-key objed-map (kbd "<s-up>") 'objed-move-window-line-up)
(define-key objed-map (kbd "<s-down>") 'objed-move-window-line-down)
;; move text
(define-key objed-map (kbd "<C-left>") 'objed-indent-left)
(define-key objed-map (kbd "<C-right>") 'objed-indent-right)
(define-key objed-map (kbd "<M-right>") 'objed-indent-to-right-tab-stop)
(define-key objed-map (kbd "<M-left>") 'objed-indent-to-left-tab-stop)
(define-key objed-map (kbd "<C-M-left>") 'objed-forward-barf-sexp)
(define-key objed-map (kbd "<C-M-right>") 'objed-forward-slurp-sexp)
(define-key objed-map (kbd "<C-S-left>") 'objed-forward-barf-sexp)
(define-key objed-map (kbd "<C-S-right>") 'objed-forward-slurp-sexp)
(define-key objed-map (kbd " <S-left>") 'objed-move-object-backward)
(define-key objed-map (kbd " <S-right>") 'objed-move-object-forward)
;; for some objects up down is more intuitive
(define-key objed-map (kbd " <S-up>") 'objed-move-object-backward)
(define-key objed-map (kbd " <S-down>") 'objed-move-object-forward)






(define-key objed-mode-map (kbd "M-#") nil)
(define-key objed-mode-map (kbd "M-(") nil)
(define-key objed-mode-map (kbd "M-)") nil)
(define-key objed-mode-map (kbd "M-[") nil)
(define-key objed-mode-map (kbd "M-]") nil)
(define-key objed-mode-map (kbd "C-,") nil)
(define-key objed-mode-map (kbd "C-.") nil)
(define-key objed-mode-map (kbd "C-<") nil)
(define-key objed-mode-map (kbd "C->") nil)
(define-key objed-mode-map (kbd "C-c o c") 'objed-activate-object)
(define-key objed-mode-map (kbd "C-c o (") 'objed-until-beg-of-object-at-point)
(define-key objed-mode-map (kbd "C-c o )") 'objed-until-end-of-object-at-point)
(define-key objed-mode-map (kbd "C-c o [") 'objed-beg-of-object-at-point)
(define-key objed-mode-map (kbd "C-c o ]") 'objed-end-of-object-at-point)
(define-key objed-mode-map (kbd "C-;") 'objed-prev-identifier)
(define-key objed-mode-map (kbd "C-:") 'objed-next-identifier)


(push '(end-of-visual-line . line) objed-cmd-alist)
(push '(beginning-of-visual-line . line) objed-cmd-alist)

;; (with-eval-after-load 'which-key
;;   (which-key-add-key-based-replacements "=" "avy inside")
;;   (which-key-add-key-based-replacements "c" "choose type")
;;   (which-key-add-key-based-replacements "x" "additional op")
;;   (which-key-add-key-based-replacements "#" "avy other")
;;   (which-key-add-key-based-replacements "`" "forward boundary")
;;   (which-key-add-key-based-replacements "´" "backward boundary")
;;   (which-key-add-key-based-replacements "'" "user map")
;;   (which-key-add-key-based-replacements "-" "other user map"))

(defun objed--insert-keys-rebound-p ()
  "Return non-nil when any self insertion key is rebound."
  (cl-dolist (char (string-to-list "abcdefghijklmnopqrstuvwxyz"))
    (let ((binding (key-binding (vector char))))
      (when (not (and
                  (symbolp binding)
                  (string-match "insert" (symbol-name binding))))
        (cl-return binding)))))

(defun objed-focus-change (&rest _args)
  (when objed--buffer
    (with-current-buffer objed--buffer
      (objed--reset))))

(defun objed-focus-change-add-hook ()
  (if (boundp 'after-focus-change-function)
      (add-function :before after-focus-change-function 'objed-focus-change)
    (add-hook 'focus-in-hook 'objed-focus-change)
    (add-hook 'focus-out-hook 'symon-clean-echo-area)))
(add-hook 'objed-init-hook 'objed-focus-change-add-hook)

(defun objed-focus-change-remove-hook ()
  (if (boundp 'after-focus-change-function)
      (remove-function after-focus-change-function 'objed-focus-change)
    (remove-hook 'focus-in-hook 'objed-focus-change)
    (remove-hook 'focus-out-hook 'symon-clean-echo-area)))
(add-hook 'objed-exit-hook 'objed-focus-change-remove-hook)

(advice-add 'objed-eval-expression :before 'objed-focus-change)

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
(global-set-key (kbd "M-s 7 o") 'objed-toggle-mode-activate)
(global-set-key (kbd "M-s c") 'objed-activate-object)
(objed-mode 1)


(provide 'objed-config)
;;; objed-config.el ends here
