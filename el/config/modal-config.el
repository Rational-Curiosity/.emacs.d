;;; modal-config.el --- Configure modal

;;; Commentary:

;; Usage:
;; (require 'modal-config)

;;; Code:
(require 'modal)

(setq modal-insert-cursor-type 'box
      modal-cursor-type 'hollow
      modal-excluded-modes '(buffer-menu-mode
                             ibuffer-mode
                             package-menu-mode
                             debugger-mode
                             dired-mode
                             calc-mode
                             ediff-mode
                             eshell-mode
                             org-agenda-mode
                             git-rebase-mode
                             magit-popup-mode
                             magit-mode
                             magit-process-mode
                             magit-status-mode
                             docker-container-mode))


(defun keyboard-esc-quit ()
  "Exit the current \"mode\" (in a generalized sense of the word).
This command can exit an interactive command such as `query-replace',
can clear out a prefix argument or a region,
can get out of the minibuffer or other recursive edit,
cancel the use of the current buffer (for special-purpose buffers)."
  (interactive)
  (cond ((eq last-command 'mode-exited) nil)
        ((region-active-p)
         (deactivate-mark))
        ((> (minibuffer-depth) 0)
         (abort-recursive-edit))
        (current-prefix-arg
         nil)
        ((> (recursion-depth) 0)
         (exit-recursive-edit))
        (buffer-quit-function
         (funcall buffer-quit-function))
        (t (keyboard-quit))))

;;;;;;;;;;
;; Keys ;;
;;;;;;;;;;

(define-key indent-rigidly-map "F" #'indent-rigidly-right-to-tab-stop)
(define-key indent-rigidly-map "B" #'indent-rigidly-left-to-tab-stop)
(define-key indent-rigidly-map "f" #'indent-rigidly-right)
(define-key indent-rigidly-map "b" #'indent-rigidly-left)

;; Modal editing
(with-eval-after-load 'smartparens
  (define-key modal-mode-map [remap backward-sexp] 'sp-backward-sexp)
  (define-key modal-mode-map [remap forward-sexp] 'sp-forward-sexp))

;; ' (handy as self-inserting symbol)
;; " (handy as self-inserting symbol)
(modal-define-kbd "(" "C-(" "sp-rewrap-sexp-lc" t)
(modal-define-kbd ")" "C-)" "sp-unwrap-sexp-lc" t)
(define-key modal-mode-map "&" #'rotate-or-inflection)
(define-key modal-mode-map "?" #'goto-last-change-reverse)
(define-key modal-mode-map "/" #'goto-last-change)
;; (modal-define-kbd "." "M-." "definition-at-point")
(modal-define-kbd ";" "M-;" "comment-dwim" t)
(modal-define-kbd ":" "M-:" "eval-expression")
(define-key modal-mode-map "+" #'fold-dwim)
(modal-define-kbd "-" "C-_" "undo-tree-undo" t)
(modal-define-kbd "_" "M-_" "undo-tree-redo" t)
;; (modal-define-kbd "," "M-," "declaration-at-point")
(modal-define-kbd "%" "M-%" "query-replace" t)
(modal-define-kbd "*" "C-*" "duplicate-current-line-or-region" t)
(modal-define-kbd "<" "M-<" "beginning-of-buffer")
(modal-define-kbd ">" "M->" "end-of-buffer")
(modal-define-kbd "SPC" "C-SPC" "set-mark-command")

(modal-define-kbd "0" "C-0" "prefix-0")
(modal-define-kbd "1" "C-1" "prefix-1")
(modal-define-kbd "2" "C-2" "prefix-2")
(modal-define-kbd "3" "C-3" "prefix-3")
(modal-define-kbd "4" "C-4" "prefix-4")
(modal-define-kbd "5" "C-5" "prefix-5")
(modal-define-kbd "6" "C-6" "prefix-6")
(modal-define-kbd "7" "C-7" "prefix-7")
(modal-define-kbd "8" "C-8" "prefix-8")
(modal-define-kbd "9" "C-9" "prefix-9")

(modal-define-kbd "a" "C-a" "move-beginning-of-line")
(modal-define-kbd "b" "C-b" "backward-char")
;; c - [ command prefix
(define-key modal-mode-map (kbd "c '") (kbd "C-c '"))
(define-key modal-mode-map (kbd "c ! c") #'flycheck-buffer)
(define-key modal-mode-map (kbd "c ! n") #'flycheck-next-error)
(define-key modal-mode-map (kbd "c ! p") #'flycheck-previous-error)
(define-key modal-mode-map (kbd "c ! l") #'flycheck-list-errors)
(modal-define-key (kbd "c a") (kbd "C-c a") "org-agenda")
(define-key modal-mode-map (kbd "c b") (kbd "C-c C-b"))  ;; go-back
(define-key modal-mode-map (kbd "c B") (kbd "C-c M-b"))  ;; org-previous-block
(modal-define-key (kbd "c c") (kbd "C-c C-c") "confirm-commit")
(define-key modal-mode-map (kbd "c e w") #'er/mark-word)
(define-key modal-mode-map (kbd "c e s") #'er/mark-symbol)
(define-key modal-mode-map (kbd "c e c") #'er/mark-method-call)
(define-key modal-mode-map (kbd "c e q") #'er/mark-inside-quotes)
(define-key modal-mode-map (kbd "c e Q") #'er/mark-outside-quotes)
(define-key modal-mode-map (kbd "c e p") #'er/mark-inside-pairs)
(define-key modal-mode-map (kbd "c e P") #'er/mark-outside-pairs)
(define-key modal-mode-map (kbd "c f") (kbd "C-c C-f"))  ;; org-forward-heading-same-level
(define-key modal-mode-map (kbd "c F") (kbd "C-c M-f"))  ;; org-next-block
(define-key modal-mode-map (kbd "c i s") #'spanish-dictionary)
(define-key modal-mode-map (kbd "c i e") #'english-dictionary)
(define-key modal-mode-map (kbd "c i c") #'flyspell-buffer)
(define-key modal-mode-map (kbd "c i n") #'flyspell-goto-next-error)
(define-key modal-mode-map (kbd "c i p") #'flyspell-goto-previous-error)
(define-key modal-mode-map (kbd "c i a") #'flyspell-auto-correct-word)
(modal-define-key (kbd "c k") (kbd "C-c C-k") "cancel-commit")
(define-key modal-mode-map (kbd "c m p") #'mc/mark-previous-like-this)
(define-key modal-mode-map (kbd "c m n") #'mc/mark-next-like-this)
(define-key modal-mode-map (kbd "c m a") #'mc/mark-all-like-this-dwim)
(define-key modal-mode-map (kbd "c n") (kbd "C-c C-n"))  ;; smartscan-symbol-go-forward or org-next-visible-heading
(define-key modal-mode-map (kbd "c o") #'operate-on-number-at-point-or-region)
(define-key modal-mode-map (kbd "c p") (kbd "C-c C-p"))  ;; smartscan-symbol-go-backward or org-previous-visible-heading
(define-key modal-mode-map (kbd "c r") (kbd "C-c M-s"))  ;; org-sort-entries-user-defined
(define-key modal-mode-map (kbd "c R") #'revert-buffer)
(modal-define-key (kbd "c s") (kbd "C-c C-s") "org-schedule")
(modal-define-key (kbd "c t") (kbd "C-c C-t") "org-todo")
(define-key modal-mode-map (kbd "c u") (kbd "C-c C-u"))  ;; outline-up-heading
(define-key modal-mode-map (kbd "c v *") #'vimish-fold-unfold-all)
(define-key modal-mode-map (kbd "c v +") #'vimish-fold-unfold)
(define-key modal-mode-map (kbd "c v -") #'vimish-fold-refold)
(define-key modal-mode-map (kbd "c v _") #'vimish-fold-refold-all)
(define-key modal-mode-map (kbd "c v .") #'vimish-fold-toggle)
(define-key modal-mode-map (kbd "c v :") #'vimish-fold-toggle-all)
(define-key modal-mode-map (kbd "c v d") #'vimish-fold-delete)
(define-key modal-mode-map (kbd "c v D") #'vimish-fold-delete-all)
(define-key modal-mode-map (kbd "c v f") #'vimish-fold)
(define-key modal-mode-map (kbd "c v G") #'vimish-fold-avy)
(define-key modal-mode-map (kbd "c V s") (kbd "C-c v s"))  ;; org-block-and-result-show-all
(define-key modal-mode-map (kbd "c V h") (kbd "C-c v h"))  ;; org-block-and-result-hide-all
(define-key modal-mode-map (kbd "c w t") #'transpose-frame)
(define-key modal-mode-map (kbd "c w h") #'flop-frame)
(define-key modal-mode-map (kbd "c w v") #'flip-frame)
(define-key modal-mode-map (kbd "c w b") #'windmove-left)
(define-key modal-mode-map (kbd "c w f") #'windmove-right)
(define-key modal-mode-map (kbd "c w n") #'windmove-down)
(define-key modal-mode-map (kbd "c w p") #'windmove-up)
(define-key modal-mode-map (kbd "c w r") #'rotate-frame-clockwise)
(define-key modal-mode-map (kbd "c w R") #'rotate-frame-anticlockwise)
(define-key modal-mode-map (kbd "c w -") #'winner-undo)
(define-key modal-mode-map (kbd "c w _") #'winner-redo)
(define-key modal-mode-map (kbd "c x n") (kbd "C-c C-x C-n"))  ;; org-next-link
(define-key modal-mode-map (kbd "c x p") (kbd "C-c C-x C-p"))  ;; org-previous-link
(define-key modal-mode-map (kbd "c x s") (kbd "C-c C-x C-s"))  ;; org-archive-subtree
;; c - ] command prefix
(modal-define-kbd "d" "<deletechar>" "delete-forward-char" t)
(modal-define-kbd "e" "C-e" "move-end-of-line")
(modal-define-kbd "f" "C-f" "forward-char")
;; g - [ prefix
(define-key modal-mode-map (kbd "g c") #'avy-goto-char)
(define-key modal-mode-map (kbd "g C") #'avy-goto-char-2)
(define-key modal-mode-map (kbd "g l") #'avy-goto-line)
(define-key modal-mode-map (kbd "g s") #'sp-end-of-sexp)
(define-key modal-mode-map (kbd "g S") #'sp-beginning-of-sexp)
(define-key modal-mode-map (kbd "g w") #'avy-goto-word-1)
(define-key modal-mode-map (kbd "g W") #'avy-goto-word-0)
(define-key modal-mode-map (kbd "g k") #'link-hint-open-link)
(define-key modal-mode-map (kbd "g K") #'link-hint-copy-link)
;; g - ] prefix
;; h - [ prefix
(define-key modal-mode-map (kbd "h b") #'describe-bindings)
(define-key modal-mode-map (kbd "h e") #'view-echo-area-messages)
(define-key modal-mode-map (kbd "h f") #'describe-function)
(define-key modal-mode-map (kbd "h k") #'describe-key)
(define-key modal-mode-map (kbd "h L") #'describe-language-environment)
(define-key modal-mode-map (kbd "h m") #'describe-mode)
(define-key modal-mode-map (kbd "h o") #'describe-symbol)
(define-key modal-mode-map (kbd "h P") #'describe-package)
(define-key modal-mode-map (kbd "h s") #'describe-syntax)
(define-key modal-mode-map (kbd "h v") #'describe-variable)
;; h - ] prefix
;; i - reserved
(modal-define-kbd "j" "C-j" "electric-newline-and-maybe-indent" t)
(modal-define-kbd "k" "C-k" "kill-line" t)
(modal-define-kbd "l" "C-l" "recenter-top-bottom")
(modal-define-kbd "m" "C-m" "newline" t)
(modal-define-kbd "n" "C-n" "next-line")
(modal-define-kbd "o" "C-o" "open-line" t)
(modal-define-kbd "p" "C-p" "previous-line")
(modal-define-kbd "q" "C-q" "quoted-insert" t)
(modal-define-kbd "r" "C-r" "isearch-backward")
(modal-define-kbd "s" "C-s" "isearch-forward")
(modal-define-kbd "t" "C-t" "transpose-chars" t)
;; u - reserved
(modal-define-kbd "v" "C-v" "scroll-up-command")
(modal-define-kbd "w" "C-w" "kill-region" t)
;; x - [ command prefix
(modal-define-kbd "x TAB" "C-x TAB" "indent-rigidly")
(modal-define-kbd "x RET" "C-x C-o" "delete-blank-lines")
(modal-define-kbd "x SPC" "C-x C-SPC" "pop-global-mark")
(modal-define-kbd "x S-SPC" "C-x SPC" "rectangle-mark-mode")
(modal-define-kbd "x ;" "C-x C-;" "comment-line")
(modal-define-kbd "x (" "C-x (" "kmacro-start-macro")
(modal-define-kbd "x )" "C-x )" "kmacro-end-macro")
(modal-define-kbd "x +" "C-x +" "balance-windows")
(modal-define-kbd "x 0" "C-x 0" "delete-window")
(modal-define-kbd "x 1" "C-x 1" "delete-other-windows")
(modal-define-kbd "x 2" "C-x 2" "vsplit-last-buffer")
(modal-define-kbd "x 3" "C-x 3" "hsplit-last-buffer")
(modal-define-kbd "x 4 f" "C-x 4 C-f" "ido-find-file-other-window")
(modal-define-kbd "x 4 b" "C-x 4 b" "ido-switch-buffer-other-window")
(modal-define-kbd "x 5 f" "C-x 5 C-f" "ido-find-file-other-frame")
(modal-define-kbd "x 5 b" "C-x 5 b" "ido-switch-buffer-other-frame")
(modal-define-kbd "x c" "C-x C-c" "save-buffers-kill-emacs")
(modal-define-kbd "x b" "C-x b" "switch-buffer")
(modal-define-kbd "x e" "C-x C-e" "eval-last-sexp")
(modal-define-kbd "x E" "C-x e" "kmacro-end-and-call-macro")
(modal-define-kbd "x f" "C-x C-f" "find-file")
(modal-define-kbd "x F" "C-x f" "find-file-at-point")
(modal-define-kbd "x H" "C-x h" "mark-whole-buffer")
(modal-define-kbd "x k k" "C-x C-k C-k" "kmacro-end-or-call-macro-repeat")
(modal-define-kbd "x k n" "C-x C-k C-n" "kmacro-cycle-ring-next")
(modal-define-kbd "x k N" "C-x C-k n" "kmacro-name-last-macro")
(modal-define-kbd "x k p" "C-x C-k C-p" "kmacro-cycle-ring-previous")
(modal-define-kbd "x k v" "C-x C-k C-v" "kmacro-view-macro-repeat")
(modal-define-kbd "x K" "C-x k" "kill-buffer")
(modal-define-kbd "x l" "C-x C-l" "downcase-region")
(modal-define-kbd "x o" "C-x o" "other-window")
(define-key modal-mode-map (kbd "x O") #'ff-find-other-file)
(modal-define-kbd "x p" "C-x C-p" "mark-page")
(modal-define-kbd "x P s" "C-x p s" "bookmark-save")
(modal-define-kbd "x r" "C-x C-r" "recentf-open")
(modal-define-kbd "x R m" "C-x r m" "bookmark-set")
(modal-define-kbd "x R b" "C-x r b" "bookmark-jump")
(modal-define-kbd "x R l" "C-x r l" "list-bookmarks")
(modal-define-kbd "x s" "C-x C-s" "save-buffer")
(modal-define-kbd "x S" "C-x s" "save-some-buffers")
(modal-define-kbd "x u" "C-x C-u" "upcase-region")
(define-key modal-mode-map (kbd "x v =") #'magit-diff)
(define-key modal-mode-map (kbd "x v b") #'magit-branch)
(define-key modal-mode-map (kbd "x v c") #'magit-checkout)
(define-key modal-mode-map (kbd "x v L") #'vc-print-root-log)
(define-key modal-mode-map (kbd "x v l") #'vc-print-log)
(define-key modal-mode-map (kbd "x v m") #'hydra-smerge/body)
(define-key modal-mode-map (kbd "x v P") #'magit-push)
(define-key modal-mode-map (kbd "x v p") #'magit-pull)
(define-key modal-mode-map (kbd "x v R") #'magit-rebase-continue)
(define-key modal-mode-map (kbd "x v r") #'magit-rebase)
(define-key modal-mode-map (kbd "x v v") #'magit-status)
(modal-define-kbd "x x" "C-x C-x" "exchange-point-and-mark")
(modal-define-kbd "x z" "C-x z" "repeat")
;; x - ] command prefix
(modal-define-kbd "y" "C-y" "yank")
(define-key modal-mode-map "z" #'avy-goto-char-timer)

(modal-define-kbd "S-\\" "C-M-\\" "indent-region")
(modal-define-kbd "S-@" "C-M-@" "mark-sexp")
(modal-define-kbd "A" "C-M-a" "beginning-of-defun")
(modal-define-kbd "B" "C-M-b" "backward-sexp")
(modal-define-kbd "C" "C-M-c" "exit-recursive-edit")
(modal-define-kbd "D" "C-M-d" "down-list")
(modal-define-kbd "E" "C-M-e" "end-of-defun")
(modal-define-kbd "F" "C-M-f" "forward-sexp")
;; G - [ command prefix
;; G - ] command prefix
(modal-define-kbd "H" "C-M-h" "mark-defun")
(modal-define-kbd "I" "C-M-i" "completion-at-point/flyspell-auto-correct-word")
(modal-define-kbd "J" "C-M-j" "indent-new-comment-line")
(modal-define-kbd "K" "C-M-k" "kill-sexp")
(modal-define-kbd "N" "C-M-n" "forward-list")
(modal-define-kbd "L" "C-M-l" "reposition-window")
(modal-define-kbd "O" "C-M-o" "split-line")
(modal-define-kbd "P" "C-M-p" "backward-list")
(define-key modal-mode-map "Q" "C-M-q")  ;; prog-indent-sexp
(modal-define-kbd "R" "C-M-r" "isearch-backward-regexp")
(modal-define-kbd "S" "C-M-s" "isearch-forward-regexp")
(modal-define-kbd "T" "C-M-t" "transpose-sexps" t)
(modal-define-kbd "U" "C-M-u" "backward-up-list")
(modal-define-kbd "V" "C-M-v" "scroll-other-window")
(modal-define-kbd "W" "C-M-w" "append-next-kill")
(modal-define-kbd "X" "C-M-x" "eval-defun")
;; Y - [ command prefix
(define-key modal-mode-map (kbd "Y h") #'sp-kill-hybrid-sexp)
(modal-define-kbd "Y l" "<C-S-backspace>")
(define-key modal-mode-map (kbd "Y s") #'kill-to-end-of-sexp)
(define-key modal-mode-map (kbd "Y S") #'kill-to-begin-of-sexp)
;; Y - ] command prefix
;; Z - [ command prefix
;; Z - ] command prefix

(define-key modal-mode-map (kbd "M-Q") #'fill-paragraph)
(modal-define-kbd "M-V" "C-M-S-v" "scroll-other-window-down")

;;;;;;;;;;;;;;;;;;;
;; new quit bind ;;
;;;;;;;;;;;;;;;;;;;
(define-key query-replace-map "\M-q" 'quit)          ;; read-event
(define-key function-key-map "\M-q" "\C-g")          ;; read-key
(define-key isearch-mode-map "\M-q" 'isearch-abort)  ;; isearch-mode
;; minibuffer keys
(define-key minibuffer-local-map "\M-q" 'abort-recursive-edit)
(define-key minibuffer-local-ns-map "\M-q" 'abort-recursive-edit)
(define-key minibuffer-local-isearch-map "\M-q" 'abort-recursive-edit)
(define-key minibuffer-local-completion-map "\M-q" 'abort-recursive-edit)
(define-key minibuffer-local-must-match-map "\M-q" 'abort-recursive-edit)
(define-key minibuffer-local-filename-completion-map "\M-q" 'abort-recursive-edit)
(global-set-key "\M-q" 'keyboard-quit)

;;;;;;;;;;;;;;;;;;;;;;
;; Globally enabled ;;
;;;;;;;;;;;;;;;;;;;;;;
(modal-global-mode 1)


(provide 'modal-config)
;;; modal-config.el ends here
