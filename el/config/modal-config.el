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
                             docker-container-mode)
      modal-insertp-functions '(sp-rewrap-sexp-lc
                                sp-unwrap-sexp-lc
                                comment-dwim
                                undo-tree-undo
                                undo-tree-redo
                                query-replace
                                duplicate-current-line-or-region
                                delete-forward-char
                                electric-newline-and-maybe-indent
                                kill-line
                                newline
                                open-line
                                quoted-insert
                                transpose-chars
                                kill-region
                                transpose-sexps))


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
;; ' (handy as self-inserting symbol)
;; " (handy as self-inserting symbol)
(modal-define-key (kbd "(") (kbd "C-("))  ;; sp-rewrap-sexp-lc
(modal-define-key (kbd ")") (kbd "C-)"))  ;; sp-unwrap-sexp-lc
(modal-define-key "&" #'rotate-or-inflection)
(modal-define-key "?" #'goto-last-change-reverse)
(modal-define-key "/" #'goto-last-change)
;; (modal-define-kbd "." "M-." "definition-at-point")
(modal-define-key (kbd ";") (kbd "M-;"))  ;; comment-dwim
(modal-define-key (kbd ":") (kbd "M-:"))  ;; eval-expression
(modal-define-key "+" #'fold-dwim)
(modal-define-key (kbd "-") (kbd "C-_"))  ;; undo-tree-undo
(modal-define-key (kbd "_") (kbd "M-_"))  ;; undo-tree-redo
(modal-define-key (kbd "%") (kbd "M-%"))  ;; query-replace
(modal-define-key (kbd "*") (kbd "C-*"))  ;; duplicate-current-line-or-region
(modal-define-key (kbd "<") (kbd "M-<"))  ;; beginning-of-buffer
(modal-define-key (kbd ">") (kbd "M->"))  ;; end-of-buffer
(modal-define-key (kbd "SPC") (kbd "C-SPC"))  ;; set-mark-command

(modal-define-key (kbd "0") (kbd "C-0"))  ;; prefix-0
(modal-define-key (kbd "1") (kbd "C-1"))  ;; prefix-1
(modal-define-key (kbd "2") (kbd "C-2"))  ;; prefix-2
(modal-define-key (kbd "3") (kbd "C-3"))  ;; prefix-3
(modal-define-key (kbd "4") (kbd "C-4"))  ;; prefix-4
(modal-define-key (kbd "5") (kbd "C-5"))  ;; prefix-5
(modal-define-key (kbd "6") (kbd "C-6"))  ;; prefix-6
(modal-define-key (kbd "7") (kbd "C-7"))  ;; prefix-7
(modal-define-key (kbd "8") (kbd "C-8"))  ;; prefix-8
(modal-define-key (kbd "9") (kbd "C-9"))  ;; prefix-9

(modal-define-key (kbd "a") (kbd "C-a"))  ;; move-beginning-of-line
(modal-define-key (kbd "b") (kbd "C-b"))  ;; backward-char
(modal-define-key "\M-b" 'backward-word)
;; c - [ command prefix
(modal-define-key (kbd "c TAB") (kbd "C-c TAB"))
(modal-define-key (kbd "c <backtab>") (kbd "C-c <backtab>"))
(modal-define-key (kbd "c '") (kbd "C-c '"))
(modal-define-key (kbd "c ! c") #'flycheck-buffer)
(modal-define-key (kbd "c ! n") #'flycheck-next-error)
(modal-define-key (kbd "c ! p") #'flycheck-previous-error)
(modal-define-key (kbd "c ! l") #'flycheck-list-errors)
(modal-define-key (kbd "c a") (kbd "C-c C-a"))
(modal-define-key (kbd (kbd "c A")) (kbd (kbd "C-c a")))  ;; org-agenda
(modal-define-key (kbd "c b") (kbd "C-c C-b"))  ;; go-back
(modal-define-key (kbd "c M-b") (kbd "C-c M-b"))  ;; org-previous-block
(modal-define-key (kbd (kbd "c c")) (kbd (kbd "C-c C-c")))  ;; confirm-commit
(modal-define-key (kbd "c e w") #'er/mark-word)
(modal-define-key (kbd "c e s") #'er/mark-symbol)
(modal-define-key (kbd "c e c") #'er/mark-method-call)
(modal-define-key (kbd "c e q") #'er/mark-inside-quotes)
(modal-define-key (kbd "c e Q") #'er/mark-outside-quotes)
(modal-define-key (kbd "c e p") #'er/mark-inside-pairs)
(modal-define-key (kbd "c e P") #'er/mark-outside-pairs)
(modal-define-key (kbd "c f") (kbd "C-c C-f"))  ;; org-forward-heading-same-level
(modal-define-key (kbd "c M-f") (kbd "C-c M-f"))  ;; org-next-block
(modal-define-key (kbd "c i s") #'spanish-dictionary)
(modal-define-key (kbd "c i e") #'english-dictionary)
(modal-define-key (kbd "c i c") #'flyspell-buffer)
(modal-define-key (kbd "c i n") #'flyspell-goto-next-error)
(modal-define-key (kbd "c i p") #'flyspell-goto-previous-error)
(modal-define-key (kbd "c i a") #'flyspell-auto-correct-word)
(modal-define-key (kbd (kbd "c k")) (kbd (kbd "C-c C-k")))  ;; cancel-commit
(modal-define-key (kbd "c m p") #'mc/mark-previous-like-this)
(modal-define-key (kbd "c m n") #'mc/mark-next-like-this)
(modal-define-key (kbd "c m a") #'mc/mark-all-like-this-dwim)
(modal-define-key (kbd "c n") (kbd "C-c C-n"))  ;; smartscan-symbol-go-forward or org-next-visible-heading
(modal-define-key (kbd "c o") #'operate-on-number-at-point-or-region)
(modal-define-key (kbd "c p") (kbd "C-c C-p"))  ;; smartscan-symbol-go-backward or org-previous-visible-heading
(modal-define-key (kbd "c q") (kbd "C-c C-q"))
(modal-define-key (kbd "c r") (kbd "C-c M-s"))  ;; org-sort-entries-user-defined
(modal-define-key (kbd "c R") #'revert-buffer)
(modal-define-key (kbd (kbd "c s")) (kbd (kbd "C-c C-s")))  ;; org-schedule
(modal-define-key (kbd (kbd "c t")) (kbd (kbd "C-c C-t")))  ;; org-todo
(modal-define-key (kbd "c u") (kbd "C-c C-u"))  ;; outline-up-heading
(modal-define-key (kbd "c v *") #'vimish-fold-unfold-all)
(modal-define-key (kbd "c v +") #'vimish-fold-unfold)
(modal-define-key (kbd "c v -") #'vimish-fold-refold)
(modal-define-key (kbd "c v _") #'vimish-fold-refold-all)
(modal-define-key (kbd "c v .") #'vimish-fold-toggle)
(modal-define-key (kbd "c v :") #'vimish-fold-toggle-all)
(modal-define-key (kbd "c v d") #'vimish-fold-delete)
(modal-define-key (kbd "c v D") #'vimish-fold-delete-all)
(modal-define-key (kbd "c v f") #'vimish-fold)
(modal-define-key (kbd "c v G") #'vimish-fold-avy)
(modal-define-key (kbd "c V s") (kbd "C-c v s"))  ;; org-block-and-result-show-all
(modal-define-key (kbd "c V h") (kbd "C-c v h"))  ;; org-block-and-result-hide-all
(modal-define-key (kbd "c w -") #'winner-undo)
(modal-define-key (kbd "c w _") #'winner-redo)
(modal-define-key (kbd "c w 2") 'shell-2-window-frame)
(modal-define-key (kbd "c w 3") 'shell-3-window-frame)
(modal-define-key (kbd "c w a") #'toggle-hscroll-aggressive)
(modal-define-key (kbd "c w d a") #'window-dedicate-all)
(modal-define-key (kbd "c w d t") #'window-dedicate-this)
(modal-define-key (kbd "c w H") #'window-resize-height)
(modal-define-key (kbd "c w h") #'flop-frame)
(modal-define-key (kbd "c w o") #'halve-other-window-height)
(modal-define-key (kbd "c w R") #'rotate-frame-anticlockwise)
(modal-define-key (kbd "c w r") #'rotate-frame-clockwise)
(modal-define-key (kbd "c w t") #'transpose-frame)
(modal-define-key (kbd "c w u a") #'window-undedicate-all)
(modal-define-key (kbd "c w u t") #'window-undedicate-this)
(modal-define-key (kbd "c w v") #'flip-frame)
(modal-define-key (kbd "c w W") #'window-resize-width)
(modal-define-key (kbd "c x n") (kbd "C-c C-x C-n"))  ;; org-next-link
(modal-define-key (kbd "c x p") (kbd "C-c C-x C-p"))  ;; org-previous-link
(modal-define-key (kbd "c x s") (kbd "C-c C-x C-s"))  ;; org-archive-subtree
;; c - ] command prefix
(modal-define-key (kbd "d") (kbd "<deletechar>"))  ;; delete-forward-char
(modal-define-key (kbd "e") (kbd "C-e"))  ;; move-end-of-line
(modal-define-key (kbd "f") (kbd "C-f"))  ;; forward-char
(modal-define-key "\M-f" 'forward-word)
(modal-define-key (kbd "g") 'modal-global-mode-disable-with-bind)
;; h - [ prefix
(modal-define-key (kbd "h b") #'describe-bindings)
(modal-define-key (kbd "h e") #'view-echo-area-messages)
(modal-define-key (kbd "h f") #'describe-function)
(modal-define-key (kbd "h k") #'describe-key)
(modal-define-key (kbd "h L") #'describe-language-environment)
(modal-define-key (kbd "h m") #'describe-mode)
(modal-define-key (kbd "h o") #'describe-symbol)
(modal-define-key (kbd "h P") #'describe-package)
(modal-define-key (kbd "h s") #'describe-syntax)
(modal-define-key (kbd "h v") #'describe-variable)
;; h - ] prefix
(modal-define-key (kbd "i") (kbd "C-i"))  ;; indent-for-tab-command
(modal-define-key (kbd "j") (kbd "C-j"))  ;; electric-newline-and-maybe-indent
(modal-define-key (kbd "k") (kbd "C-k"))  ;; kill-line
(modal-define-key (kbd "l") (kbd "C-l"))  ;; recenter-top-bottom
(modal-define-key (kbd "m") (kbd "C-m"))  ;; newline
(modal-define-key (kbd "n") (kbd "C-n"))  ;; next-line
(modal-define-key (kbd "o") (kbd "C-o"))  ;; open-line
(modal-define-key (kbd "p") (kbd "C-p"))  ;; previous-line
(modal-define-key (kbd "q") (kbd "C-q"))  ;; quoted-insert
(modal-define-key (kbd "r") (kbd "C-r"))  ;; isearch-backward
(modal-define-key (kbd "s") (kbd "C-s"))  ;; isearch-forward
(modal-define-key (kbd "t") (kbd "C-t"))  ;; transpose-chars
;; u - reserved
(modal-define-key (kbd "v") (kbd "C-v"))  ;; scroll-up-command
(modal-define-key (kbd "w") (kbd "C-w"))  ;; kill-region
;; x - [ command prefix
(modal-define-key (kbd "x TAB") (kbd "C-x TAB"))  ;; indent-rigidly
(modal-define-key (kbd "x RET") (kbd "C-x C-o"))  ;; delete-blank-lines
(modal-define-key (kbd "x SPC") (kbd "C-x C-SPC"))  ;; pop-global-mark
(modal-define-key (kbd "x S-SPC") (kbd "C-x SPC"))  ;; rectangle-mark-mode
(modal-define-key (kbd "x -") (kbd "C-x -"))  ;; shrink-window-if-larger-than-buffer
(modal-define-key (kbd "x <") (kbd "C-x <"))  ;; scroll-left
(modal-define-key (kbd "x >") (kbd "C-x >"))  ;; scroll-right
(modal-define-key (kbd "x ;") (kbd "C-x C-;"))  ;; comment-line
(modal-define-key (kbd "x (") (kbd "C-x ("))  ;; kmacro-start-macro
(modal-define-key (kbd "x )") (kbd "C-x )"))  ;; kmacro-end-macro
(modal-define-key (kbd "x ^") (kbd "C-x ^"))  ;; enlarge-window
(modal-define-key (kbd "x {") (kbd "C-x {"))  ;; shrink-window-horizontally
(modal-define-key (kbd "x }") (kbd "C-x }"))  ;; enlarge-window-horizontally
(modal-define-key (kbd "x +") (kbd "C-x +"))  ;; balance-windows
(modal-define-key (kbd "x 0") (kbd "C-x 0"))  ;; delete-window
(modal-define-key (kbd "x 1") (kbd "C-x 1"))  ;; delete-other-windows
(modal-define-key (kbd "x 2") (kbd "C-x 2"))  ;; vsplit-last-buffer
(modal-define-key (kbd "x 3") (kbd "C-x 3"))  ;; hsplit-last-buffer
(modal-define-key (kbd "x 4 f") (kbd "C-x 4 C-f"))  ;; ido-find-file-other-window
(modal-define-key (kbd "x 4 b") (kbd "C-x 4 b"))  ;; ido-switch-buffer-other-window
(modal-define-key (kbd "x 5 f") (kbd "C-x 5 C-f"))  ;; ido-find-file-other-frame
(modal-define-key (kbd "x 5 b") (kbd "C-x 5 b"))  ;; ido-switch-buffer-other-frame
(modal-define-key (kbd "x c") (kbd "C-x C-c"))  ;; save-buffers-kill-emacs
(modal-define-key (kbd "x b") (kbd "C-x b"))  ;; switch-buffer
(modal-define-key (kbd "x e") (kbd "C-x C-e"))  ;; eval-last-sexp
(modal-define-key (kbd "x E") (kbd "C-x e"))  ;; kmacro-end-and-call-macro
(modal-define-key (kbd "x f") (kbd "C-x C-f"))  ;; find-file
(modal-define-key (kbd "x F") (kbd "C-x f"))  ;; find-file-at-point
(modal-define-key (kbd "x H") (kbd "C-x h"))  ;; mark-whole-buffer
(modal-define-key (kbd "x k i") #'select-kbd-macro)
(modal-define-key (kbd "x k k") (kbd "C-x C-k C-k"))  ;; kmacro-end-or-call-macro-repeat
(modal-define-key (kbd "x k n") (kbd "C-x C-k C-n"))  ;; kmacro-cycle-ring-next
(modal-define-key (kbd "x k N") (kbd "C-x C-k n"))  ;; kmacro-name-last-macro
(modal-define-key (kbd "x k p") (kbd "C-x C-k C-p"))  ;; kmacro-cycle-ring-previous
(modal-define-key (kbd "x k v") (kbd "C-x C-k C-v"))  ;; kmacro-view-macro-repeat
(modal-define-key (kbd "x K") (kbd "C-x k"))  ;; kill-buffer
(modal-define-key (kbd "x l") (kbd "C-x C-l"))  ;; downcase-region
(modal-define-key (kbd "x M-l") (kbd "C-x M-l"))  ;; recenter-horizontal
(modal-define-key (kbd "x o") (kbd "C-x o"))  ;; other-window
(modal-define-key (kbd "x O") #'ff-find-other-file)
(modal-define-key (kbd "x p") (kbd "C-x C-p"))  ;; mark-page
(modal-define-key (kbd "x P s") (kbd "C-x p s"))  ;; bookmark-save
(modal-define-key (kbd "x r") (kbd "C-x C-r"))  ;; recentf-open
(modal-define-key (kbd "x R m") (kbd "C-x r m"))  ;; bookmark-set
(modal-define-key (kbd "x R b") (kbd "C-x r b"))  ;; bookmark-jump
(modal-define-key (kbd "x R l") (kbd "C-x r l"))  ;; list-bookmarks
(modal-define-key (kbd "x s") (kbd "C-x C-s"))  ;; save-buffer
(modal-define-key (kbd "x S") (kbd "C-x s"))  ;; save-some-buffers
(modal-define-key (kbd "x u") (kbd "C-x C-u"))  ;; upcase-region
(modal-define-key (kbd "x v =") #'magit-diff)
(modal-define-key (kbd "x v b") #'magit-branch)
(modal-define-key (kbd "x v c") #'magit-checkout)
(modal-define-key (kbd "x v L") #'vc-print-root-log)
(modal-define-key (kbd "x v l") #'vc-print-log)
(modal-define-key (kbd "x v m") #'hydra-smerge/body)
(modal-define-key (kbd "x v P") #'magit-push)
(modal-define-key (kbd "x v p") #'magit-pull)
(modal-define-key (kbd "x v R") #'magit-rebase-continue)
(modal-define-key (kbd "x v r") #'magit-rebase)
(modal-define-key (kbd "x v v") #'magit-status)
(modal-define-key (kbd "x x") (kbd "C-x C-x"))  ;; exchange-point-and-mark
(modal-define-key (kbd "x z") (kbd "C-x z"))  ;; repeat
;; x - ] command prefix
(modal-define-key (kbd "y") (kbd "C-y"))  ;; yank
(modal-define-key "z" #'avy-goto-char-timer)

(modal-define-key (kbd "S-\\") (kbd "C-M-\\"))  ;; indent-region
(modal-define-key (kbd "S-@") (kbd "C-M-@"))  ;; mark-sexp
(modal-define-key (kbd "A") (kbd "C-M-a"))  ;; beginning-of-defun
(modal-define-key (kbd "B") (kbd "C-M-b"))  ;; backward-sexp
(modal-define-key (kbd "C") (kbd "C-M-c"))  ;; exit-recursive-edit
(modal-define-key (kbd "D") (kbd "C-M-d"))  ;; down-list
(modal-define-key (kbd "E") (kbd "C-M-e"))  ;; end-of-defun
(modal-define-key (kbd "F") (kbd "C-M-f"))  ;; forward-sexp
;; G - [ command prefix
;; G - ] command prefix
(modal-define-key (kbd "H") (kbd "C-M-h"))  ;; mark-defun
(modal-define-key (kbd "I") (kbd "C-M-i"))  ;; completion-at-point/flyspell-auto-correct-word
(modal-define-key (kbd "J") (kbd "C-M-j"))  ;; indent-new-comment-line
(modal-define-key (kbd "K") (kbd "C-M-k"))  ;; kill-sexp
(modal-define-key (kbd "N") (kbd "C-M-n"))  ;; forward-list
(modal-define-key (kbd "L") (kbd "C-M-l"))  ;; reposition-window
(modal-define-key (kbd "O") (kbd "C-M-o"))  ;; split-line
(modal-define-key (kbd "P") (kbd "C-M-p"))  ;; backward-list
(modal-define-key "Q" "C-M-q")  ;; prog-indent-sexp
(modal-define-key (kbd "R") (kbd "C-M-r"))  ;; isearch-backward-regexp
(modal-define-key (kbd "S") (kbd "C-M-s"))  ;; isearch-forward-regexp
(modal-define-key (kbd "T") (kbd "C-M-t"))  ;; transpose-sexps
(modal-define-key (kbd "U") (kbd "C-M-u"))  ;; backward-up-list
(modal-define-key (kbd "V") (kbd "C-M-v"))  ;; scroll-other-window
(modal-define-key (kbd "W") (kbd "C-M-w"))  ;; append-next-kill
(modal-define-key (kbd "X") (kbd "C-M-x"))  ;; eval-defun
;; Y - [ command prefix
(modal-define-key (kbd "Y h") #'sp-kill-hybrid-sexp)
(modal-define-key (kbd "Y l") (kbd "<C-S-backspace>"))
(modal-define-key (kbd "Y s") #'kill-to-end-of-sexp)
(modal-define-key (kbd "Y S") #'kill-to-begin-of-sexp)
;; Y - ] command prefix
;; Z - [ command prefix
;; Z - ] command prefix

(modal-define-key (kbd "M-%") (kbd "C-M-%"))
(modal-define-key (kbd "M-Q") #'fill-paragraph)
(modal-define-key (kbd "M-V") (kbd "C-M-S-v"))  ;; scroll-other-window-down

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
