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
                             ;; package-menu-mode
                             ;; debugger-mode
                             ;; dired-mode
                             ediff-mode
                             ;; eshell-mode
                             ;; org-agenda-mode
                             git-rebase-mode
                             ;; docker-container-mode
                             magit-popup-mode
                             ;; magit-mode
                             ;; magit-process-mode
                             ;; magit-status-mode
                             calc-mode)
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
;; transient
(with-eval-after-load 'transient
  (if modal-mode
      (define-key transient-map "G" 'transient-quit-one)))
;; magit-popup
(with-eval-after-load 'magit-popup
  (if modal-mode
      (define-key magit-popup-mode-map "G" 'magit-popup-quit)))

(defun modal-mode-bind-higher-priority-maps ()
  (if modal-mode
      (progn
        ;; read-key
        (define-key function-key-map "G" "\C-g")
        ;; universal-argument
        (define-key universal-argument-map "U" #'universal-argument-more)
        ;; indent-rigidly
        (define-key indent-rigidly-map "F" #'indent-rigidly-right-to-tab-stop)
        (define-key indent-rigidly-map "B" #'indent-rigidly-left-to-tab-stop)
        (define-key indent-rigidly-map "f" #'indent-rigidly-right)
        (define-key indent-rigidly-map "b" #'indent-rigidly-left)
        ;; transient
        (when (boundp 'transient-map)
          (define-key transient-map "G" 'transient-quit-one))
        ;; magit-popup
        (when (boundp 'magit-popup-mode-map)
          (define-key magit-popup-mode-map "G" 'magit-popup-quit)))
    ;; read-key
    (define-key function-key-map "G" nil)
    ;; universal-argument
    (define-key universal-argument-map "U" nil)
    ;; indent-rigidly
    (define-key indent-rigidly-map "F" nil)
    (define-key indent-rigidly-map "B" nil)
    (define-key indent-rigidly-map "f" nil)
    (define-key indent-rigidly-map "b" nil)
    ;; transient
    (when (boundp 'transient-map)
      (define-key transient-map "G" nil))
    ;; magit-popup
    (when (boundp 'magit-popup-mode-map)
          (define-key magit-popup-mode-map "G" nil))))
(add-hook 'modal-mode-hook 'modal-mode-bind-higher-priority-maps)

;; Modal editing
;; ' (handy as self-inserting symbol)
;; " (handy as self-inserting symbol)
(modal-define-key (kbd "M (") #'sp-rewrap-sexp-lc)
(modal-define-key (kbd "M )") #'sp-unwrap-sexp-lc)
(modal-define-key (kbd "M &") #'rotate-or-inflection)
(modal-define-key (kbd "M ?") #'goto-last-change-reverse)
(modal-define-key (kbd "M /") #'goto-last-change)
;; (modal-define-kbd "." "M-." "definition-at-point")
;; (modal-define-key (kbd ";") (kbd "M-;"))  ;; comment-dwim
;; (modal-define-key (kbd ":") (kbd "M-:"))  ;; eval-expression
(modal-define-key (kbd "M +") #'fold-dwim)
(modal-define-key (kbd "M _") (kbd "C-_"))  ;; undo-tree-undo
;; (modal-define-key (kbd "_") (kbd "M-_"))  ;; undo-tree-redo
;; (modal-define-key (kbd "%") (kbd "M-%"))  ;; query-replace
(modal-define-key (kbd "M *") (kbd "C-*"))  ;; duplicate-current-line-or-region
;; (modal-define-key (kbd "<") (kbd "M-<"))  ;; beginning-of-buffer
;; (modal-define-key (kbd ">") (kbd "M->"))  ;; end-of-buffer
(modal-define-key (kbd "M %") (kbd "C-M-%")) ;; query-replace-regexp
(modal-define-key (kbd "M k h") #'sp-kill-hybrid-sexp)
(modal-define-key (kbd "M k l") (kbd "<C-S-backspace>")) ;; kill-whole-line
(modal-define-key (kbd "M k s") #'kill-to-end-of-sexp)
(modal-define-key (kbd "M k S") #'kill-to-begin-of-sexp)
;; (modal-define-key (kbd "S-SPC") (kbd "C-SPC"))  ;; set-mark-command

(modal-define-key (kbd "A") (kbd "C-a"))  ;; move-beginning-of-line
(modal-define-key (kbd "B") (kbd "C-b"))  ;; backward-char
;; (modal-define-key "\M-b" 'backward-word)
;; c - [ command prefix
(modal-define-key (kbd "C %") (kbd "C-c %"))  ;; org-mark-ring-push
(modal-define-key (kbd "C &") (kbd "C-c &"))  ;; org-mark-ring-goto
(modal-define-key (kbd "C TAB") (kbd "C-c TAB"))  ;; org-show-subtree
(modal-define-key (kbd "C <backtab>") (kbd "C-c <backtab>"))  ;; org-show-all
(modal-define-key (kbd "C '") (kbd "C-c '"))
;; [ equivalent
(modal-define-key (kbd "C !") (kbd "C-c !"))
;; (modal-define-key (kbd "C ! c") #'flycheck-buffer)
;; (modal-define-key (kbd "C ! n") #'flycheck-next-error)
;; (modal-define-key (kbd "C ! p") #'flycheck-previous-error)
;; (modal-define-key (kbd "C ! l") #'flycheck-list-errors)
;; ]
(modal-define-key (kbd "C ,") (kbd "C-c ,"))
(modal-define-key (kbd "C M ,") (kbd "C-c C-,"))  ;; org-insert-structure-template
(modal-define-key (kbd "C A") (kbd "C-c C-a"))
(modal-define-key (kbd "C a") (kbd "C-c a"))  ;; org-agenda
(modal-define-key (kbd "C B") (kbd "C-c C-b"))  ;; go-back
(modal-define-key (kbd "C M-b") (kbd "C-c M-b"))  ;; org-previous-block
(modal-define-key (kbd "C C") (kbd "C-c C-c"))  ;; confirm-commit
(modal-define-key (kbd "C e w") #'er/mark-word)
(modal-define-key (kbd "C e s") #'er/mark-symbol)
(modal-define-key (kbd "C e c") #'er/mark-method-call)
(modal-define-key (kbd "C e q") #'er/mark-inside-quotes)
(modal-define-key (kbd "C e Q") #'er/mark-outside-quotes)
(modal-define-key (kbd "C e p") #'er/mark-inside-pairs)
(modal-define-key (kbd "C e P") #'er/mark-outside-pairs)
(modal-define-key (kbd "C F") (kbd "C-c C-f"))  ;; org-forward-heading-same-level
(modal-define-key (kbd "C M-f") (kbd "C-c M-f"))  ;; org-next-block
;; [ equivalent
(modal-define-key (kbd "C i") (kbd "C-c i"))
;; (modal-define-key (kbd "C i s") #'spanish-dictionary)
;; (modal-define-key (kbd "C i e") #'english-dictionary)
;; (modal-define-key (kbd "C i c") #'flyspell-buffer)
;; (modal-define-key (kbd "C i n") #'flyspell-goto-next-error)
;; (modal-define-key (kbd "C i p") #'flyspell-goto-previous-error)
;; (modal-define-key (kbd "C i a") #'flyspell-auto-correct-word)
;; ]
(modal-define-key (kbd "C K") (kbd "C-c C-k"))  ;; cancel-commit
(modal-define-key (kbd "C l") (kbd "C-c l"))  ;; org-store-link
(modal-define-key (kbd "C L") (kbd "C-c C-l"))  ;; org-insert-link
(modal-define-key (kbd "C m p") #'mc/mark-previous-like-this)
(modal-define-key (kbd "C m n") #'mc/mark-next-like-this)
(modal-define-key (kbd "C m a") #'mc/mark-all-like-this-dwim)
(modal-define-key (kbd "C N") (kbd "C-c C-n"))  ;; smartscan-symbol-go-forward or org-next-visible-heading
(modal-define-key (kbd "C o") #'operate-on-number-at-point-or-region)
(modal-define-key (kbd "C O") (kbd "C-c C-o"))  ;; org-open-at-point
(modal-define-key (kbd "C P") (kbd "C-c C-p"))  ;; smartscan-symbol-go-backward or org-previous-visible-heading
(modal-define-key (kbd "C Q") (kbd "C-c C-q"))
(modal-define-key (kbd "C M-s") (kbd "C-c M-s"))  ;; org-sort-entries-user-defined
(modal-define-key (kbd "C M-r") #'revert-buffer)
(modal-define-key (kbd "C S") (kbd "C-c C-s"))  ;; org-schedule
(modal-define-key (kbd "C D") (kbd "C-c C-d"))  ;; org-deadline
(modal-define-key (kbd "C T") (kbd "C-c C-t"))  ;; org-todo
(modal-define-key (kbd "C U") (kbd "C-c C-u"))  ;; outline-up-heading
;; [ equivalent
(modal-define-key (kbd "C v") (kbd "C-c v"))
;; (modal-define-key (kbd "C v *") #'vimish-fold-unfold-all)
;; (modal-define-key (kbd "C v +") #'vimish-fold-unfold)
;; (modal-define-key (kbd "C v -") #'vimish-fold-refold)
;; (modal-define-key (kbd "C v _") #'vimish-fold-refold-all)
;; (modal-define-key (kbd "C v .") #'vimish-fold-toggle)
;; (modal-define-key (kbd "C v :") #'vimish-fold-toggle-all)
;; (modal-define-key (kbd "C v d") #'vimish-fold-delete)
;; (modal-define-key (kbd "C v D") #'vimish-fold-delete-all)
;; (modal-define-key (kbd "C v f") #'vimish-fold)
;; (modal-define-key (kbd "C v G") #'vimish-fold-avy)
;; (modal-define-key (kbd "C v p") #'vimish-fold-previous-fold)
;; (modal-define-key (kbd "C v n") #'vimish-fold-next-fold)
;; (modal-define-key (kbd "C v s") (kbd "C-c v s"))  ;; org-block-and-result-show-all
;; (modal-define-key (kbd "C v h") (kbd "C-c v h"))  ;; org-block-and-result-hide-all
;; ]
(modal-define-key (kbd "C w -") #'winner-undo)
(modal-define-key (kbd "C w _") #'winner-redo)
(modal-define-key (kbd "C w 2") 'shell-2-window-frame)
(modal-define-key (kbd "C w 3") 'shell-3-window-frame)
(modal-define-key (kbd "C w a") #'toggle-hscroll-aggressive)
(modal-define-key (kbd "C w B") #'windmove-left)
(modal-define-key (kbd "C w d a") #'window-dedicate-all)
(modal-define-key (kbd "C w d t") #'window-dedicate-this)
(modal-define-key (kbd "C V j") (kbd "C-c C-v j"))  ;; org-babel-insert-header-arg
(modal-define-key (kbd "C V J") (kbd "C-c C-v C-j"))  ;; org-babel-insert-header-arg
(modal-define-key (kbd "C V k") (kbd "C-c C-v k"))  ;; org-babel-remove-result
(modal-define-key (kbd "C V K") (kbd "C-c C-v C-k"))  ;; org-babel-remove-result
(modal-define-key (kbd "C V o") (kbd "C-c C-v o"))  ;; org-babel-open-src-block-result
(modal-define-key (kbd "C V O") (kbd "C-c C-v C-o"))  ;; org-babel-open-src-block-result
(modal-define-key (kbd "C V p") (kbd "C-c C-v p"))  ;; org-babel-previous-src-block
(modal-define-key (kbd "C V P") (kbd "C-c C-v C-p"))  ;; org-babel-previous-src-block
(modal-define-key (kbd "C V n") (kbd "C-c C-v n"))  ;; org-babel-next-src-block
(modal-define-key (kbd "C V N") (kbd "C-c C-v C-n"))  ;; org-babel-next-src-block
(modal-define-key (kbd "C w F") #'windmove-right)
(modal-define-key (kbd "C w H") #'window-resize-height)
(modal-define-key (kbd "C w h") #'flop-frame)
(modal-define-key (kbd "C w N") #'windmove-down)
(modal-define-key (kbd "C w o") #'halve-other-window-height)
(modal-define-key (kbd "C w P") #'windmove-up)
(modal-define-key (kbd "C w R") #'rotate-frame-anticlockwise)
(modal-define-key (kbd "C w r") #'rotate-frame-clockwise)
(modal-define-key (kbd "C w t") #'transpose-frame)
(modal-define-key (kbd "C w u a") #'window-undedicate-all)
(modal-define-key (kbd "C w u t") #'window-undedicate-this)
(modal-define-key (kbd "C w v") #'flip-frame)
(modal-define-key (kbd "C w W") #'window-resize-width)
(modal-define-key (kbd "C X D") (kbd "C-c C-x C-d"))  ;; org-clock-display
(modal-define-key (kbd "C X E") (kbd "C-c C-x C-e"))  ;; org-clock-modify-effort-estimate
(modal-define-key (kbd "C X I") (kbd "C-c C-x C-i"))  ;; org-clock-in
(modal-define-key (kbd "C X J") (kbd "C-c C-x C-j"))  ;; org-clock-goto
(modal-define-key (kbd "C X K") (kbd "C-c C-x C-k"))  ;; org-toggle-link-display
(modal-define-key (kbd "C X N") (kbd "C-c C-x C-n"))  ;; org-next-link
(modal-define-key (kbd "C X O") (kbd "C-c C-x C-o"))  ;; org-clock-out
(modal-define-key (kbd "C X P") (kbd "C-c C-x C-p"))  ;; org-previous-link
(modal-define-key (kbd "C X Q") (kbd "C-c C-x C-q"))  ;; org-clock-cancel
(modal-define-key (kbd "C X S") (kbd "C-c C-x C-s"))  ;; org-archive-subtree
(modal-define-key (kbd "C X V") (kbd "C-c C-x C-v"))  ;; org-toggle-inline-images
(modal-define-key (kbd "C X X") (kbd "C-c C-x C-x"))  ;; org-clock-in-last
;; c - ] command prefix
(modal-define-key (kbd "C") (kbd "C-c"))  ;; ido-case-fold
(modal-define-key (kbd "D") (kbd "<deletechar>"))  ;; delete-forward-char
(modal-define-key (kbd "E") (kbd "C-e"))  ;; move-end-of-line
(modal-define-key (kbd "F") (kbd "C-f"))  ;; forward-char
;; (modal-define-key "\M-f" 'forward-word)
(modal-define-key (kbd "G") (kbd "C-g"))
;; h - [ prefix
;; [ equivalent
(modal-define-key (kbd "H") (kbd "C-h"))
;; (modal-define-key (kbd "H b") #'describe-bindings)
;; (modal-define-key (kbd "H e") #'view-echo-area-messages)
;; (modal-define-key (kbd "H f") #'describe-function)
;; (modal-define-key (kbd "H k") #'describe-key)
;; (modal-define-key (kbd "H L") #'describe-language-environment)
;; (modal-define-key (kbd "H m") #'describe-mode)
;; (modal-define-key (kbd "H o") #'describe-symbol)
;; (modal-define-key (kbd "H P") #'describe-package)
;; (modal-define-key (kbd "H s") #'describe-syntax)
;; (modal-define-key (kbd "H v") #'describe-variable)
;; ]
;; h - ] prefix
(modal-define-key (kbd "I") (kbd "C-i"))  ;; indent-for-tab-command
(modal-define-key (kbd "J") (kbd "C-j"))  ;; electric-newline-and-maybe-indent
(modal-define-key (kbd "K") (kbd "C-k"))  ;; kill-line
(modal-define-key (kbd "L") (kbd "C-l"))  ;; recenter-top-bottom
;; (modal-define-key (kbd "M") (kbd "C-m"))  ;; newline
(modal-define-key (kbd "N") (kbd "C-n"))  ;; next-line
(modal-define-key (kbd "Ñ") #'find-next-unsafe-char)
(modal-define-key (kbd "O") (kbd "C-o"))  ;; open-line
(modal-define-key (kbd "P") (kbd "C-p"))  ;; previous-line
(modal-define-key (kbd "Q") (kbd "C-q"))  ;; quoted-insert
(modal-define-key (kbd "R") (kbd "C-r"))  ;; isearch-backward
(modal-define-key (kbd "S") (kbd "C-s"))  ;; isearch-forward
(modal-define-key (kbd "T") (kbd "C-t"))  ;; transpose-chars
(modal-define-key (kbd "U") (kbd "C-u"))  ;; universal-argument
(modal-define-key (kbd "V") (kbd "C-v"))  ;; scroll-up-command
(modal-define-key (kbd "W") (kbd "C-w"))  ;; kill-region
;; x - [ command prefix
(modal-define-key (kbd "X #") (kbd "C-x #"))  ;; server-edit
(modal-define-key (kbd "X TAB") (kbd "C-x TAB"))  ;; indent-rigidly
(modal-define-key (kbd "X <backtab>") (kbd "C-x <C-tab>"))  ;; align-regexp
(modal-define-key (kbd "X RET") (kbd "C-x C-o"))  ;; delete-blank-lines
(modal-define-key (kbd "X S-SPC") (kbd "C-x C-SPC"))  ;; pop-global-mark
(modal-define-key (kbd "X SPC") (kbd "C-x SPC"))  ;; rectangle-mark-mode
(modal-define-key (kbd "X -") (kbd "C-x -"))  ;; shrink-window-if-larger-than-buffer
(modal-define-key (kbd "X <") (kbd "C-x <"))  ;; scroll-left
(modal-define-key (kbd "X >") (kbd "C-x >"))  ;; scroll-right
(modal-define-key (kbd "X ;") (kbd "C-x C-;"))  ;; comment-line
(modal-define-key (kbd "X (") (kbd "C-x ("))  ;; kmacro-start-macro
(modal-define-key (kbd "X )") (kbd "C-x )"))  ;; kmacro-end-macro
(modal-define-key (kbd "X ^") (kbd "C-x ^"))  ;; enlarge-window
(modal-define-key (kbd "X {") (kbd "C-x {"))  ;; shrink-window-horizontally
(modal-define-key (kbd "X }") (kbd "C-x }"))  ;; enlarge-window-horizontally
(modal-define-key (kbd "X +") (kbd "C-x +"))  ;; balance-windows
(modal-define-key (kbd "X 0") (kbd "C-x 0"))  ;; delete-window
(modal-define-key (kbd "X 1") (kbd "C-x 1"))  ;; delete-other-windows
(modal-define-key (kbd "X 2") (kbd "C-x 2"))  ;; vsplit-last-buffer
(modal-define-key (kbd "X 3") (kbd "C-x 3"))  ;; hsplit-last-buffer
(modal-define-key (kbd "X 4 F") (kbd "C-x 4 C-f"))  ;; ido-find-file-other-window
(modal-define-key (kbd "X 4 b") (kbd "C-x 4 b"))  ;; ido-switch-buffer-other-window
(modal-define-key (kbd "X 5 F") (kbd "C-x 5 C-f"))  ;; ido-find-file-other-frame
(modal-define-key (kbd "X 5 b") (kbd "C-x 5 b"))  ;; ido-switch-buffer-other-frame
(modal-define-key (kbd "X C") (kbd "C-x C-c"))  ;; save-buffers-kill-emacs
(modal-define-key (kbd "X c") (kbd "C-x c"))  ;; rotate-or-inflection
(modal-define-key (kbd "X B") (kbd "C-x C-b"))  ;; list-buffers
(modal-define-key (kbd "X b") (kbd "C-x b"))  ;; switch-buffer
(modal-define-key (kbd "X D") (kbd "C-x C-d"))  ;; list-directory
(modal-define-key (kbd "X d") (kbd "C-x d"))  ;; dired
(modal-define-key (kbd "X E") (kbd "C-x C-e"))  ;; eval-last-sexp
(modal-define-key (kbd "X e") (kbd "C-x e"))  ;; kmacro-end-and-call-macro
(modal-define-key (kbd "X F") (kbd "C-x C-f"))  ;; find-file
(modal-define-key (kbd "X f") (kbd "C-x f"))  ;; find-file-at-point
(modal-define-key (kbd "X h") (kbd "C-x h"))  ;; mark-whole-buffer
(modal-define-key (kbd "X i") (kbd "C-x i"))  ;; ido-insert-file
(modal-define-key (kbd "X K TAB") #'select-kbd-macro)
(modal-define-key (kbd "X K K") (kbd "C-x C-k C-k"))  ;; kmacro-end-or-call-macro-repeat
(modal-define-key (kbd "X K N") (kbd "C-x C-k C-n"))  ;; kmacro-cycle-ring-next
(modal-define-key (kbd "X K n") (kbd "C-x C-k n"))  ;; kmacro-name-last-macro
(modal-define-key (kbd "X K P") (kbd "C-x C-k C-p"))  ;; kmacro-cycle-ring-previous
(modal-define-key (kbd "X K V") (kbd "C-x C-k C-v"))  ;; kmacro-view-macro-repeat
(modal-define-key (kbd "X k") (kbd "C-x k"))  ;; kill-buffer
(modal-define-key (kbd "X L") (kbd "C-x C-l"))  ;; downcase-region
(modal-define-key (kbd "X M-l") (kbd "C-x M-l"))  ;; recenter-horizontal
(modal-define-key (kbd "X o") (kbd "C-x o"))  ;; other-window
(modal-define-key (kbd "X O") #'ff-find-other-file)
(modal-define-key (kbd "X P") (kbd "C-x C-p"))  ;; mark-page
(modal-define-key (kbd "X p s") (kbd "C-x p s"))  ;; bookmark-save
(modal-define-key (kbd "X Q") (kbd "C-x C-q"))  ;; read-only-mode
(modal-define-key (kbd "X R") (kbd "C-x C-r"))  ;; recentf-open
;; [ equivalent
(modal-define-key (kbd "X r") (kbd "C-x r"))
;; (modal-define-key (kbd "X r SPC") (kbd "C-x r SPC"))  ;; point-to-register
;; (modal-define-key (kbd "X r +") (kbd "C-x r +"))  ;; increment-register
;; (modal-define-key (kbd "X r b") (kbd "C-x r b"))  ;; bookmark-jump
;; (modal-define-key (kbd "X r c") (kbd "C-x r c"))  ;; clean-rectangle
;; (modal-define-key (kbd "X r d") (kbd "C-x r d"))  ;; delete-rectangle
;; (modal-define-key (kbd "X r f") (kbd "C-x r f"))  ;; frameset-to-register
;; (modal-define-key (kbd "X r i") (kbd "C-x r i"))  ;; insert-register
;; (modal-define-key (kbd "X r j") (kbd "C-x r j"))  ;; jump-to-register
;; (modal-define-key (kbd "X r k") (kbd "C-x r k"))  ;; kill-rectangle
;; (modal-define-key (kbd "X r l") (kbd "C-x r l"))  ;; list-bookmarks
;; (modal-define-key (kbd "X r m") (kbd "C-x r m"))  ;; bookmark-set
;; (modal-define-key (kbd "X r N") (kbd "C-x r N"))  ;; rectangle-number-lines
;; (modal-define-key (kbd "X r n") (kbd "C-x r n"))  ;; number-to-register
;; (modal-define-key (kbd "X r o") (kbd "C-x r o"))  ;; open-rectangle
;; (modal-define-key (kbd "X r r") (kbd "C-x r r"))  ;; copy-rectangle-to-register
;; (modal-define-key (kbd "X r s") (kbd "C-x r s"))  ;; copy-to-register
;; (modal-define-key (kbd "X r t") (kbd "C-x r t"))  ;; string-rectangle
;; (modal-define-key (kbd "X r U") (kbd "C-x r U"))  ;; undo-tree-restore-state-from-register
;; (modal-define-key (kbd "X r u") (kbd "C-x r u"))  ;; undo-tree-save-state-to-register
;; (modal-define-key (kbd "X r y") (kbd "C-x r y"))  ;; yank-rectangle
;; (modal-define-key (kbd "X r M-w") (kbd "C-x r M-w"))  ;; copy-rectangle-as-kill
;; ]
(modal-define-key (kbd "X S") (kbd "C-x C-s"))  ;; save-buffer
(modal-define-key (kbd "X s") (kbd "C-x s"))  ;; save-some-buffers
(modal-define-key (kbd "X U") (kbd "C-x C-u"))  ;; upcase-region
(modal-define-key (kbd "X v =") #'magit-diff)
(modal-define-key (kbd "X v b") #'magit-branch)
(modal-define-key (kbd "X v c") #'magit-checkout)
(modal-define-key (kbd "X v L") #'vc-print-root-log)
(modal-define-key (kbd "X v l") #'vc-print-log)
(modal-define-key (kbd "X v m") #'hydra-smerge/body)
(modal-define-key (kbd "X v P") #'magit-push)
(modal-define-key (kbd "X v p") #'magit-pull)
(modal-define-key (kbd "X v R") #'magit-rebase-continue)
(modal-define-key (kbd "X v r") #'magit-rebase)
(modal-define-key (kbd "X v v") #'magit-status)
(modal-define-key (kbd "X W") (kbd "C-x C-w"))  ;; write-file
(modal-define-key (kbd "X X") (kbd "C-x C-x"))  ;; exchange-point-and-mark
(modal-define-key (kbd "X z") (kbd "C-x z"))  ;; repeat
;; x - ] command prefix
(modal-define-key (kbd "Y") (kbd "C-y"))  ;; yank
(modal-define-key (kbd "Z") (kbd "C-x z"))  ;; repeat (suspend-frame)

(modal-define-key (kbd "S-\\") (kbd "C-M-\\"))  ;; indent-region
(modal-define-key (kbd "S-@") (kbd "C-M-@"))  ;; mark-sexp
;; Caps-lock don't work with M-<Cap letter>, use Shift
(modal-define-key (kbd "M-A") (kbd "C-M-a"))  ;; beginning-of-defun
(modal-define-key (kbd "M-B") (kbd "C-M-b"))  ;; backward-sexp
(modal-define-key (kbd "M-C") (kbd "C-M-c"))  ;; exit-recursive-edit
(modal-define-key (kbd "M-D") (kbd "C-M-d"))  ;; down-list
(modal-define-key (kbd "M-E") (kbd "C-M-e"))  ;; end-of-defun
(modal-define-key (kbd "M-F") (kbd "C-M-f"))  ;; forward-sexp
;; G - [ command prefix
;; G - ] command prefix
(modal-define-key (kbd "M-H") (kbd "C-M-h"))  ;; mark-defun
(modal-define-key (kbd "M-I") (kbd "C-M-i"))  ;; completion-at-point/flyspell-auto-correct-word
(modal-define-key (kbd "M-J") (kbd "C-M-j"))  ;; indent-new-comment-line
(modal-define-key (kbd "M-K") (kbd "C-M-k"))  ;; kill-sexp
(modal-define-key (kbd "M-N") (kbd "C-M-n"))  ;; forward-list
(modal-define-key (kbd "M-L") (kbd "C-M-l"))  ;; reposition-window
(modal-define-key (kbd "M-O") (kbd "C-M-o"))  ;; split-line
(modal-define-key (kbd "M-P") (kbd "C-M-p"))  ;; backward-list
(modal-define-key (kbd "M-Q") "C-M-q")  ;; prog-indent-sexp
(modal-define-key (kbd "M-R") (kbd "C-M-r"))  ;; isearch-backward-regexp
(modal-define-key (kbd "M-S") (kbd "C-M-s"))  ;; isearch-forward-regexp
(modal-define-key (kbd "M-T") (kbd "C-M-t"))  ;; transpose-sexps
(modal-define-key (kbd "M-U") (kbd "C-M-u"))  ;; backward-up-list
(modal-define-key (kbd "M-V n") (kbd "C-M-v"))  ;; scroll-other-window
(modal-define-key (kbd "M-W") (kbd "C-M-w"))  ;; append-next-kill
(modal-define-key (kbd "M-X") (kbd "C-M-x"))  ;; eval-defun
;; Y - [ command prefix
;; Y - ] command prefix
;; Z - [ command prefix
;; Z - ] command prefix

;; (modal-define-key (kbd "M-Q") #'fill-paragraph)
(modal-define-key (kbd "M-V p") (kbd "C-M-S-v"))  ;; scroll-other-window-down

;;;;;;;;;;;;;;;
;; Caps keys ;;
;;;;;;;;;;;;;;;
;; (global-set-key "ª" 'modal-global-mode-post-command)
(modal-define-key "ª" #'caps-lock-mode)
(modal-define-key "º" #'caps-lock-mode-post-command)
;; Modal keys
;; (modal-define-key (kbd "µ") #'caps-lock-mode) ;; #'modal-global-mode-toggle)
;; (modal-define-key (kbd "<key-924>") #'caps-lock-mode) ;; #'modal-global-mode-toggle)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make compatible with other modules ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-key special-mode-map [?\S-\ ] nil)        ;; simple.el
(with-eval-after-load 'rmail
  (define-key rmail-mode-map [?\S-\ ] nil))       ;; rmail.el
(with-eval-after-load 'cus-edit
  (define-key custom-mode-map [?\S-\ ] nil)       ;; cus-edit.el
  (define-key custom-mode-link-map [?\S-\ ] nil)) ;; cus-edit.el
(mapc (lambda (keymap)
        (define-key keymap [?\S-\ ] nil))
      (keymaps-with-binding [?\S-\ ]))

;;;;;;;;;;;;;;;;
;; Ubiquitous ;;
;;;;;;;;;;;;;;;;
;; (setq isearch-mode-map (make-composed-keymap (copy-keymap modal-mode-map) isearch-mode-map)
;;       query-replace-map (make-composed-keymap (copy-keymap modal-mode-map) query-replace-map)
;;       function-key-map (make-composed-keymap (copy-keymap modal-mode-map) function-key-map)
;;       minibuffer-local-map (make-composed-keymap (copy-keymap modal-mode-map) minibuffer-local-map))

;;;;;;;;;;;;;;;;;;;
;; new quit bind ;;
;;;;;;;;;;;;;;;;;;;
;; (define-key query-replace-map "\M-q" 'quit)          ;; read-event
;; (define-key function-key-map "\M-q" "\C-g")          ;; read-key
;; (define-key isearch-mode-map "\M-q" 'isearch-abort)  ;; isearch-mode
;; ;; minibuffer keys
;; (define-key minibuffer-local-map "\M-q" 'abort-recursive-edit)
;; (define-key minibuffer-local-ns-map "\M-q" 'abort-recursive-edit)
;; (define-key minibuffer-local-isearch-map "\M-q" 'abort-recursive-edit)
;; (define-key minibuffer-local-completion-map "\M-q" 'abort-recursive-edit)
;; (define-key minibuffer-local-must-match-map "\M-q" 'abort-recursive-edit)
;; (define-key minibuffer-local-filename-completion-map "\M-q" 'abort-recursive-edit)
;; (global-set-key "\M-q" 'keyboard-quit)

;;;;;;;;;;;;;;;;;;;;;;
;; Globally enabled ;;
;;;;;;;;;;;;;;;;;;;;;;
(modal-global-mode 1)


(provide 'modal-config)
;;; modal-config.el ends here
