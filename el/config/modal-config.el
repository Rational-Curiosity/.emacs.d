;;; modal-config.el --- Configure modal

;;; Commentary:

;; Usage:
;; (require 'modal-config)

;;; Code:
(require 'modal)

(setq modal-insert-cursor-type 'box
      modal-cursor-type 'hollow
      modal-excluded-modes '(calc-mode
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
        ((string-match "^ \\*" (buffer-name (current-buffer)))
         (bury-buffer))
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
(modal-define-kbd "(" "C-(" "sp-rewrap-sexp-lc")
(modal-define-kbd ")" "C-)" "sp-unwrap-sexp-lc")
(define-key modal-mode-map "º" #'er/expand-region)
(define-key modal-mode-map "ª" #'er/contract-region)
(define-key modal-mode-map "&" #'rotate-or-inflection)
(define-key modal-mode-map "?" #'goto-last-change-reverse)
(define-key modal-mode-map "/" #'goto-last-change)
(modal-define-kbd "." "M-." "definition-at-point")
(modal-define-kbd ";" "M-;" "comment-dwim")
(modal-define-kbd ":" "M-:" "eval-expression")
(modal-define-kbd "-" "C-_" "undo-tree-undo")
(modal-define-kbd "_" "M-_" "undo-tree-redo")
(modal-define-kbd "," "M-," "declaration-at-point")
(modal-define-kbd "%" "M-%" "query-replace")
(modal-define-kbd "*" "C-*" "duplicate-current-line-or-region")
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
(define-key modal-mode-map (kbd "c ! c") #'flycheck-buffer)
(define-key modal-mode-map (kbd "c ! n") #'flycheck-next-error)
(define-key modal-mode-map (kbd "c ! p") #'flycheck-previous-error)
(define-key modal-mode-map (kbd "c ! l") #'flycheck-list-errors)
;; c - [ command prefix
(modal-define-kbd "c b" "C-c C-b" "go-back")
(modal-define-kbd "c c" "C-c C-c")
(define-key modal-mode-map (kbd "c e w") #'er/mark-word)
(define-key modal-mode-map (kbd "c e s") #'er/mark-symbol)
(define-key modal-mode-map (kbd "c e c") #'er/mark-method-call)
(define-key modal-mode-map (kbd "c e q") #'er/mark-inside-quotes)
(define-key modal-mode-map (kbd "c e Q") #'er/mark-outside-quotes)
(define-key modal-mode-map (kbd "c e p") #'er/mark-inside-pairs)
(define-key modal-mode-map (kbd "c e P") #'er/mark-outside-pairs)
(define-key modal-mode-map (kbd "c i s") #'spanish-dictionary)
(define-key modal-mode-map (kbd "c i e") #'english-dictionary)
(define-key modal-mode-map (kbd "c i c") #'flyspell-buffer)
(define-key modal-mode-map (kbd "c i n") #'flyspell-goto-next-error)
(define-key modal-mode-map (kbd "c i p") #'flyspell-goto-previous-error)
(define-key modal-mode-map (kbd "c i a") #'flyspell-auto-correct-word)
(modal-define-kbd "c k" "C-c C-k")
(define-key modal-mode-map (kbd "c m p") #'mc/mark-previous-like-this)
(define-key modal-mode-map (kbd "c m n") #'mc/mark-next-like-this)
(define-key modal-mode-map (kbd "c m a") #'mc/mark-all-like-this-dwim)
(modal-define-kbd "c n" "C-c C-n" "smartscan-symbol-go-forward")
(define-key modal-mode-map (kbd "c o") #'operate-on-number-at-point-or-region)
(modal-define-kbd "c p" "C-c C-p" "smartscan-symbol-go-backward")
(modal-define-kbd "c r" "C-c M-s" "org-sort-entries-user-defined")
(define-key modal-mode-map (kbd "c R") #'revert-buffer)
(modal-define-kbd "c s" "C-c C-s" "org-schedule")
(modal-define-kbd "c t" "C-c C-t" "org-todo")
(modal-define-kbd "c u" "C-c C-u" "outline-up-heading")
(modal-define-kbd "c v" "C-c C-v")
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
;; c - ] command prefix
(modal-define-kbd "d" "<deletechar>" "delete-forward-char")
(modal-define-kbd "e" "C-e" "move-end-of-line")
(modal-define-kbd "f" "C-f" "forward-char")
(modal-define-kbd "g" "C-g" "keyboard-quit")
(modal-define-kbd "h" "M-h" "mark-paragraph")
;; i - reserved
(modal-define-kbd "j" "M-j" "indent-new-comment-line")
(modal-define-kbd "k" "C-k" "kill-line")
(modal-define-kbd "l" "C-l" "recenter-top-bottom")
(modal-define-kbd "m" "C-m" "newline")
(modal-define-kbd "n" "C-n" "next-line")
(modal-define-kbd "o" "C-o" "open-line")
(modal-define-kbd "p" "C-p" "previous-line")
(modal-define-kbd "r" "C-r" "isearch-backward")
(modal-define-kbd "s" "C-s" "isearch-forward")
(modal-define-kbd "t" "C-t" "transpose-chars")
;; u - reserved
(modal-define-kbd "v" "C-v" "scroll-up-command")
(modal-define-kbd "w" "C-w" "kill-region")
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
(modal-define-kbd "x s" "C-x C-s" "save-buffer")
(modal-define-kbd "x S" "C-x s" "save-some-buffers")
(modal-define-kbd "x r" "C-x C-r" "recentf-open")
(modal-define-kbd "x u" "C-x C-u" "upcase-region")
(modal-define-kbd "x x" "C-x C-x" "exchange-point-and-mark")
;; x - ] command prefix
(modal-define-kbd "y" "C-y" "yank")
(modal-define-kbd "z" "M-z" "avy-goto-char-timer")

(modal-define-kbd "A" "M-a")
(modal-define-kbd "B" "M-b")
(modal-define-kbd "C" "M-c")
(modal-define-kbd "D" "M-d")
(modal-define-kbd "E" "M-e")
(modal-define-kbd "F" "M-f")
(modal-define-kbd "G g" "M-g g" "goto-line")
(modal-define-kbd "G G" "M-g M-g" "goto-line")
(modal-define-kbd "H" "M-H")
(modal-define-kbd "I" "M-i")
(modal-define-kbd "J" "M-j")
(modal-define-kbd "K" "M-k")
(modal-define-kbd "L" "M-l")
(modal-define-kbd "M" "M-m")
(modal-define-kbd "N" "M-n")
(modal-define-kbd "O" "M-o")
(define-key modal-mode-map "Q" #'fill-paragraph)
(modal-define-kbd "P" "M-p")
(modal-define-kbd "R" "M-r")
;; S
(modal-define-kbd "T" "M-t")
(modal-define-kbd "U" "M-u")
(modal-define-kbd "V" "M-v")
(modal-define-kbd "W" "M-w")
;; X - [ command prefix
;; X - ] command prefix
(modal-define-kbd "Y" "M-y")
(modal-define-kbd "Z" "C-x z" "repeat")

(modal-define-kbd "M-\\" "C-M-\\" "indent-region")
(modal-define-kbd "M-@" "C-M-@" "mark-sexp")
(modal-define-kbd "M-a" "C-M-a" "beginning-of-defun")
(modal-define-kbd "M-b" "C-M-b" "backward-sexp")
(modal-define-kbd "M-e" "C-M-e" "end-of-defun")
(modal-define-kbd "M-f" "C-M-f" "forward-sexp")
(modal-define-kbd "M-h" "C-M-h" "mark-defun")
(modal-define-kbd "M-k" "C-M-k" "kill-sexp")
(modal-define-kbd "M-n" "C-M-n" "forward-list")
(modal-define-kbd "M-p" "C-M-p" "backward-list")
(global-set-key "\M-q" #'keyboard-esc-quit)

(modal-global-mode 1)


(provide 'modal-config)
;;; modal-config.el ends here
