;; (defvar multiple-windows-mode-map
;;   (let (map (make-keymap))
;;     (set-char-table-range (nth 1 map) t #'multiple-windows--keypressed)
;;     (define-key map [escape] #'multiple-windows-mode)
;;     map))

(defvar multiple-windows--isearch-direction nil
  "Last isearch direction")

(defvar multiple-windows--default-cmds-prepare-alist
  '((isearch-forward . (setq multiple-windows--isearch-direction 'forward))
    (isearch-backward . (setq multiple-windows--isearch-direction 'backward))))

(defvar multiple-windows--default-cmds-remap-alist
  `((isearch-exit . ,(lambda ()
                       (interactive)
                       (isearch-repeat multiple-windows--isearch-direction))))
  "Default set of commands that should be mirrored by all cursors")

(defvar multiple-windows--default-cmds-to-run-for-all
  '(mc/keyboard-quit
    self-insert-command
    quoted-insert
    previous-line
    next-line
    newline
    newline-and-indent
    open-line
    delete-blank-lines
    transpose-chars
    transpose-lines
    transpose-paragraphs
    transpose-regions
    join-line
    right-char
    right-word
    forward-char
    forward-word
    left-char
    left-word
    backward-char
    backward-word
    forward-paragraph
    backward-paragraph
    upcase-word
    downcase-word
    capitalize-word
    forward-list
    backward-list
    hippie-expand
    hippie-expand-lines
    yank
    yank-pop
    append-next-kill
    kill-line
    kill-region
    kill-whole-line
    kill-word
    backward-kill-word
    backward-delete-char-untabify
    delete-char delete-forward-char
    delete-backward-char
    py-electric-backspace
    c-electric-backspace
    org-delete-backward-char
    cperl-electric-backspace
    python-indent-dedent-line-backspace
    paredit-backward-delete
    autopair-backspace
    just-one-space
    zap-to-char
    end-of-buffer
    end-of-defun
    end-of-line
    end-of-sexp
    set-mark-command
    exchange-point-and-mark
    cua-set-mark
    cua-replace-region
    cua-delete-region
    move-end-of-line
    beginning-of-buffer
    beginning-of-defun
    beginning-of-line
    beginning-of-sexp
    move-beginning-of-line
    kill-ring-save
    back-to-indentation
    subword-forward
    subword-backward
    subword-mark
    subword-kill
    subword-backward-kill
    subword-transpose
    subword-capitalize
    subword-upcase
    subword-downcase
    er/expand-region
    er/contract-region
    smart-forward
    smart-backward
    smart-up
    smart-down
    undo-tree-redo
    undo-tree-undo)
  "Default set of commands that should be mirrored by all cursors")

(defun multiple-windows--post-command ()
  (let ((prepare (alist-get this-original-command multiple-windows--default-cmds-prepare-alist)))
    (if prepare (eval prepare)))
  (catch 'break
    (let ((cmd (or (alist-get this-original-command multiple-windows--default-cmds-remap-alist)
                   (car (memq this-original-command multiple-windows--default-cmds-to-run-for-all))
                   (throw 'break nil))))
      (save-selected-window
        (dolist (other-window (cdr (window-list (selected-frame) 0 (selected-window))))
          (select-window other-window)
          (condition-case raised-error
              (call-interactively cmd)
            (error (message "%s: %s %s" cmd (error-message-string raised-error) other-window))))))))

(define-minor-mode multiple-windows-mode
  "Toggle Multiple Windows mode.
     With no argument, this command toggles the mode.
     Non-null prefix argument turns on the mode.
     Null prefix argument turns off the mode."
  :init-value nil
  :lighter "*"
  :group 'multiple-windows
  (if multiple-windows-mode
      (add-hook 'post-command-hook 'multiple-windows--post-command nil t)
    (remove-hook 'post-command-hook 'multiple-windows--post-command t)))


(provide 'multiple-windows)
;;; multiple-windows ends here
