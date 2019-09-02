;;; frames-windows-buffers-config.el --- Configuration and utils for visuals

;;; Commentary:

;; Usage:
;; (require 'frames-windows-buffers-config)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                               ;;
;;   Frames, windows & buffers   ;;
;;                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;
;; Buffers ;;
;;;;;;;;;;;;;
(setq revert-without-query '("\\.calc\\.py\\'"))

(defun buffers-from-file (&optional frame)
  "Get buffers from file in FRAME."
  (let ((tmp-list '()))
    (dolist (buffer (buffer-list frame))
      (when (buffer-file-name buffer)
        (setq tmp-list (cons buffer tmp-list))))
    tmp-list))

(defun kill-buffers-from-file (&optional frame)
  "Kill buffers from file in FRAME."
  (interactive (list (if current-prefix-arg
                         (read-from-minibuffer "Frame: ")
                       nil)))
  (let ((file-list (buffers-from-file frame)))
    (when file-list
      (mapc #'kill-buffer file-list))))

(defun kill-buffer-or-buffers-from-file (&optional frame)
  "Kill buffers or buffers from file in FRAME."
  (interactive (list (if current-prefix-arg
                         (selected-frame)
                       nil)))
  (if frame
      (kill-buffers-from-file frame)
    (kill-buffer)))

(defun buffers-with-window (&optional frame)
  "Get buffers with window in FRAME."
  (let ((tmp-list '()))
    (dolist (window (window-list frame))
      (let ((buffer (window-buffer window)))
        (when (buffer-file-name buffer)
          (setq tmp-list (cons buffer tmp-list)))))
    (delete-dups tmp-list)))

;; Kill all file buffers with window
(defun kill-buffers-with-window (&optional frame)
  "Kill all buffers from file with window in FRAME."
  (interactive (list (if current-prefix-arg
                         (read-from-minibuffer "Frame: ")
                       nil)))
  (let ((file-list (buffers-with-window frame)))
    (when file-list
      (mapc 'kill-buffer file-list))))

;; Kill buffer if you wish when close frame.
(defun kill-buffers-group-choice (&optional frame)
  "Kill buffers if you wish when close FRAME."
  (interactive (list (if current-prefix-arg
                         (read-from-minibuffer "Frame: ")
                       nil)))
  (when (buffers-with-window)
    (cl-case (read-char-choice "Kill buffers [c]urrent/[f]iles/[w]indows/[n]othing? "
                            (append "cCfFwWnNqQ" nil))
      ((?c ?C)
       (kill-buffer))
      ((?f ?F)
       (kill-buffers-from-file frame))
      ((?w ?W)
       (kill-buffers-with-window frame)))))


;;;;;;;;;;;;;
;; Windows ;;
;;;;;;;;;;;;;
(defvar hscroll-aggressive nil)
(setq split-width-threshold 140
      ;; Vertical Scroll
      ;;scroll-preserve-screen-position 'allways
      scroll-margin 2
      scroll-step 1
      ;;redisplay-dont-pause t
      ;; Scroll line by line
      scroll-conservatively 101
      ;; Horizontal Scroll
      hscroll-margin 2
      hscroll-step 1
      message-truncate-lines nil)
(add-hook 'term-mode-hook
          (lambda ()
            (set (make-local-variable 'scroll-margin) 0)))


(defun toggle-message-truncate-lines ()
  "Toggle truncate lines in messages."
  (interactive)
  (setq message-truncate-lines (not message-truncate-lines)))

(defun toggle-hscroll-aggressive ()
  "Toggle hscroll aggressive."
  (interactive)
  (if hscroll-aggressive
      (progn
        (setq  hscroll-margin 2
               hscroll-step 1
               hscroll-aggressive nil))
    (progn
      (setq  hscroll-margin 10
             hscroll-step 25
             hscroll-aggressive t))))
;; [ filter annoying messages
;; (defvar message-filter-regexp-list '("^Starting new Ispell process \\[.+\\] \\.\\.\\.$"
;;                                      "^Ispell process killed$")
;;   "filter formatted message string to remove noisy messages")
;; (defadvice message (around message-filter-by-regexp activate)
;;   (if (not (ad-get-arg 0))
;;       ad-do-it
;;     (let ((formatted-string (apply 'format (ad-get-args 0))))
;;       (if (and (stringp formatted-string)
;;                (some (lambda (re) (string-match re formatted-string)) message-filter-regexp-list))
;;           (save-excursion
;;             (set-buffer "*Messages*")
;;             (goto-char (point-max))
;;             (insert formatted-string "\n"))
;;         (progn
;;           (ad-set-args 0 `("%s" ,formatted-string))
;;           ad-do-it)))))
;; ]
;; message timestamp
;; thanks to: https://emacs.stackexchange.com/questions/32150/how-to-add-a-timestamp-to-each-entry-in-emacs-messages-buffer
(defun message-timestamp-advice (format-string &rest args)
  "Advice to run before `message' with FORMAT-STRING ARGS that prepend a timestamp to each message."
  (unless (string-equal format-string "%s%s")
    (let ((deactivate-mark nil)
          (inhibit-read-only t))
      (with-current-buffer "*Messages*"
        (goto-char (point-max))
        (if (not (bolp))
            (newline))
        (let* ((nowtime (current-time))
               (now-ms (nth 2 nowtime)))
          (insert (format-time-string "[%Y-%m-%d %T" nowtime)
                  (format ".%06d]" now-ms) " "))))))
(defvar message-advice-timestamp nil)
(defun advice-message-timestamp ()
  (interactive)
  (set 'message-advice-timestamp t))
(defun unadvice-message-timestamp ()
  (interactive)
  (set 'message-advice-timestamp nil))
;; not necesary, included in message-filter
;; (advice-add 'message :before 'message-timestamp-advice)
(defun signal-timestamp-advice (error-symbol data)
  "Advice to run before `signal' with ERROR-SYMBOL DATA that prepend a timestamp to each message."
  (let ((deactivate-mark nil)
        (inhibit-read-only t))
    (with-current-buffer "*Messages*"
      (goto-char (point-max))
      (if (not (bolp))
          (newline))
      (let* ((nowtime (current-time))
             (now-ms (nth 2 nowtime)))
        (insert (format-time-string "<%Y-%m-%d %T" nowtime)
                (format ".%06d>" now-ms) " " (if data (format "%s" data)))))))
(defun advice-signal-timestamp ()
  (interactive)
  (advice-add 'signal :before 'signal-timestamp-advice))
(defun unadvice-signal-timestamp ()
  (interactive)
  (advice-remove 'signal 'signal-timestamp-advice))
;; Truncate lines in messages and filter messages buffer
(defvar message-nillog-filter-functions '()) ;; (lambda (str) (string-match-p "oading" str))
(defvar message-inhibit-filter-functions '())
(defun message-filter (orig-fun msg &rest args)
  "Advice ORIG-FUN with args MSG and ARGS.  Filter arguments."
  (if (and
       message-log-max
       (not inhibit-message)
       msg)
      (let ((msg-str (apply #'format msg args)))
        (let ((inhibit-message
               (and
                message-inhibit-filter-functions
                (cl-some #'(lambda (func)
                             (funcall func msg-str)) message-inhibit-filter-functions)))
              (message-log-max
               (if (and
                    message-nillog-filter-functions
                    (cl-some #'(lambda (func)
                                 (funcall func msg-str)) message-nillog-filter-functions))
                   nil
                 message-log-max)))
          (if message-advice-timestamp (message-timestamp-advice msg))
          (apply orig-fun msg args)))
    (apply orig-fun msg args)))
(advice-add 'message :around #'message-filter)

;; Don't show on windows buffers currently showed
(defun display-buffer-if-not-showed (orig-fun buffer-or-name &rest args)
  "Advice ORIG-FUN with args BUFFER-OR-NAME and ARGS.
Don't show on windows buffers currently showed."
  (let ((window (get-buffer-window buffer-or-name 0)))
    (if (windowp window)
        window
      (apply orig-fun buffer-or-name args))))
(advice-add 'display-buffer :around #'display-buffer-if-not-showed)


;; undo and redo window distributions
(winner-mode)

(require 'windmove)
;;(windmove-default-keybindings 'meta)

(require 'find-file)
(defun switch-to-other-buffer ()
  "Switch to dual buffer whether exists."
  (interactive)
  (let ((ignore ff-ignore-include)
        (create ff-always-try-to-create))
    (setq ff-ignore-include t)
    (setq ff-always-try-to-create nil)
    (unless (or (not (fboundp 'ff-find-the-other-file))
                (ff-find-the-other-file))
      (let ((file-list (buffers-from-file)))
        (if file-list
            (switch-to-buffer (cl-first file-list))
          (switch-to-prev-buffer))))
    (setq ff-ignore-include ignore)
    (setq ff-always-try-to-create create)))

(defun vsplit-last-buffer ()
  "Split last buffer vertically."
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-other-buffer))

(defun hsplit-last-buffer ()
  "Split last buffer horizontally."
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-other-buffer))

;; Desbalancea el split vertical
;;(defadvice split-window-vertically
;;   (after my-window-splitting-advice activate)
;;    (enlarge-window (truncate (/ (window-body-height) 2))))

(defun halve-other-window-height ()
  "Expand current window to use half of the other window's lines."
  (interactive)
  (balance-windows)
  (enlarge-window (truncate (/ (window-height) 2))))

;; use Shift+arrow_keys to move cursor around split panes
;;(windmove-default-keybindings)

;; when cursor is on edge, move to the other side, as in a toroidal space
;;(setq windmove-wrap-around t )

(defun window-dedicate-this ()
  "Dedicate focus window."
  (interactive)
  (set-window-dedicated-p (selected-window) t))

(defun window-undedicate-this ()
  "Dedicate focus window."
  (interactive)
  (set-window-dedicated-p (selected-window) nil))

(defun window-dedicate-all (&optional frame)
  "Dedicate all windows in the presente FRAME."
  (interactive)
  (dolist (window (window-list frame))
    (set-window-dedicated-p window t)))

(defun window-undedicate-all (&optional frame)
  "Dedicate all windows in the presente FRAME."
  (interactive)
  (dolist (window (window-list frame))
    (set-window-dedicated-p window nil)))

;; window resize
(defun window-resize-width (arg &optional window max-width min-width preserve-size)
  "ARG nil Fit WINDOW according to its buffer's width.
WINDOW, MAX-WIDTH and MIN-WIDTH have the same meaning as in
`fit-window-to-buffer'.

ARG non-nil resize window to ARG width."
  (interactive "P")
  (if arg
      (window-resize (or window (selected-window)) (- arg (window-width)) t)
    (let ((fit-window-to-buffer-horizontally 'only))
      (fit-window-to-buffer window nil nil max-width min-width preserve-size))))

(defun window-resize-height (arg &optional window max-height min-height preserve-size)
  "ARG nil Fit WINDOW according to its buffer's height.
WINDOW, MAX-HEIGHT and MIN-HEIGHT have the same meaning as in
`fit-window-to-buffer'.

ARG non-nil resize window to ARG height."
  (interactive "P")
  (if arg
      (window-resize (or window (selected-window)) (- arg (window-height)))
    (let ((fit-window-to-buffer-horizontally nil))
      (fit-window-to-buffer window max-height min-height nil nil preserve-size))))

(require 'winner)
(defhydra hydra-win (:foreign-keys warn)
  "WIN"
  ("C-<right>" (lambda () (interactive)
               (enlarge-window-horizontally 1)
               (message "Width: %i" (window-width))))
  ("S-<right>" (lambda () (interactive)
                 (enlarge-window-horizontally 10)
                 (message "Width: %i" (window-width))) "↔+")
  ("C-<left>" (lambda () (interactive)
              (shrink-window-horizontally 1)
              (message "Width: %i" (window-width))))
  ("S-<left>" (lambda () (interactive)
              (shrink-window-horizontally 10)
              (message "Width: %i" (window-width))) "↔-")
  ("C-<up>" (lambda () (interactive)
            (enlarge-window 1)
            (message "Height: %i" (window-height))))
  ("S-<up>" (lambda () (interactive)
            (enlarge-window 10)
            (message "Height: %i" (window-height))) "↕+")
  ("C-<down>" (lambda () (interactive)
              (shrink-window 1)
              (message "Height: %i" (window-height))))
  ("S-<down>" (lambda () (interactive)
              (shrink-window 10)
              (message "Height: %i" (window-height))) "↕-")
  ("C-p" winner-undo "undo")
  ("C-n" winner-redo "redo")
  ("M-q" nil "quit"))
(global-set-key (kbd "C-c w m") 'hydra-win/body)

;;;;;;;;;;;;
;; Frames ;;
;;;;;;;;;;;;
;; [ Marcas laterales
;; (require 'fringe)
(with-eval-after-load 'fringe
  (setq-default indicate-buffer-boundaries 'right))
;; (fringe-mode '(4 . 4))
;; ]
;; Deshabilita la barra de scroll
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
;; Deshabilita la barra de iconos
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'tooltip-bar-mode) (tooltip-bar-mode -1))
;; Deshabilida la barra de menús
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
;; Ajusta el tamaño de la ventana a la resolución.
;; (defun set-frame-size-according-to-resolution ()
;;   (interactive)
;;   (if window-system
;;   (progn
;;     ;; use 100 char wide window for largeish displays
;;     ;; and smaller 80 column windows for smaller displays
;;     ;; pick whatever numbers make sense for you
;;     (if (> (x-display-pixel-width) 1280)
;;            (add-to-list 'default-frame-alist (cons 'width 100))
;;            (add-to-list 'default-frame-alist (cons 'width 80)))
;;     ;; for the height, subtract a couple hundred pixels
;;     ;; from the screen height (for panels, menubars and
;;     ;; whatnot), then divide by the height of a char to
;;     ;; get the height we want
;;     (add-to-list 'default-frame-alist
;;          (cons 'height (/ (- (x-display-pixel-height) 250)
;;                              (+ (frame-char-height) 1)))))))
;;
;; (set-frame-size-according-to-resolution)

;; Call last keyboard macro in windows
(defun kmacro-call-other-windows-all-frames (window &optional all-frames)
  "Call last keyboard macro in windows other than WINDOW.

Optional argument ALL-FRAMES nil or omitted means consider all windows
on WINDOW’s frame, plus the minibuffer window if specified by the
MINIBUF argument.  If the minibuffer counts, consider all windows on all
frames that share that minibuffer too.  The following non-nil values of
ALL-FRAMES have special meanings:

- t means consider all windows on all existing frames.

- ‘visible’ means consider all windows on all visible frames.

- 0 (the number ero) means consider all windows on all visible and
  iconified frames.

- A frame means consider all windows on that frame only.

Anything else means consider all windows on WINDOW’s frame and no
others."
  (interactive (list (selected-window) 'visible))
  (save-selected-window
    (dolist (other-window (cdr (window-list-1 window 0 all-frames)))
      (select-window other-window)
      (kmacro-call-macro 1))))

(defun kmacro-call-all-windows-all-frames (&optional all-frames)
  "Call last keyboard macro in windows other than WINDOW.

Optional argument ALL-FRAMES nil or omitted means consider all windows
on WINDOW’s frame, plus the minibuffer window if specified by the
MINIBUF argument.  If the minibuffer counts, consider all windows on all
frames that share that minibuffer too.  The following non-nil values of
ALL-FRAMES have special meanings:

- t means consider all windows on all existing frames.

- ‘visible’ means consider all windows on all visible frames.

- 0 (the number zero) means consider all windows on all visible and
  iconified frames.

- A frame means consider all windows on that frame only.

Anything else means consider all windows on WINDOW’s frame and no
others."
  (interactive (list 'visible))
  (save-selected-window
    (dolist (window (window-list-1 nil 0 all-frames))
      (select-window window)
      (kmacro-call-macro 1))))

(defun kmacro-call-other-windows-in-frame (&optional frame window)
  "Call last keyboard macro in FRAME's windows other than WINDOW."
  (interactive (list (selected-frame) (selected-window)))
  (save-selected-window
    (dolist (other-window (cdr (window-list frame 0 window)))
      (select-window other-window)
      (kmacro-call-macro 1))))

(defun kmacro-call-all-windows-in-frame (&optional frame)
  "Call last keyboard macro in FRAME's windows."
  (interactive (list (selected-frame)))
  (save-selected-window
    (dolist (window (window-list frame 0))
      (select-window window)
      (kmacro-call-macro 1))))

;;(add-hook 'delete-frame-functions #'kill-buffers-group-choice)
(require 'server)
(defun save-buffers-kill-terminal-with-choice (&optional arg)
  "Exit Emacs with ARG option."
  (interactive "P")
  (cond
   ((not (frame-parameter nil 'client))
    (save-buffers-kill-emacs arg))
   ((equal arg '(4))
    (save-buffers-kill-terminal arg))
   ((equal arg '(16))
    (save-buffers-kill-emacs arg))
   (t
    (progn
      (save-some-buffers arg)
      (kill-buffers-group-choice)
      (let ((proc (frame-parameter nil 'client)))
        (cond ((eq proc 'nowait)
               ;; Nowait frames have no client buffer list.
               (if (cdr (frame-list))
                   (delete-frame)
                 ;; If we're the last frame standing, kill Emacs.
                 (save-buffers-kill-emacs arg)))
              ((processp proc)
               (let ((buffers (process-get proc 'buffers)))
                 ;; If client is bufferless, emulate a normal Emacs exit
                 ;; and offer to save all buffers.  Otherwise, offer to
                 ;; save only the buffers belonging to the client.
                 (save-some-buffers
                  arg (if buffers
                          (lambda () (memq (current-buffer) buffers))
                        t))
                 (server-delete-client proc)))
              (t (error "Invalid client frame"))))))))

(require 'transpose-frame)
;; [ Frames layouts
(defun shell-3-window-frame ()
  "Development window format."
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (split-window-vertically)
  (shrink-window (truncate (/ (* (window-height) 2) 5)))
  (other-window 1)
  (other-window 1)
  (shell)
  (other-window 2)
  (switch-to-other-buffer))

(defun shell-2-window-frame ()
  "Test window format."
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (other-window 1)
  (shell))
;; ]
;;;;;;;;;;
;; Keys ;;
;;;;;;;;;;
(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)

;; (defhydra hydra-win (global-map "C-c")
;;   "WIN"
;;   ("<left>" windmove-left)
;;   ("<right>" windmove-right)
;;   ("<up>" windmove-up)
;;   ("<down>" windmove-down))

(global-set-key (kbd "C-c R") #'revert-buffer)
(global-set-key (kbd "C-x C-c") 'save-buffers-kill-terminal-with-choice)
(global-set-key (kbd "C-x k") 'kill-buffer-or-buffers-from-file)

(global-set-key (kbd "C-c b t") #'toggle-tool-bar-mode-from-frame)
(global-set-key (kbd "C-c b m") #'toggle-menu-bar-mode-from-frame)
(global-set-key (kbd "C-c e") 'toggle-message-truncate-lines)

(global-set-key (kbd "C-x 2") 'vsplit-last-buffer)
(global-set-key (kbd "C-x 3") 'hsplit-last-buffer)

(global-set-key (kbd "C-c w t") #'transpose-frame)
(global-set-key (kbd "C-c w h") #'flop-frame)
(global-set-key (kbd "C-c w v") #'flip-frame)
(global-set-key (kbd "C-c w r") #'rotate-frame-clockwise)
(global-set-key (kbd "C-c w R") #'rotate-frame-anticlockwise)
(global-set-key (kbd "C-c w -") #'winner-undo)
(global-set-key (kbd "C-c w _") #'winner-redo)
(global-set-key (kbd "C-c w 2") 'shell-2-window-frame)
(global-set-key (kbd "C-c w 3") 'shell-3-window-frame)
(global-set-key (kbd "C-c w a") 'toggle-hscroll-aggressive)
(global-set-key (kbd "C-c w o") 'halve-other-window-height)
(global-set-key (kbd "C-c w d a") 'window-dedicate-all)
(global-set-key (kbd "C-c w u a") 'window-undedicate-all)
(global-set-key (kbd "C-c w d t") 'window-dedicate-this)
(global-set-key (kbd "C-c w u t") 'window-undedicate-this)
(global-set-key (kbd "C-c w H") 'window-resize-height)
(global-set-key (kbd "C-c w W") 'window-resize-width)


(provide 'frames-windows-buffers-config)
;;; frames-windows-buffers-config.el ends here
