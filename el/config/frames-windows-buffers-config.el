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

(defun list-all-buffers (&optional files-only)
  "Display a list of names of existing buffers.
The list is displayed in a buffer named `*Buffer List*'.
Non-null optional arg FILES-ONLY means mention only file buffers.

For more information, see the function `buffer-menu'."
  (interactive "P")
  (select-window
   (display-buffer (list-buffers-noselect files-only (buffer-list)))))

;;;;;;;;;;;;;
;; Windows ;;
;;;;;;;;;;;;;
;; display left rotating anticlockwise
(defun display-buffer-tiling-anticlockwise (buffer alist)
  (rotate-frame-anticlockwise)
  (display-buffer-in-direction buffer (cons '(direction . leftmost) alist)))

;; display right
(defun display-buffer-help-condition (buffer-name action)
  (with-current-buffer buffer-name
    (derived-mode-p 'help-mode)))

(defun display-buffer-at-right (buffer alist)
  (display-buffer-in-direction buffer (cons '(direction . rightmost) alist)))

;; (push '(display-buffer-help-condition
;;         display-buffer-at-right)
;;       display-buffer-alist)

;; display bottom
(defun display-buffer-term-condition (buffer-name action)
  (with-current-buffer buffer-name
    (derived-mode-p 'term-mode 'shell-mode 'eshell-mode
                    'docker-container-mode)))

;; (push '(display-buffer-term-condition
;;         display-buffer-at-bottom)
;;       display-buffer-alist)

;; display left
(defun display-buffer-main-condition (buffer-name action)
  (with-current-buffer buffer-name
    (derived-mode-p 'prog-mode 'org-mode)))

(defun display-buffer-at-left (buffer alist)
  (display-buffer-in-direction buffer (cons '(direction . leftmost) alist)))

;; (push '(display-buffer-main-condition
;;         display-buffer-at-left)
;;       display-buffer-alist)

;; split window
(defun split-window-mode-sensibly (&optional window)
  (or window (setq window (selected-window)))
  (cond
   ((with-selected-window window
      (derived-mode-p 'prog-mode 'org-mode 'help-mode))
    (let ((split-height-threshold nil)
          (split-width-threshold 140))
      (split-window-sensibly window)))
   ((with-selected-window window
      (derived-mode-p 'term-mode 'shell-mode 'eshell-mode
                      'docker-container-mode))
    (let ((split-height-threshold 20))
      (split-window-sensibly window)))
   (t
    (split-window-sensibly window))))

(defvar hscroll-aggressive nil)
(setq fit-window-to-buffer-horizontally nil
      register-preview-delay nil
      split-window-preferred-function 'split-window-mode-sensibly
      message-truncate-lines nil
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
      ;; [ ace-window 2
      ;; aw-scope 'visible
      ;; aw-char-position 'left
      ;; aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
      ;; aw-background t
      ;; ]
      )

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
;;(advice-add 'message :around #'message-filter)

;; Don't show on windows buffers currently showed
;; (defun diplay-buffer-advice (orig-fun buffer-or-name &optional action frame)
;;   (let ((window (funcall orig-fun buffer-or-name action frame)))
;;     (when (and (windowp window)
;;                (window-live-p window))
;;       (select-window window))))
;; (advice-add 'display-buffer :around 'diplay-buffer-advice)

;; (with-selected-window window
;;   (pulse-momentary-highlight-region (window-start window)
;;                                     (window-end window)))

(defun display-buffer-if-not-showed (orig-fun buffer-or-name &rest args)
  "Advice ORIG-FUN with args BUFFER-OR-NAME and ARGS.
Don't show on windows buffers currently showed."
  (let ((window (get-buffer-window buffer-or-name 0)))
    (if (windowp window)
        window
      (apply orig-fun buffer-or-name args))))
(advice-add 'display-buffer :around #'display-buffer-if-not-showed)

;; Thanks to: https://superuser.com/questions/132225/how-to-get-back-to-an-active-minibuffer-prompt-in-emacs-without-the-mouse
(defun switch-to-minibuffer-window ()
  "switch to minibuffer window (if active)"
  (interactive)
  (if (and (bound-and-true-p mini-frame-frame)
           (frame-live-p mini-frame-frame)
           (frame-visible-p mini-frame-frame))
      (select-frame mini-frame-frame)
    (when-let (minibuffer-window (active-minibuffer-window))
      (if (window-minibuffer-p)
          (switch-to-completions)
        (select-frame-set-input-focus (window-frame minibuffer-window))
        (select-window minibuffer-window)))))

;; undo and redo window distributions
(setq winner-dont-bind-my-keys t)
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

(defun vsplit-last-buffer (&optional size)
  "Split last buffer vertically."
  (interactive "P")
  (split-window-vertically size)
  (other-window 1)
  (switch-to-other-buffer))

(defun hsplit-last-buffer (&optional size)
  "Split last buffer horizontally."
  (interactive "P")
  (split-window-horizontally size)
  (other-window 1)
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

;; Switch window
(defun switch-to-window (arg)
  (interactive "P")
  (let ((windows (pcase arg
                   ('()
                    (window-list))
                   ('(4)
                    (apply #'append (mapcar #'window-list (visible-frame-list))))
                   ('(16)
                    (apply #'append (mapcar #'window-list (frame-list)))))))
    (setq windows (delq (selected-window) windows))
    (pcase (length windows)
      (0)
      (1 (select-window (car windows)))
      (_
       (let* ((windows-strings (mapcar #'buffer-name (mapcar #'window-buffer windows)))
              (windows-alist (cl-mapcar #'cons windows-strings windows))
              (option (completing-read
                       "Switch to: "
                       `(,@windows-strings windmove-left windmove-right windmove-up windmove-down)
                       nil t nil nil (car windows-strings)))
              (window-assoc (assoc option windows-alist)))
         (if window-assoc
             (select-window (cdr window-assoc))
           (funcall (intern option))))))))

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

(defun window-preserve-width (&optional window)
  (interactive)
  (window-preserve-size window t t))

(defun window-resize-equal (arg size)
  (interactive "P\nnSize: ")
  (let ((window (selected-window))
        (horizontal (not arg)))
    (window-resize window (- (if (< 0 size) size 80)
                             (window-size window horizontal))
                   horizontal)))

(defun window-resize-delta (arg delta)
  (interactive "P\nnDelta: ")
  (window-resize (selected-window) delta (not arg)))

(defun window-resize-factor (arg factor)
  (interactive "P\nnFactor: ")
  (let ((window (selected-window))
        (horizontal (not arg)))
    (window-resize window (round
                           (* (window-size window horizontal)
                              (1- factor)))
                   horizontal)))

;; autoresize
(setq resize-mini-windows t
      max-mini-window-height 0.7)
(defvar-local window-autoresize-size nil)

(defun window-autoresize (window)
  (when (and window-autoresize-size
             (not (active-minibuffer-window)))
    (let ((width-heigth (cdr (assoc (if (eq window (selected-window))
                                        'selected
                                      'unselected)
                                    window-autoresize-size))))
      (if width-heigth
          (let ((width (car width-heigth))
                (height (cdr width-heigth)))
            (if (numberp width)
                (let ((delta-width (- width (window-size window t))))
                  (if (and (/= 0 delta-width)
                           (/= 0 (setq delta-width
                                       (window-resizable
                                        window
                                        delta-width
                                        t))))
                      (window-resize window
                                     delta-width
                                     t))))
            (if (numberp height)
                (let ((delta-height (- height (window-size window))))
                  (if (and (/= 0 delta-height)
                           (/= 0 (setq delta-height
                                       (window-resizable
                                        window
                                        delta-height))))
                      (window-resize window
                                     delta-height)))))))))
(add-hook 'pre-redisplay-functions 'window-autoresize)

(defun window-autoresize-set-size (selected-width selected-height
                                                  unselected-width unselected-height)
  (interactive
   "xWidth selected: \nxHeight selected: \nxWidth unselected: \nxHeight unselected: ")
  (setq window-autoresize-size
        (list (cons 'selected (cons selected-width selected-height))
              (cons 'unselected (cons unselected-width unselected-height)))))

(defun window-autoresize-set-default (unselected-width)
  (interactive "p")
  (cond
   ((or (derived-mode-p 'org-mode)
        (derived-mode-p 'python-mode))
    (let ((numbers-margin (if display-line-numbers
                              (if (numberp display-line-numbers-width)
                                  display-line-numbers-width
                                3)
                            0)))
      (window-autoresize-set-size
       (+ 82 numbers-margin)
       30
       (if (<= unselected-width 1)
           (+ 19 numbers-margin)
         (+ 2 unselected-width numbers-margin))
       4)))))

(defun window-autoresize-unset ()
  (interactive)
  (setq window-autoresize-size nil))

;; [ Marcas laterales
(defun toggle-continuation-lines (&optional arg)
  (interactive "P")
  (if (if (numberp arg)
          (< 0 arg)
        (or visual-line-mode
            (null truncate-lines)))
      (progn
        (visual-line-mode -1)
        (toggle-truncate-lines 1))
    (unless (default-value 'truncate-lines)
      (toggle-truncate-lines -1))
    (when global-visual-line-mode
      (visual-line-mode 1))))

(with-eval-after-load 'fringe
  (setq-default indicate-buffer-boundaries 'right))
;; (fringe-mode '(4 . 4))
(with-eval-after-load 'simple
  (setq minor-mode-alist (assq-delete-all 'visual-line-mode minor-mode-alist))
  (defface visual-line-fringe-face
    '((t :foreground "gold1"))
    "Visual line fringe face" :group 'visual-line)
  (set-fringe-bitmap-face 'left-curly-arrow 'visual-line-fringe-face)
  (set-fringe-bitmap-face 'right-curly-arrow 'visual-line-fringe-face)
  (setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
  (global-visual-line-mode 1)
  (add-hook 'minibuffer-setup-hook 'visual-line-mode))
;; ]

;; winner
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
(global-set-key (kbd "M-s 0") 'switch-to-minibuffer-window)
(global-set-key (kbd "M-s 7 w") 'toggle-continuation-lines)
(global-set-key (kbd "M-s 7 v") #'visual-line-mode)
(global-set-key (kbd "C-x `") 'shrink-window)


;;;;;;;;;;;;
;; Frames ;;
;;;;;;;;;;;;
;; (setq initial-frame-alist (nconc '((minibuffer . only)) initial-frame-alist)
;;       default-frame-alist (nconc '((minibuffer . nil)) default-frame-alist)
;;       minibuffer-auto-raise t)
;; (add-hook 'minibuffer-exit-hook 'lower-frame)
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
(global-set-key (kbd "C-c w b") 'windmove-left)
(global-set-key (kbd "C-c w f") 'windmove-right)
(global-set-key (kbd "C-c w p") 'windmove-up)
(global-set-key (kbd "C-c w n") 'windmove-down)
(global-set-key (kbd "C-c b p") #'previous-buffer)
(global-set-key (kbd "C-c b n") #'next-buffer)
(global-set-key (kbd "C-x C-b") 'list-all-buffers)

;; (defhydra hydra-win (global-map "C-c")
;;   "WIN"
;;   ("<left>" windmove-left)
;;   ("<right>" windmove-right)
;;   ("<up>" windmove-up)
;;   ("<down>" windmove-down))

(global-set-key (kbd "C-c M-r") #'revert-buffer)
(global-set-key (kbd "C-x C-c") 'save-buffers-kill-terminal-with-choice)
(global-set-key (kbd "C-x k") 'kill-buffer-or-buffers-from-file)

(global-set-key (kbd "C-c b t") #'toggle-tool-bar-mode-from-frame)
(global-set-key (kbd "C-c b m") #'toggle-menu-bar-mode-from-frame)
(global-set-key (kbd "M-s 7 e") 'toggle-message-truncate-lines)

;; (global-set-key (kbd "C-x o") 'switch-to-window)
;; (global-set-key (kbd "C-x 2") 'vsplit-last-buffer)
;; (global-set-key (kbd "C-x 3") 'hsplit-last-buffer)


(define-key winner-mode-map [(control c) left] nil)
(define-key winner-mode-map [(control c) right] nil)
(define-key winner-mode-map (kbd "C-c w -") #'winner-undo)
(define-key winner-mode-map (kbd "<s-f11>") #'winner-undo)
(define-key winner-mode-map (kbd "C-c w _") #'winner-redo)
(define-key winner-mode-map (kbd "<s-f12>") #'winner-redo)
(global-set-key (kbd "<s-f10>") #'window-configuration-to-register)
(global-set-key (kbd "<s-f9>") #'jump-to-register)
(global-set-key (kbd "C-c w =") 'window-resize-equal)
(global-set-key (kbd "C-c w +") 'window-resize-delta)
(global-set-key (kbd "C-c w *") 'window-resize-factor)
(global-set-key (kbd "C-c w t") #'transpose-frame)
(global-set-key (kbd "C-c w h") #'flop-frame)
(global-set-key (kbd "C-c w v") #'flip-frame)
(global-set-key (kbd "C-c w r") #'rotate-frame-clockwise)
(global-set-key (kbd "C-c w C-r") #'rotate-frame-anticlockwise)
(global-set-key (kbd "C-c w 2") 'shell-2-window-frame)
(global-set-key (kbd "C-c w 3") 'shell-3-window-frame)
(global-set-key (kbd "C-c w a") 'toggle-hscroll-aggressive)
(global-set-key (kbd "C-c w o") 'halve-other-window-height)
(global-set-key (kbd "C-c w d a") 'window-dedicate-all)
(global-set-key (kbd "C-c w u a") 'window-undedicate-all)
(global-set-key (kbd "C-c w d t") 'window-dedicate-this)
(global-set-key (kbd "C-c w u t") 'window-undedicate-this)
(global-set-key (kbd "C-c w A d") 'window-autoresize-set-default)
(global-set-key (kbd "C-c w A s") 'window-autoresize-set-size)
(global-set-key (kbd "C-c w A u") 'window-autoresize-unset)
(global-set-key (kbd "C-c w P w") 'window-preserve-width)
(global-set-key (kbd "C-c w C-h") 'window-resize-height)
(global-set-key (kbd "C-c w C-w") 'window-resize-width)
(global-set-key (kbd "C-c w S") 'balance-windows-area)

(define-key global-map [remap list-buffers] 'ibuffer)
(define-key global-map (kbd "C-x B") 'ibuffer-list-buffers)


(provide 'frames-windows-buffers-config)
;;; frames-windows-buffers-config.el ends here
