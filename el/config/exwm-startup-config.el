;; Inhibit dangerous functions
(with-eval-after-load 'frame
  (defun suspend-frame ()
    (interactive)
    (message "Command `suspend-frame' is dangerous in EXWM.")))

;; Functions
(defun exwm-screensaver-lock ()
  (interactive)
  (when (not (member "xscreensaver"
                     (mapcar
                      (lambda (item) (cdr (assoc 'comm item)))
                      (mapcar 'process-attributes (list-system-processes)))))
    (start-process "xscreensaver" nil "xscreensaver" "-no-splash")
    (sleep-for 1))
  (start-process "xscreensaver-command" nil "xscreensaver-command" "-lock"))

(defun exwm-update-screens ()
  (interactive)
  (let ((xrandr-monitor-regexp "\n\\([^ ]+\\) connected ")
        default-monitor)
    (with-temp-buffer
      (call-process "xrandr" nil t nil)
      (goto-char (point-min))
      (re-search-forward xrandr-monitor-regexp nil 'noerror)
      (setq default-monitor (match-string 1))
      (forward-line)
      (if (not (re-search-forward xrandr-monitor-regexp nil 'noerror))
          (progn
            (call-process "xrandr" nil nil nil "--output" default-monitor "--auto")
            (setq exwm-randr-workspace-monitor-plist (list 0 default-monitor)
                  exwm-workspace-number 1))
        (call-process "setup" nil nil nil "monitor" "left")
        (setq exwm-randr-workspace-monitor-plist (list 0 default-monitor 1 (match-string 1)))
        (forward-line)
        (let ((monitor-number 1))
          (while (re-search-forward xrandr-monitor-regexp nil 'noerror)
            (setq exwm-randr-workspace-monitor-plist
                  (nconc exwm-randr-workspace-monitor-plist (list (incf monitor-number)
                                                                  (match-string 1))))
            (forward-line))
          (setq exwm-workspace-number monitor-number))))))

(defun exwm-screen-count ()
  (let ((monitor-number 0))
    (with-temp-buffer
      (call-process "xrandr" nil t nil)
      (goto-char (point-min))
      (while (re-search-forward "\n\\([^ ]+\\) connected " nil 'noerror)
        (incf monitor-number)
        (forward-line)))
    monitor-number))

(defun exwm-workspace-index-plus (arg)
  (exwm-workspace-switch
   (let* ((workspace-count (exwm-workspace--count))
          (remainer (% (+ arg exwm-workspace-current-index) workspace-count)))
     (if (< remainer 0)
         (+ remainer workspace-count)
       remainer))))

(defun exwm-workspace-next ()
  (interactive)
  (exwm-workspace-index-plus 1))

(defun exwm-workspace-prev ()
  (interactive)
  (exwm-workspace-index-plus -1))

(defun exwm-randr-workspace-move (workspace monitor)
  (setq exwm-randr-workspace-monitor-plist
        (plist-put exwm-randr-workspace-monitor-plist workspace monitor)))

(defun exwm-randr-workspace-move-current (monitor)
  (interactive (list (let* ((result (if exwm-randr--compatibility-mode
                                        (exwm-randr--get-outputs)
                                      (exwm-randr--get-monitors)))
                            (primary-monitor (elt result 0))
                            (monitor-list (mapcar 'car (elt result 2))))
                       (completing-read "Move to monitor: " monitor-list nil t nil nil primary-monitor))))
  (exwm-randr-workspace-move exwm-workspace-current-index monitor)
  (exwm-randr-refresh))

;; Turn on `display-time-mode' if you don't use an external bar.
(setq display-time-default-load-average nil
      display-time-day-and-date t
      display-time-24hr-format t
      display-time-mail-string "✉")

;; You are strongly encouraged to enable something like `ido-mode' to alter
;; the default behavior of 'C-x b', or you will take great pains to switch
;; to or back from a floating frame (remember 'C-x 5 o' if you refuse this
;; proposal however).
;; You may also want to call `exwm-config-ido' later (see below).
(ido-mode 1)

;; Emacs server is not required to run EXWM but it has some interesting uses
;; (see next section).
(server-start)

;; (require 'mini-modeline)                     ;; + with mini-modeline
;; (setq mini-modeline-frame (selected-frame))  ;; + with mini-modeline

;;;; Below are configurations for EXWM.

;; Add paths (not required if EXWM is installed from GNU ELPA).
;(add-to-list 'load-path "/path/to/xelb/")
;(add-to-list 'load-path "/path/to/exwm/")

;; Load EXWM.
(require 'exwm)

;; Fix problems with Ido (if you use it).
(require 'exwm-config)
(exwm-config-ido)

;; Set the initial number of workspaces (they can also be created later).
(setq exwm-workspace-number (exwm-screen-count)
      exwm-workspace-minibuffer-position nil
      exwm-workspace-show-all-buffers t
      exwm-layout-show-all-buffers t)

;; All buffers created in EXWM mode are named "*EXWM*". You may want to
;; change it in `exwm-update-class-hook' and `exwm-update-title-hook', which
;; are run when a new X window class name or title is available.  Here's
;; some advice on this topic:
;; + Always use `exwm-workspace-rename-buffer` to avoid naming conflict.
;; + For applications with multiple windows (e.g. GIMP), the class names of
;    all windows are probably the same.  Using window titles for them makes
;;   more sense.
;; In the following example, we use class names for all windows expect for
;; Java applications and GIMP.
(add-hook 'exwm-update-class-hook
          (lambda ()
            (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-class-name))))
(add-hook 'exwm-update-title-hook
          (lambda ()
            (when (or (not exwm-instance-name)
                      (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                      (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-title))))

;; Global keybindings can be defined with `exwm-input-global-keys'.
;; Here are a few examples:
(setq exwm-input-global-keys
      `(
        ;; Bind "s-r" to exit char-mode and fullscreen mode.
        ([?\s-r] . exwm-reset)
        ;; Bind "s-w" to switch workspace interactively.
        ([?\s-w] . exwm-workspace-switch)
        ;; Bind "s-0" to "s-9" to switch to a workspace by its index.
        ,@(mapcar (lambda (i)
                    `(,(kbd (format "s-%d" i)) .
                      (lambda ()
                        (interactive)
                        (exwm-workspace-switch-create ,i))))
                  (number-sequence 0 9))
        ;; Bind "s-&" to launch applications ('M-&' also works if the output
        ;; buffer does not bother you).
        ([?\s-&] . (lambda (command)
                     (interactive (list (read-shell-command "$ ")))
                     (start-process-shell-command command nil command)))
        ;; Bind "s-<f2>" to "slock", a simple X display locker.
        ([s-f2] . (lambda ()
                    (interactive)
                    (start-process "" nil "/usr/bin/slock")))
        ;; Toggle char-line modes
        ([?\s-c] . exwm-input-toggle-keyboard)
        ;; Display datetime
        ([?\s-a] . display-time-mode)
        ;; Workspaces
        ([?\s-n] . exwm-workspace-next)
        ([?\s-p] . exwm-workspace-prev)
        ([?\s-s] . exwm-workspace-swap)
        ([?\s-m] . exwm-randr-workspace-move-current)
        ;; Bind lock screen
        (,(kbd "<s-escape>") . exwm-screensaver-lock)))

;; To add a key binding only available in line-mode, simply define it in
;; `exwm-mode-map'.  The following example shortens 'C-c q' to 'C-q'.
(define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)

;; The following example demonstrates how to use simulation keys to mimic
;; the behavior of Emacs.  The value of `exwm-input-simulation-keys` is a
;; list of cons cells (SRC . DEST), where SRC is the key sequence you press
;; and DEST is what EXWM actually sends to application.  Note that both SRC
;; and DEST should be key sequences (vector or string).
(setq exwm-input-simulation-keys
      '(
        ;; movement
        ([?\C-b] . [left])
        ([?\M-b] . [C-left])
        ([?\C-f] . [right])
        ([?\M-f] . [C-right])
        ([?\C-p] . [up])
        ([?\C-n] . [down])
        ([?\C-a] . [home])
        ([?\C-e] . [end])
        ([?\M-v] . [prior])
        ([?\C-v] . [next])
        ([?\C-d] . [delete])
        ([?\C-k] . [S-end delete])
        ;; cut/paste.
        ([?\C-w] . [?\C-x])
        ([?\M-w] . [?\C-c])
        ([?\C-y] . [?\C-v])
        ;; search
        ([?\C-s] . [?\C-f])))

;; You can hide the minibuffer and echo area when they're not used, by
;; uncommenting the following line.
;(setq exwm-workspace-minibuffer-position 'bottom)

;; Do not forget to enable EXWM. It will start by itself when things are
;; ready.  You can put it _anywhere_ in your configuration.
(exwm-enable)

;; Multi-monitor
(require 'exwm-randr)
(add-hook 'exwm-randr-screen-change-hook 'exwm-update-screens)
(exwm-randr-enable)

;; System tray
(require 'exwm-systemtray)
(exwm-systemtray-enable)

;; System monitor
(require 'symon)

(defun symon--message (format-string &rest args)
  (with-selected-window (minibuffer-window (next-frame (selected-frame) 'visible))
    (delete-region (minibuffer-prompt-end) (point-max))
    (insert (apply #'format-message format-string args))))

(when (bug-check-function-bytecode
       'symon--display-update
       "CIYGAMYgP4VIAMeJyBkaGwzHHYkeEYNDAA4RQBUJDhJVgzIAycrLzM3ODSIiIoiCNwDPzg0iiAlUEQ4RQYkWEYQZAC3QiRYThw==")
  (defun symon--display-update ()
    "update symon display"
    (unless (or cursor-in-echo-area (active-minibuffer-window))
      (let ((message-log-max nil)  ; do not insert to *Messages* buffer
            (display-string nil)
            (page 0))
        (dolist (lst symon--display-fns)
          (if (= page symon--active-page)
              (symon--message "%s" (apply 'concat (mapcar 'funcall lst)))
            (mapc 'funcall lst))
          (setq page (1+ page))))
      (setq symon--display-active t))))

(setcdr (last symon-monitors)
        `(,(cond ((memq system-type '(gnu/linux cygwin))
                  'symon-linux-battery-monitor)
                 ((memq system-type '(darwin))
                  'symon-darwin-battery-monitor)
                 ((memq system-type '(windows-nt))
                  'symon-windows-battery-monitor))
          symon-current-time-monitor))

(setq symon-delay 3
      symon-refresh-rate 5)

(symon-mode)


(provide 'exwm-startup-config)
