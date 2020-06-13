;;;; Below are configurations for EXWM.

;; Add paths (not required if EXWM is installed from GNU ELPA).
;(add-to-list 'load-path "/path/to/xelb/")
;(add-to-list 'load-path "/path/to/exwm/")

;; Load EXWM.
(require 'exwm)

;; Fix problems with Ido (if you use it).
;; (require 'exwm-config)
;; (exwm-config-ido)
(require 'exwm-randr)

;; Inhibit dangerous functions
(with-eval-after-load 'frame
  (defun suspend-frame ()
    (interactive)
    (message "Command `suspend-frame' is dangerous in EXWM.")))

;; Faces
(defface exwm-record-face
  '((t :foreground "red2"))
  "Basic face used to highlight errors and to denote failure."
  :group 'exwm)

;; Variables
(defvar exwm-exclude-transparency '("totem" "vlc" "darkplaces" "doom" "gzdoom")
  "EXWM instances without transparency.")

(defvar exwm-default-transparency 0.85
  "EXWM default transparency.")

(defvar exwm-hide-mode-line '("code")
  "EXWM instances without mode line")

;; example: export EXWM_MONITOR_ORDER="eDP-1 HDMI-1 DP-1"
(defvar exwm-default-monitor-order
  (let ((monitor-order (getenv "EXWM_MONITOR_ORDER")))
    (if monitor-order
        (condition-case nil
            (split-string monitor-order  " ")
          (error nil))))
  "EXWM default monitor order.")

(defvar exwm-default-minibuffer-number
  (let ((number (getenv "EXWM_MINIBUFFER_NUMBER")))
    (if number
        (string-to-number number)
      0))
  "EXWM default minibuffer workspace number.")

(defvar exwm-default-wallpaper-folder "~/Pictures/backgrounds/"
  "EXWM default wallpaper folder.")

(defvar exwm-record-process nil
  "EXWM record process when recording.")

(defvar exwm-record-recording (propertize "⏺" 'face 'exwm-record-face)
  "EXWM recording text displayed while recording")

;; Functions
(defun exwm-record-stop ()
  (interactive)
  (when exwm-record-process
    (interrupt-process exwm-record-process)
    (message "EXWM Record process interrupted")))

(defun exwm-record-start (&optional monitor-name)
  (interactive)
  (let ((monitors (exwm-xrandr-parse)))
    (if (null monitor-name)
        (setq monitor-name (completing-read
                            "Select monitor: "
                            (hash-table-keys monitors)
                            nil t)))
    (let ((monitor (gethash monitor-name monitors)))
      (setq exwm-record-process
            (start-process
             "*exwm-record-process*" (if current-prefix-arg "*ffmpeg output*")
             "ffmpeg" "-thread_queue_size" "512"
             "-nostats" "-hide_banner"
             "-loglevel" (if current-prefix-arg "warning" "quiet")
             ;; video input
             "-video_size" (gethash 'resolution monitor)
             "-framerate" "20"
             "-probesize" "30M"
             "-f" "x11grab"
             "-i" (concat ":0.0+" (gethash 'x monitor) "," (gethash 'y monitor))
             ;; audio imput
             "-f" "pulse" "-ac" "2" "-i" "default"
             ;; audio codec
             "-codec:a" "copy"
             ;; video codec
             "-codec:v" "libx264"
             ;; options
             "-crf" "0" "-preset" "ultrafast"
             "-threads" "4"
             (expand-file-name (concat
                                monitor-name
                                (format-time-string "_%Y-%m-%d_%H.%M.%S.mkv"))
                               (if (file-directory-p "~/Videos/")
                                   "~/Videos/"
                                 "~/"))))))
  (if (eq 'run (process-status exwm-record-process))
      (message "EXWM Record process started")
    (message "EXWM Record process failed")))

(defun exwm-record-toggle ()
  (interactive)
  (if exwm-record-process
      (if (eq 'run (process-status exwm-record-process))
          (exwm-record-stop)
        (exwm-record-start))
    (exwm-record-start)))

(defun exwm-screensaver-lock ()
  (interactive)
  (when (not (member "xscreensaver"
                     (mapcar
                      (lambda (item) (cdr (assoc 'comm item)))
                      (mapcar 'process-attributes (list-system-processes)))))
    (start-process " *xscreensaver" nil "xscreensaver" "-no-splash")
    (sit-for 1))
  (start-process " *xscreensaver-command" nil "xscreensaver-command" "-lock"))

(defun exwm-screenshot ()
  (interactive)
  (start-process " *screenshot" nil "gnome-screenshot"))

(defun exwm-set-random-wallpaper (path &optional reason)
  (interactive (list (read-directory-name "Random image from: "
                                          exwm-default-wallpaper-folder)))
  (let* ((paths (directory-files path t "^[^.]"))
         (random-picture (nth (random (length paths)) paths)))
   (start-process " *feh" " *feh outputs*" "feh" "--bg-fill"
                  random-picture)
   (let ((inhibit-message t))
     (message "EXWM wallpaper%s: %s" (if reason
                                         (concat " (" reason ")")
                                       "")
              (abbreviate-file-name random-picture)))))

(defun exwm-set-window-transparency (buffer &optional opacity)
  (interactive (list (current-buffer)
                     (read-number "Opacity: " exwm-default-transparency)))
  (let ((window-id (exwm--buffer->id buffer)))
    (if window-id
        (start-process " *transset" " *transset outputs*"
                       "transset" "--id"
                       (int-to-string window-id)
                       (int-to-string (or opacity exwm-default-transparency)))
      (message "Buffer %s without window." (buffer-name buffer)))))

(defun exwm-toggle-transparency ()
  (interactive)
  (if (= 1 exwm-default-transparency)
      (progn
        (setq exwm-default-transparency 0.85)
        (mapc (lambda (buffer)
                (with-current-buffer buffer
                  (unless (member exwm-instance-name exwm-exclude-transparency)
                    (exwm-set-window-transparency buffer exwm-default-transparency))))
              (exwm-buffer-list)))
    (setq exwm-default-transparency 1)
    (mapc 'exwm-set-window-transparency (exwm-buffer-list))))

(defun exwm-xrandr-parse ()
  (let ((monitors (make-hash-table :test 'equal)))
    (with-temp-buffer
      (call-process "xrandr" nil t nil)
      (goto-char (point-min))
      (while (re-search-forward "\n\\([^ ]+\\) connected " nil 'noerror)
        (let ((monitor (make-hash-table :test 'eq))
              (monitor-name (match-string 1)))
          (let ((primary (string-equal "primary" (thing-at-point 'word))))
            (puthash 'primary primary monitor)
            (when primary
              (forward-word)
              (forward-char)))
          (let* ((resolution-pos (thing-at-point 'sexp))
                 (values (split-string resolution-pos "+")))
            (puthash 'resolution (nth 0 values) monitor)
            (puthash 'x (nth 1 values) monitor)
            (puthash 'y (nth 2 values) monitor))
          (forward-line)
          (forward-word)
          (puthash 'max (thing-at-point 'sexp) monitor)
          (puthash monitor-name monitor monitors))))
    monitors))

(require 'crm)
(defun exwm-update-screens ()
  (interactive)
  (let* ((monitors (exwm-xrandr-parse))
         (names (hash-table-keys monitors)))
    (if (called-interactively-p 'interactive)
        (setq exwm-default-monitor-order
              (or (completing-read-multiple
                   (concat
                    "External monitor order (" crm-separator "): ")
                   names
                   nil t)
                  exwm-default-monitor-order)))
    (if (null exwm-default-monitor-order)
        (setq exwm-default-monitor-order
              (list
               (cl-some (lambda (name)
                          (if (gethash 'primary (gethash name monitors))
                              name))
                        names))))
    (let* ((names (cl-remove-if-not (lambda (name)
                                      (member name names))
                                    exwm-default-monitor-order))
           (posx 0)
           (gety-lambda
            (lambda (name)
              (string-to-number
               (nth 1 (split-string
                       (gethash 'max (gethash name monitors)) "x")))))
           (ymax
            (apply 'max
                   (mapcar
                    gety-lambda
                    names)))
           (args
            (apply
             'nconc
             (mapcar
              (lambda (name)
                (let* ((monitor (gethash name monitors))
                       (max-resolution (gethash 'max monitor)))
                  (prog1
                      (list "--output" name
                            "--mode" max-resolution
                            "--pos" (concat (number-to-string posx)
                                            "x"
                                            (number-to-string
                                             (- ymax (funcall gety-lambda name))))
                            "--rotate" "normal")
                    (setq posx (+ posx (string-to-number
                                        (nth 0 (split-string
                                                (gethash 'max monitor) "x"))))))))
              names))))
      (apply 'call-process "xrandr" nil nil nil args)
      (if exwm-randr-workspace-monitor-plist
          (exwm-set-random-wallpaper exwm-default-wallpaper-folder "update"))
      (setq exwm-randr-workspace-monitor-plist nil)
      (let ((monitor-number -1))
        (mapc (lambda (name)
                (setq exwm-randr-workspace-monitor-plist
                      (nconc exwm-randr-workspace-monitor-plist
                             (list (cl-incf monitor-number) name))))
              names)
        (setq exwm-workspace-number (1+ monitor-number))))))

(defun exwm-update-minibuffer-monitor ()
  (interactive)
  (if (and (< 0 exwm-default-minibuffer-number)
           (> exwm-workspace-number exwm-default-minibuffer-number))
      (exwm-workspace-swap (exwm-workspace--workspace-from-frame-or-index 0)
                           (exwm-workspace--workspace-from-frame-or-index
                            exwm-default-minibuffer-number))))
(advice-add #'exwm-randr--init :after 'exwm-update-minibuffer-monitor)

(defun exwm-screen-count ()
  (let ((monitor-number 0))
    (with-temp-buffer
      (call-process "xrandr" nil t nil)
      (goto-char (point-min))
      (while (re-search-forward "\n\\([^ ]+\\) connected " nil 'noerror)
        (cl-incf monitor-number)
        (forward-line)))
    monitor-number))

(defun exwm-workspace-index-plus (arg)
  (let* ((workspace-count (exwm-workspace--count))
         (remainer (% (+ arg exwm-workspace-current-index) workspace-count)))
    (if (< remainer 0)
        (+ remainer workspace-count)
      remainer)))

(defun exwm-workspace-next ()
  (interactive)
  (exwm-workspace-switch (exwm-workspace-index-plus 1)))

(defun exwm-workspace-prev ()
  (interactive)
  (exwm-workspace-switch (exwm-workspace-index-plus -1)))

(defun exwm-randr-workspace-move (workspace monitor)
  (setq exwm-randr-workspace-monitor-plist
        (plist-put exwm-randr-workspace-monitor-plist workspace monitor)))

(defun exwm-randr-workspace-move-current (monitor)
  (interactive (list (let* ((result (if exwm-randr--compatibility-mode
                                        (exwm-randr--get-outputs)
                                      (exwm-randr--get-monitors)))
                            (primary-monitor (elt result 0))
                            (monitor-list (mapcar 'car (elt result 2))))
                       (completing-read "Move to monitor: "
                                        monitor-list nil t nil nil primary-monitor))))
  (exwm-randr-workspace-move exwm-workspace-current-index monitor)
  (exwm-randr-refresh))

(defun exwm-buffer-p (buffer-or-name)
  (with-current-buffer buffer-or-name
    (derived-mode-p 'exwm-mode)))

(defun exwm-buffer-list ()
  (cl-remove-if-not 'exwm-buffer-p (buffer-list)))

(defun exwm-display-buffer-condition (buffer-name &optional action)
  (and (exwm-buffer-p buffer-name)
       (exwm-buffer-p (current-buffer))))

(defun exwm-display-buffer-function (buffer &optional alist)
  (let ((avaible-window-list
         (cl-remove-if
          #'window-dedicated-p
          (delete
           (selected-window)
           (apply #'append (mapcar #'window-list (visible-frame-list)))))))
    (if avaible-window-list
        (if (< 1 (length avaible-window-list))
            (let* ((window-width-list (mapcar (lambda (w)
                                                (+ (* (window-width w) 10) (window-height w)))
                                              avaible-window-list))
                   (window (nth (cl-position
                                 (seq-max window-width-list)
                                 window-width-list) avaible-window-list)))
              (select-frame (window-frame window))
              (set-window-buffer window buffer))
          (select-frame (window-frame (car avaible-window-list)))
          (set-window-buffer (car avaible-window-list) buffer))
      (display-buffer-pop-up-window buffer alist))))

(defun exwm-windows-processes ()
  (cl-remove-if-not (lambda (p)
                      (and (eq 'run (process-status p))
                           (process-tty-name p)
                           (null (process-buffer p))))
                    (process-list)))

(defun exwm-kill-emacs-query-function ()
  (mapc #'interrupt-process (exwm-windows-processes))
  (let (processes)
    (while (setq processes (exwm-windows-processes))
      (sit-for 0.1)
      (message "Waiting processes: %s" (mapconcat #'process-name processes ", "))))
  (message "All processes terminated.")
  t)

(defun exwm-start-process (command)
  (interactive (list (read-shell-command "> ")))
  (cond ((string-match-p "\\\\ " command)
         (start-process-shell-command command nil command))
        ((string-match-p "\"" command)
         (let ((split (split-string-and-unquote command)))
           (apply #'start-process (car split) nil (pop split) split)))
        (t
         (let ((split (split-string command)))
           (apply #'start-process (car split) nil (pop split) split)))))

(defun exwm-ace-window (arg)
  (interactive "p")
  (if (and (derived-mode-p 'exwm-mode)
           (eq exwm--input-mode 'char-mode))
      (let ((id (exwm--buffer->id (window-buffer))))
        (exwm-input-grab-keyboard id)
        (unwind-protect
            (ace-window arg)
          (exwm-input-release-keyboard id)))
    (ace-window arg)))

(defun exwm-shutdown (&optional arg)
  (interactive "P")
  (add-hook 'kill-emacs-hook
            (lambda ()
              (call-process "systemctl" nil nil nil "poweroff")) t)
  (save-buffers-kill-terminal-with-choice arg))

;; display buffer rules
(push '(exwm-display-buffer-condition exwm-display-buffer-function) display-buffer-alist)

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
;; (ido-mode 1)

;; Emacs server is not required to run EXWM but it has some interesting uses
;; (see next section).
(server-start)
(push 'exwm-kill-emacs-query-function kill-emacs-query-functions)

;; (require 'mini-modeline)                     ;; + with mini-modeline
;; (setq mini-modeline-frame (selected-frame))  ;; + with mini-modeline

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
(defun exwm-update-class-defaults ()
  (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
              (string-equal "gimp" exwm-instance-name))
    (exwm-workspace-rename-buffer exwm-class-name))
  (unless (member exwm-instance-name exwm-exclude-transparency)
    (exwm-set-window-transparency (current-buffer) exwm-default-transparency))
  (when (member exwm-instance-name exwm-hide-mode-line)
    (setq mode-line-format nil)))
(add-hook 'exwm-update-class-hook 'exwm-update-class-defaults)

(defun exwm-update-title-defaults ()
  (when (or (not exwm-instance-name)
            (string-prefix-p "sun-awt-X11-" exwm-instance-name)
            (string-equal "gimp" exwm-instance-name))
    (exwm-workspace-rename-buffer exwm-title)))
(add-hook 'exwm-update-title-hook 'exwm-update-title-defaults)

(with-eval-after-load 'exwm-input
  ;; line-mode prefix keys
  (push ?\M-o exwm-input-prefix-keys)
  ;; Global keybindings can be defined with `exwm-input-global-keys'.
  ;; Here are a few examples:
  (setq exwm-input-global-keys
        `(;; Universal argument
          ([?\s-u] . universal-argument)
          ;; Bind "s-r" to exit char-mode and fullscreen mode.
          ([?\s-r] . exwm-reset)
          ;; Bind "s-w" to switch workspace interactively.
          ([?\s-w] . exwm-workspace-switch)
          ;; Bind "s-1" to "s-0" to switch to a workspace by its index.
          ([?\s-0] . (lambda ()
                       (interactive)
                       (exwm-workspace-switch 9)))
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "s-%d" (1+ i))) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch ,i))))
                    (number-sequence 0 8))
          (,(kbd "S-s-0") . (lambda ()
                              (interactive)
                              (exwm-workspace-switch-create 9)))
          ,@(mapcar (lambda (i)
                      `(,(kbd (format "S-s-%d" (1+ i))) .
                        (lambda ()
                          (interactive)
                          (exwm-workspace-switch-create ,i))))
                    (number-sequence 0 8))
          ;; Bind "s-&" to launch applications ('M-&' also works if the output
          ;; buffer does not bother you).
          ([?\s-&] . exwm-start-process)
          ;; Bind "s-<f2>" to "slock", a simple X display locker.
          ([s-f2] . (lambda ()
                      (interactive)
                      (start-process "" nil "/usr/bin/slock")))
          ;; Toggle char-line modes
          ([?\s-q] . exwm-input-toggle-keyboard)
          ;; Display datetime
          ([?\s-a] . display-time-mode)
          ;; Workspaces
          ([?\s-n] . exwm-workspace-next)
          ([?\s-p] . exwm-workspace-prev)
          ([?\s-s] . exwm-workspace-swap)
          ([?\s-m] . exwm-randr-workspace-move-current)
          ;; ace-window
          ([?\s-o] . exwm-ace-window)
          ;; Bind lock screen
          (,(kbd "<s-escape>") . exwm-screensaver-lock)
          ;; Screenshot
          (,(kbd "<s-print>") . exwm-screenshot)
          ;; Record audio and video
          (,(kbd "<S-s-print>") . exwm-record-toggle)
          ;; Execute command menu
          ([?\s-x] . ,(if (featurep 'helm) 'helm-M-x 'execute-extended-command))
          ;; shutdown computer
          (,(kbd "<s-end>") . exwm-shutdown))))

(with-eval-after-load 'exwm-manage
  (setq exwm-manage-configurations
        '(((member exwm-class-name '("XTerm" "Emacs")) char-mode t)
          ((member exwm-class-name
                   '("darkplaces" "doom" "gzdoom"))
           floating nil))))

;; To add a key binding only available in line-mode, simply define it in
;; `exwm-mode-map'.  The following example shortens 'C-c q' to 'C-q'.
(define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)

;; The following example demonstrates how to use simulation keys to mimic
;; the behavior of Emacs.  The value of `exwm-input-simulation-keys` is a
;; list of cons cells (SRC . DEST), where SRC is the key sequence you press
;; and DEST is what EXWM actually sends to application.  Note that both SRC
;; and DEST should be key sequences (vector or string).
(setq exwm-input-simulation-keys
      `(;; movement
        ([?\C-b] . [left])
        ([?\M-b] . [C-left])
        ([?\C-f] . [right])
        ([?\M-f] . [C-right])
        ([?\C-p] . [up])
        ([?\C-n] . [down])
        (,(kbd "C-a") . [home])
        ([?\M-<] . [C-home])
        ([?\C-e] . [end])
        ([?\M->] . [C-end])
        ([?\M-v] . [prior])
        ([?\C-v] . [next])
        ([?\C-d] . [delete])
        ([?\C-k] . [S-end delete])
        ;; jumps
        (,(kbd "M-g M-g") . [?\C-g])
        (,(kbd "M-g M-n") . ,(kbd "<f8>"))
        (,(kbd "M-g M-p") . ,(kbd "<S-f8>"))
        (,(kbd "M-.") . ,(kbd "<C-f12>"))
        (,(kbd "C-,") . ,(kbd "C-S--"))
        (,(kbd "C-.") . ,(kbd "C-M--"))
        (,(kbd "C-x C-SPC") . ,(kbd "C-M--"))
        ;; comments
        (,(kbd "M-;") . ,(kbd "M-S-a"))
        ;; select
        ([?\C-x ?h] . [?\C-a])
        ;; cut/paste
        ([?\C-w] . [?\C-x])
        ([?\M-w] . [?\C-c])
        ([?\C-y] . [?\C-v])
        ;; search
        ([?\C-s] . [?\C-f])
        ;; files
        ([?\C-x ?\C-s] . [?\C-s])
        ;; undo redo
        (,(kbd "C-_") . [?\C-z])
        (,(kbd "M-_") . [?\C-y])
        ;; format
        (,(kbd "M-SPC") . ,(kbd "C-S-i"))))

;; You can hide the minibuffer and echo area when they're not used, by
;; uncommenting the following line.
;(setq exwm-workspace-minibuffer-position 'bottom)

;; Do not forget to enable EXWM. It will start by itself when things are
;; ready.  You can put it _anywhere_ in your configuration.
(exwm-enable)

;; Multi-monitor
(add-hook 'exwm-randr-screen-change-hook 'exwm-update-screens)
(exwm-randr-enable)

;; System tray
(require 'exwm-systemtray)
(exwm-systemtray-enable)

;; System monitor
(require 'symon)
;; (defun message-advice (orig-fun format-string &rest args)
;;   (if format-string
;;       (apply orig-fun format-string args)))
;; (advice-add #'message :around 'message-advice)

;; (defvar symon--minibuffer-window
;;   (minibuffer-window (car exwm-workspace--list)))
;; (defun symon-message-trick (format-string &rest args)
;;   (if (not (cdr exwm-workspace--list))
;;       (apply #'message format-string args)
;;     (if (null symon--minibuffer-window)
;;         (setq symon--minibuffer-window
;;               (minibuffer-window (car exwm-workspace--list))))
;;     (with-selected-window symon--minibuffer-window
;;       (delete-region (minibuffer-prompt-end) (point-max))
;;       (insert (apply #'format-message format-string args)))))

(defvar symon--datetime-monitor-pulse nil)
(define-symon-monitor symon-current-datetime-monitor
  :interval 10
  :display (if (setq symon--datetime-monitor-pulse
                     (null symon--datetime-monitor-pulse))
               (format-time-string "%e %b %H:%M.")
             (format-time-string "%e %b %H:%M ")))

(define-symon-monitor symon-org-clock-in-monitor
  :interval 10
  :display (if (bound-and-true-p org-clock-mode-line-timer)
               org-mode-line-string))

(define-symon-monitor symon-venv-current-name-monitor
  :interval 10
  :display (if (and (boundp 'venv-current-name)
                    venv-current-name
                    (not (string-empty-p venv-current-name)))
               (concat "[" (propertize venv-current-name 'face 'mode-line-correct) "]")))

(define-symon-monitor symon-recording-monitor
  :display (if (and exwm-record-process
                    (eq 'run (process-status exwm-record-process)))
               exwm-record-recording))

(setcdr (last symon-monitors)
        `(,(cond ((memq system-type '(gnu/linux cygwin))
                  'symon-linux-battery-monitor)
                 ((memq system-type '(darwin))
                  'symon-darwin-battery-monitor)
                 ((memq system-type '(windows-nt))
                  'symon-windows-battery-monitor))
          symon-current-datetime-monitor))

(push 'symon-org-clock-in-monitor symon-monitors)
(push 'symon-venv-current-name-monitor symon-monitors)
(push 'symon-recording-monitor symon-monitors)

(setq symon-delay 0.5
      symon-refresh-rate 4
      symon-sparkline-type 'bounded
      symon-sparkline-thickness 1
      symon-history-size 24
      symon-sparkline-width 24
      symon-total-spark-width 12)

(symon-mode)

;; Background
(defvar exwm-timer-random-wallpaper nil
  "Random wallpaper timer")

(defun exwm-start-random-wallpaper ()
  (interactive)
  (if exwm-timer-random-wallpaper
      (message "Exists previous random wallpaper timer")
    (setq exwm-timer-random-wallpaper
          (run-at-time 600 600
                       'exwm-set-random-wallpaper
                       exwm-default-wallpaper-folder
                       "timer"))))
(exwm-start-random-wallpaper)

(defun exwm-cancel-random-wallpaper ()
  (interactive)
  (if (null exwm-timer-random-wallpaper)
      (message "Nil random wallpaper timer")
    (cancel-timer exwm-timer-random-wallpaper)
    (setq exwm-timer-random-wallpaper nil)))

;; Applications
(dolist (program-and-args-list '(("compton")
                                 ("volumeicon")
                                 ("nm-applet")))
  (let ((executable (car program-and-args-list)))
   (if (executable-find executable)
      (apply 'start-process
             (concat " *" executable)
             (concat " *" executable " outputs*")
             program-and-args-list)
    (message "Unable to find `%s' executable." executable))))

(when (load "helm-exwm" nil t)
  (setq helm-exwm-emacs-buffers-source (helm-exwm-build-emacs-buffers-source)
        helm-exwm-source (helm-exwm-build-source)
        helm-mini-default-sources `(helm-exwm-emacs-buffers-source
                                    helm-exwm-source
                                    helm-source-recentf)))

(when (featurep 'helm-posframe)
  (defvar exwm-helm-posframe-display-buffer nil)
  (defun helm-posframe-display-advice (&rest args)
    (let ((buffer (current-buffer)))
      (when (exwm-buffer-p buffer)
        (exwm-set-window-transparency buffer 0.2)
        (setq exwm-helm-posframe-display-buffer buffer))))
  (advice-add 'helm-posframe-display :before 'helm-posframe-display-advice)

  (defun helm-posframe-cleanup-advice (&rest args)
    (when exwm-helm-posframe-display-buffer
      (with-current-buffer exwm-helm-posframe-display-buffer
        (exwm-set-window-transparency
         exwm-helm-posframe-display-buffer
         (if (member exwm-instance-name exwm-exclude-transparency)
             1 exwm-default-transparency)))
      (setq exwm-helm-posframe-display-buffer nil)))
  (advice-add 'helm-posframe-cleanup :after 'helm-posframe-cleanup-advice))

(when (featurep 'winum)
  (defun exwm-winum-bindings ()
    (if winum-mode
        (winum--define-keys exwm-mode-map)
      (winum--undefine-keys exwm-mode-map)))
  (exwm-winum-bindings)
  (add-hook 'winum-mode-hook 'exwm-winum-bindings))

;; minibuffer
(setq mini-frame-show-parameters
      '((left . -1) (top . -1) (width . 0.75) (height . 1) (alpha . 75)
        (border-width . 0) (internal-border-width . 0)
        (background-color . "black"))
      mini-frame-ignore-commands '("edebug-eval-expression"
                                   debugger-eval-expression
                                   "exwm-workspace-"))
(mini-frame-mode)

(defun common-minibuffer-all-frames ()
  (let ((frame (car (minibuffer-frame-list))))
    (setf (alist-get 'minibuffer default-frame-alist)
          (if frame nil t))))
(add-hook 'before-make-frame-hook 'common-minibuffer-all-frames)

;; systemtray hold
(defun exwm-systemtray--on-workspace-switch-advice (orig-fun &rest args)
  (if (eq exwm-workspace--current (window-frame (minibuffer-window)))
      (apply orig-fun args)))
(advice-add #'exwm-systemtray--on-workspace-switch :around 'exwm-systemtray--on-workspace-switch-advice)

;; helm integration
(when (featurep 'helm)
  (when (bug-check-function-bytecode
         'helm-resolve-display-function
         "csYgcYgIKYY5AIkJPoQ0AAqDHADHyAshIYQ0AAyDLAANhCwAySBHylaENAAODssgnYQ2AMyHzcAhhw==")
    (defun helm-resolve-display-function (com)
      (or (with-helm-buffer helm-display-function)
          (default-value 'helm-display-function))))

  (when (bug-check-function-bytecode
         'helm-display-mode-line
         "xsAhiMcCPIMRAMjJBCKGFADKwCEDIhDLAiGEKgAJhSsAyMwDIgqdhSsAzQuFXQDIzAQiC86JiQM6g1kAA0CyA8jMBEAisgIBBZiDUgACAUKyAQNBsgSCNwCJn7aFCIOhAM/Q0dDSBgbQ0wYIhXkA1NXWBgtHItfYI0TT2QzaQkJE20JCQkJCQkJC3EJCFd0IPIOaAAhBQIKbAAghFiaCpQDKxSEVDieDswDesgPfIIiC1gAOKIPWAMcEPIXDAMjgBgYiBSLh4iDjItTQAwNR1+QjFim2ArYCiYXeAOUghw==")
    (defun helm-display-mode-line (source &optional force)
      "Set up mode line and header line for `helm-buffer'.

SOURCE is a Helm source object.

Optional argument FORCE forces redisplay of the Helm buffer's
mode and header lines."
      (set (make-local-variable 'helm-mode-line-string)
           (helm-interpret-value (or (and (listp source) ; Check if source is empty.
                                          (assoc-default 'mode-line source))
                                     (default-value 'helm-mode-line-string))
                                 source))
      (let ((follow (and (or (helm-follow-mode-p source)
                             (and helm-follow-mode-persistent
                                  (member (assoc-default 'name source)
                                          helm-source-names-using-follow)))
                         " (HF)"))
            (marked (and helm-marked-candidates
                         (cl-loop with cur-name = (assoc-default 'name source)
                                  for c in helm-marked-candidates
                                  for name = (assoc-default 'name (car c))
                                  when (string= name cur-name)
                                  collect c))))
        ;; Setup mode-line.
        (if helm-mode-line-string
            (setq mode-line-format
                  `(:propertize
                    ;; (" " mode-line-buffer-identification " "  ;; -
                    (                                            ;; +
                     (:eval (format "L%-3d" (helm-candidate-number-at-point)))
                     ,follow
                     " "
                     (:eval ,(and marked
                                  (propertize
                                   (format "M%d" (length marked))
                                   'face 'helm-visible-mark)))
                     (:eval (when ,helm--mode-line-display-prefarg
                              (let ((arg (prefix-numeric-value
                                          (or prefix-arg current-prefix-arg))))
                                (unless (= arg 1)
                                  (propertize (format " [prefarg:%s]" arg)
                                              'face 'helm-prefarg)))))
                     " "
                     (:eval (with-helm-buffer
                              (helm-show-candidate-number
                               (car-safe helm-mode-line-string))))
                     " " helm--mode-line-string-real " "
                     (:eval (make-string (window-width) ? )))
                    keymap (keymap (mode-line keymap
                                              (mouse-1 . ignore)
                                              (down-mouse-1 . ignore)
                                              (drag-mouse-1 . ignore)
                                              (mouse-2 . ignore)
                                              (down-mouse-2 . ignore)
                                              (drag-mouse-2 . ignore)
                                              (mouse-3 . ignore)
                                              (down-mouse-3 . ignore)
                                              (drag-mouse-3 . ignore))))
                  helm--mode-line-string-real
                  (substitute-command-keys (if (listp helm-mode-line-string)
                                               (cadr helm-mode-line-string)
                                             helm-mode-line-string)))
          (setq mode-line-format (default-value 'mode-line-format)))
        ;; Setup header-line.
        (cond (helm-echo-input-in-header-line
               (setq force t)
               (helm--set-header-line))
              (helm-display-header-line
               (let ((hlstr (helm-interpret-value
                             (and (listp source)
                                  (assoc-default 'header-line source))
                             source))
                     (endstr (make-string (window-width) ? )))
                 (setq header-line-format
                       (propertize (concat " " hlstr endstr)
                                   'face 'helm-header))))))
      (when force (force-mode-line-update)))))

;;;;;;;;;;
;; Keys ;;
;;;;;;;;;;
(global-set-key (kbd "<f7> T") 'exwm-toggle-transparency)


(provide 'exwm-startup-config)
