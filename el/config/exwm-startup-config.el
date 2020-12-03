;;; exwm-startup-config.el --- Provides EXWM improvements

;;; Commentary:

;;; Code:

;; Below are configurations for EXWM.
(message "Importing exwm-startup-config")
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

;;;;;;;;;;;
;; Faces ;;
;;;;;;;;;;;
(defface exwm-record-face
  '((t :foreground "red2"))
  "Basic face used to highlight errors and to denote failure."
  :group 'exwm)

;;;;;;;;;;;;;;;
;; Variables ;;
;;;;;;;;;;;;;;;
(defvar exwm-close-window-on-kill nil
  "EXWM close window when kill buffer.")

(defvar exwm-exclude-transparency '("totem" "vlc" "darkplaces" "doom" "gzdoom")
  "EXWM instances without transparency.")

(defvar exwm-default-transparency 0.85
  "EXWM default transparency.")

;; example: export EXWM_MONITOR_ORDER="eDP-1 HDMI-1 DP-1"
(defvar exwm-default-monitor-order
  (let ((monitor-order (getenv "EXWM_MONITOR_ORDER")))
    (if monitor-order
        (condition-case nil
            (split-string monitor-order  " ")
          (error nil))))
  "EXWM default monitor order.")

;; example: export EXWM_MONITOR_RESOLUTION="HDMI-1 1280x720 DP-1 800x600"
(defvar exwm-default-monitor-resolution
  (let ((monitor-resolution (getenv "EXWM_MONITOR_RESOLUTION")))
    (if monitor-resolution
        (condition-case nil
            (split-string monitor-resolution  " ")
          (error nil))))
  "EXWM default monitor resolution.")

;; example: export EXWM_MINIBUFFER_NUMBER="1"
;; example: export EXWM_MINIBUFFER_NUMBER="eDP-1"
(defvar exwm-default-minibuffer-workspace-or-screen
  (let ((workspace-or-screen
         (getenv "EXWM_MINIBUFFER_WORKSPACE_OR_SCREEN")))
    (if workspace-or-screen
        (or (cl-parse-integer
             workspace-or-screen :junk-allowed t)
            workspace-or-screen)
      0))
  "EXWM default minibuffer workspace number.")

(defvar exwm-default-wallpaper-folder "~/Pictures/backgrounds/"
  "EXWM default wallpaper folder.")

(defvar exwm-screensaver-process nil
  "EXWM screensaver process.")

(defvar exwm-record-process nil
  "EXWM record process when recording.")

(defvar exwm-record-recording (propertize "⏺" 'face 'exwm-record-face)
  "EXWM recording text displayed while recording")

;;;;;;;;;;;;;;;
;; Functions ;;
;;;;;;;;;;;;;;;
(defun exwm-set-border-color (color &optional buffer)
  "Set BUFFER border COLOR color."
  (when-let ((id (car (rassoc (or buffer (current-buffer))
                              exwm--id-buffer-alist))))
    (xcb:+request exwm--connection
        (make-instance 'xcb:ChangeWindowAttributes
                       :window id
                       :value-mask xcb:CW:BorderPixel
                       :border-pixel (exwm--color->pixel color)))))

(defun exwm-set-border-width (border-width &optional buffer)
  "Set BUFFER border BORDER-WIDTH width."
  (when-let (id (car (rassoc (or buffer (current-buffer))
                             exwm--id-buffer-alist)))
    (xcb:+request exwm--connection
        (make-instance 'xcb:ConfigureWindow
                       :window id
                       :value-mask xcb:ConfigWindow:BorderWidth
                       :border-width border-width))))

(when (bug-check-function-bytecode
       'exwm-layout-toggle-fullscreen
       "wzJKAAiDHADExQmDEwAJIIIUAMbHBIYaAMgkiImEKwDJyiGEKwDLw8wiiImFSQByic0BCiJBsgFxiM4gg0UAzwEhgkgA0AEhKTCH")
  (cl-defun exwm-layout-toggle-fullscreen (&optional id)
    "Toggle fullscreen mode."
    (interactive (list (exwm--buffer->id (window-buffer))))
    (exwm--log "id=#x%x" (or id 0))
    (unless (or id (derived-mode-p 'exwm-mode))
      (cl-return-from exwm-layout-toggle-fullscreen))
    (when id
      (with-current-buffer (exwm--id->buffer id)
        (if (exwm-layout--fullscreen-p)
            (progn
              (exwm-randr-refresh)
              (exwm-layout-unset-fullscreen id))
          (let ((exwm-gap-monitor 0))
            (exwm-randr-refresh))
          (exwm-layout-set-fullscreen id))))))

(when (bug-check-function-bytecode
       'exwm-layout-unset-fullscreen
       "xjKcAAiDHADHyAmDEwAJIIIUAMnKBIYaAMskiImEJgDMzSGDKwDOIIQwAM/G0CKIcomDQACJ0QEKIkGyAYJCANIgcYjTCwwiFA2DWADUDiXVDSEiiIKAANYOJtfY2Q4l2tsOJw4oItwOKd0OKiYJIoje0N8iiYN/ANQOJQIiiIjgASGI4Q4mIYji3iDQIogOK+M9hZoA5A4lISkwhw==")
  (cl-defun exwm-layout-unset-fullscreen (&optional id)
    "Restore window from fullscreen state."
    (interactive)
    (exwm--log "id=#x%x" (or id 0))
    (unless (and (or id (derived-mode-p 'exwm-mode))
                 (exwm-layout--fullscreen-p))
      (cl-return-from exwm-layout-unset-fullscreen))
    (with-current-buffer (if id (exwm--id->buffer id) (window-buffer))
      (setq exwm--ewmh-state
            (delq xcb:Atom:_NET_WM_STATE_FULLSCREEN exwm--ewmh-state))
      (if exwm--floating-frame
          (exwm-layout--show exwm--id (frame-root-window exwm--floating-frame))
        (xcb:+request exwm--connection
            (make-instance 'xcb:ConfigureWindow
                           :window exwm--id
                           :value-mask (logior xcb:ConfigWindow:Sibling
                                               xcb:ConfigWindow:StackMode)
                           :sibling exwm--guide-window
                           :stack-mode xcb:StackMode:Above))
        (let ((window (get-buffer-window nil t)))
          (when window
            (exwm-layout--show exwm--id window))))
      (xcb:+request exwm--connection
          (make-instance 'xcb:ewmh:set-_NET_WM_STATE
                         :window exwm--id
                         :data exwm--ewmh-state))
      (xcb:flush exwm--connection)
      (set-window-dedicated-p (get-buffer-window) nil)
      (when (eq 'line-mode exwm--selected-input-mode)
        (exwm-input--grab-keyboard exwm--id)))))

(when (bug-check-function-bytecode
       'exwm-randr-refresh
       "CIMTAMbHCYMPAAkgghAAyMkjiAqDHADKIIIeAMsgicycAc2cAs6cz4kEhRsBA4UbAQuDNwDMEwxHzIkCV4OXAInQDQIi0QEGCSJBDAOc0gHTIgKDYQDRBAYLIkGyBIJuAAYLsgTRBgwGDCJBsgOJ1AUhQkMGCaSyCYkCQkMGCKSyCNUC1gYGI4jVAtcFI7YGiVSyAYI6ALYC2CCIDImDrgCJQNkBIYgBQbaCgp0AiNoOLCGI2yCDwQDcIIPBAN0giN4giAyJg9cAiUDfAc8iiAFBtoKCxQCI4OHiDizjDizk5eYOLSMizyNA5yIhiYMSAYlAiQSeQYmDCgHoAQYGIrIF3wIFnkHpIoiIAUG2goLtAIjaDiwhiOrrIYc=")
  (defvar exwm-gap-monitor 20)
  (defun exwm-randr-refresh ()
    "Refresh workspaces according to the updated RandR info."
    (interactive)
    (exwm--log)
    (let* ((result (if exwm-randr--compatibility-mode
                       (exwm-randr--get-outputs)
                     (exwm-randr--get-monitors)))
           (primary-monitor (elt result 0))
           (monitor-geometry-alist (elt result 1))
           (monitor-alias-alist (elt result 2))
           container-monitor-alist container-frame-alist)
      (when (and primary-monitor monitor-geometry-alist)
        (when exwm-workspace--fullscreen-frame-count
          ;; Not all workspaces are fullscreen; reset this counter.
          (setq exwm-workspace--fullscreen-frame-count 0))
        (dotimes (i (exwm-workspace--count))
          (let* ((monitor (plist-get exwm-randr-workspace-monitor-plist i))
                 (geometry (cdr (assoc monitor monitor-geometry-alist)))
                 (frame (elt exwm-workspace--list i))
                 (container (frame-parameter frame 'exwm-container)))
            (if geometry
                ;; Unify monitor names in case it's a mirroring setup.
                (setq monitor (cdr (assoc monitor monitor-alias-alist)))
              ;; Missing monitors fallback to the primary one.
              (setq monitor primary-monitor
                    geometry (cdr (assoc primary-monitor
                                         monitor-geometry-alist))))
            (setq container-monitor-alist (nconc
                                           `((,container . ,(intern monitor)))
                                           container-monitor-alist)
                  container-frame-alist (nconc `((,container . ,frame))
                                               container-frame-alist))
            (set-frame-parameter frame 'exwm-randr-monitor monitor)
            (set-frame-parameter
             frame 'exwm-geometry
             (with-slots (x y width height) geometry
               (make-instance 'xcb:RECTANGLE
                              :x (and x (+ x exwm-gap-monitor))
                              :y (and y (+ y exwm-gap-monitor))
                              :width (and width
                                          (- width
                                             (* 2 exwm-gap-monitor)))
                              :height (and height
                                           (- height
                                              (* 2 exwm-gap-monitor))))))))
        ;; Update workareas.
        (exwm-workspace--update-workareas)
        ;; Resize workspace.
        (dolist (f exwm-workspace--list)
          (exwm-workspace--set-fullscreen f))
        (xcb:flush exwm--connection)
        ;; Raise the minibuffer if it's active.
        (when (and (active-minibuffer-window)
                   (exwm-workspace--minibuffer-own-frame-p))
          (exwm-workspace--show-minibuffer))
        ;; Set _NET_DESKTOP_GEOMETRY.
        (exwm-workspace--set-desktop-geometry)
        ;; Update active/inactive workspaces.
        (dolist (w exwm-workspace--list)
          (exwm-workspace--set-active w nil))
        ;; Mark the workspace on the top of each monitor as active.
        (dolist (xwin
                 (reverse
                  (slot-value (xcb:+request-unchecked+reply exwm--connection
                                  (make-instance 'xcb:QueryTree
                                                 :window exwm--root))
                              'children)))
          (let ((monitor (cdr (assq xwin container-monitor-alist))))
            (when monitor
              (setq container-monitor-alist
                    (rassq-delete-all monitor container-monitor-alist))
              (exwm-workspace--set-active (cdr (assq xwin container-frame-alist))
                                          t))))
        (xcb:flush exwm--connection)
        (run-hooks 'exwm-randr-refresh-hook))))
  )

(defun exwm-record-stop ()
  (interactive)
  (when exwm-record-process
    (interrupt-process exwm-record-process)
    (message "EXWM Record process interrupted")))

(defun exwm-record-start (monitor pcm-device)
  (interactive (list (let ((monitors (exwm-xrandr-parse)))
                       (gethash (completing-read
                                 "Select monitor: "
                                 (hash-table-keys monitors)
                                 nil t)
                                monitors))
                     (completing-read
                      "Select audio input: "
                      (split-string
                       (shell-command-to-string
                        "arecord -L | grep -v -E \"^[[:space:]]\"")
                       "\n" t)
                      nil t nil nil "default")))
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
         "-f" "pulse" "-ac" "2" "-i" pcm-device
         ;; audio codec
         "-codec:a" "copy"
         ;; video codec
         "-codec:v" "libx264"
         ;; options
         "-crf" "0" "-preset" "ultrafast"
         "-threads" "4"
         (expand-file-name (concat
                            "Capture_"
                            (gethash 'resolution monitor)
                            (format-time-string "_%Y-%m-%d_%H.%M.%S.mkv"))
                           (if (file-directory-p "~/Videos/")
                               "~/Videos/"
                             "~/"))))
  (if (eq 'run (process-status exwm-record-process))
      (message "EXWM Record process started")
    (message "EXWM Record process failed")))

(defun exwm-record-toggle ()
  (interactive)
  (if exwm-record-process
      (if (eq 'run (process-status exwm-record-process))
          (exwm-record-stop)
        (call-interactively 'exwm-record-start))
    (call-interactively 'exwm-record-start)))

(defun exwm-screensaver-lock ()
  (interactive)
  (when (not (setq exwm-screensaver-process
                   (car
                    (member "xscreensaver"
                            (mapcar
                             (lambda (item) (cdr (assoc 'comm item)))
                             (mapcar 'process-attributes (list-system-processes)))))))
    (setq exwm-screensaver-process
          (start-process " *xscreensaver" nil "xscreensaver" "-no-splash"))
    (sit-for 1))
  (start-process " *xscreensaver-command" nil "xscreensaver-command" "-lock"))

(defun exwm-screensaver-interrupt ()
  (interactive)
  (when exwm-screensaver-process
    (interrupt-process exwm-screensaver-process)))

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
                    (exwm-set-window-transparency
                     buffer
                     exwm-default-transparency))))
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

(defun exwm-get-default-monitor-resolution (monitor)
  (let ((pos (cl-position monitor exwm-default-monitor-resolution :test 'string-equal)))
    (if pos
        (nth (1+ pos) exwm-default-monitor-resolution))))

(require 'crm)
(defun exwm-update-screens ()
  (interactive)
  (when (null (member
               "arandr"
               (mapcar (lambda (pid)
                         (cdr (assq 'comm (process-get-attrs pid '(comm)))))
                       (list-system-processes))))
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
                         (or
                          (exwm-get-default-monitor-resolution name)
                          (gethash 'max (gethash name monitors))) "x")))))
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
                         (resolution
                          (or
                           (exwm-get-default-monitor-resolution name)
                           (gethash 'max monitor))))
                    (prog1
                        (list "--output" name
                              "--mode" resolution
                              "--pos" (concat (number-to-string posx)
                                              "x"
                                              (number-to-string
                                               (- ymax (funcall gety-lambda name))))
                              "--rotate" "normal")
                      (setq posx (+ posx
                                    (string-to-number
                                     (nth 0 (split-string
                                             (or
                                              (exwm-get-default-monitor-resolution name)
                                              (gethash 'max monitor)) "x"))))))))
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
          (setq exwm-workspace-number (1+ monitor-number)))))))

(defun exwm-update-minibuffer-monitor ()
  (interactive)
  (cond
   ((and (numberp exwm-default-minibuffer-workspace-or-screen)
         (< 0 exwm-default-minibuffer-workspace-or-screen)
         (> exwm-workspace-number exwm-default-minibuffer-workspace-or-screen))
    (exwm-workspace-swap (exwm-workspace--workspace-from-frame-or-index 0)
                         (exwm-workspace--workspace-from-frame-or-index
                          exwm-default-minibuffer-workspace-or-screen)))
   ((and
     (stringp exwm-default-minibuffer-workspace-or-screen)
     (let ((pos (cl-position exwm-default-minibuffer-workspace-or-screen
                             exwm-randr-workspace-monitor-plist
                             :test 'equal)))
       (if (and pos (/= pos 0))
           (progn
             (exwm-workspace-swap (exwm-workspace--workspace-from-frame-or-index 0)
                                  (exwm-workspace--workspace-from-frame-or-index
                                   (nth (1- pos) exwm-randr-workspace-monitor-plist)))
             t)))))))
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

(defun exwm-display-buffer-condition (buffer-name action)
  (and (exwm-buffer-p buffer-name)
       (let ((buf (current-buffer)))
         (and (null (eq buf (get-buffer buffer-name)))
              (exwm-buffer-p buf)))))

(defun exwm-display-buffer-biggest (buffer alist)
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

(defun exwm-display-buffer-tiling-anticlockwise (buffer alist)
  (with-current-buffer buffer
    (set (make-local-variable 'exwm-close-window-on-kill) t))
  (rotate-frame-anticlockwise)
  (display-buffer-in-direction buffer (cons '(direction . leftmost) alist)))

(defun exwm-display-buffer-cycle (&optional arg)
  (interactive "P")
  (let ((funcs '(exwm-display-buffer-biggest
                 exwm-display-buffer-tiling-anticlockwise
                 ;; display-buffer-pop-up-window
                 ;; display-buffer-at-bottom
                 ;; display-buffer-below-selected
                 ;; display-buffer-in-side-window
                 ;; display-buffer-in-direction
                 ;; display-buffer-same-window
                 )))
    (when arg
      (setq funcs (nreverse funcs)))
    (let* ((display-funcs (cdr (assoc 'exwm-display-buffer-condition
                                      display-buffer-alist)))
           (func (car display-funcs)))
      (if (null func)
          (message "`display-buffer-alist' without EXWM case.")
        (let ((new-func (or (car (cdr (memq func funcs)))
                            (car funcs))))
          (setcar display-funcs new-func)
          (message "EXWM display function: `%s'" new-func))))))

(defun exwm-windows-processes ()
  (cl-remove-if-not (lambda (p)
                      (and (eq 'run (process-status p))
                           (process-tty-name p)
                           ;; (null (process-buffer p))
                           ))
                    (process-list)))

(defun exwm-kill-emacs-query-function ()
  (mapc (lambda (p)
          (let ((sigcgt (string-to-number
                         (substring
                          (string-trim-right
                           (shell-command-to-string
                            (concat "cat /proc/"
                                    (number-to-string (process-id p))
                                    "/status | grep SigCgt | cut -f2")))
                          -1)
                         16)))
            (cond ((= 1 (mod sigcgt 2))
                   (message "Sending `sighup' to `%s' with cgt %i"
                            (process-name p) sigcgt)
                   (signal-process p 'sighup))
                  ((= 1 (mod (/ sigcgt 2) 2))
                   (message "Sending `sigint' to `%s' with cgt %i"
                            (process-name p) sigcgt)
                   (interrupt-process p))
                  (t
                   (message "Sending `sigkill' to `%s' with cgt %i"
                            (process-name p) sigcgt)
                   (kill-process p)))))
        (exwm-windows-processes))
  (let ((times 30)
        last-procs)
    (while (and (<= 0 (cl-decf times))
                (let ((procs (exwm-windows-processes)))
                  (unless (equal last-procs procs)
                    (setq last-procs procs)
                    (message "Waiting processes: %s"
                             (mapconcat #'process-name procs ", ")))
                  procs))
      (sit-for 0.1))
    (if last-procs
        (progn
          (message "Interrupting processes failed.")
          nil)
      (message "All processes closed.")
      t)))

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

(defun exwm-start-terminal (arg)
  (interactive "P")
  (if arg
      (cond ((executable-find "tmux")
             (cond ((executable-find "st")
                    (message "Starting st with tmux")
                    (start-process "st" nil "st" "-e" "tmux"))
                   ((executable-find "urxvt")
                    (message "Starting urxvt with tmux")
                    (start-process "urxvt" nil "urxvt" "-e" "tmux"))))
            ((executable-find "screen")
             (cond ((executable-find "st")
                    (message "Starting st with screen")
                    (start-process "st" nil "st" "-e" "screen"))
                   ((executable-find "urxvt")
                    (message "Starting urxvt with screen")
                    (start-process "urxvt" nil "urxvt" "-e" "screen"))))
            (t (message "Terminal multiplexer not found")))
    (cond ((executable-find "urxvt")
           (message "Starting urxvt")
           (start-process "urxvt" nil "urxvt"))
          ((executable-find "alacritty")
           (message "Starting alacritty")
           (start-process "alacritty" nil "alacritty"))
          ((executable-find "xterm")
           (message "Starting xterm")
           (start-process "xterm" nil "xterm")))))

(defun exwm-start-emacs (filepath)
  (interactive (list (buffer-file-name)))
  (if (and (stringp filepath)
           (null current-prefix-arg))
      (start-process "emacs" nil "emacs" filepath)
    (start-process "emacs" nil "emacs")))

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

(defun exwm-close-window-if-exwm-mode ()
  (when (and (derived-mode-p 'exwm-mode)
             (< 1 (length (window-list)))
             exwm-close-window-on-kill)
    (delete-window)))

(defun exwm-input-mode-change-color ()
  (cl-case exwm--input-mode
    (line-mode (exwm-set-border-color "blue"))
    (char-mode (exwm-set-border-color "red"))))

(defun exwm-selected-window-advice (&rest _args)
  (when (derived-mode-p 'exwm-mode)
    (exwm-set-border-width 1)))
(advice-add 'select-frame :after 'exwm-selected-window-advice)
(advice-add 'select-window :after 'exwm-selected-window-advice)

(defun exwm-unselected-window-advice (&rest _args)
  (when (derived-mode-p 'exwm-mode)
    (exwm-set-border-width 0)))
(advice-add 'select-frame :before 'exwm-unselected-window-advice)
(advice-add 'select-window :before 'exwm-unselected-window-advice)

;;;;;;;;;;;;;
;; layouts ;;
;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;
;; Customizations ;;
;;;;;;;;;;;;;;;;;;;;
(add-hook 'exwm-input-input-mode-change-hook
          'exwm-input-mode-change-color)

;; display buffer rules
(push '(exwm-display-buffer-condition
        ;; exwm-display-buffer-biggest
        exwm-display-buffer-tiling-anticlockwise)
      display-buffer-alist)

(add-hook 'kill-buffer-hook 'exwm-close-window-if-exwm-mode)

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
    (exwm-set-window-transparency (current-buffer) exwm-default-transparency)))
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
  (cl-pushnew 'XF86PowerOff exwm-input-prefix-keys)

  (global-set-key (kbd "<XF86PowerOff>") 'exwm-shutdown)
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
          ;; New terminal
          ([s-return] . exwm-start-terminal)
          ([s-S-return] . exwm-start-emacs)
          ;; Bind "s-<f2>" to "slock", a simple X display locker.
          ([s-f2] . (lambda ()
                      (interactive)
                      (start-process "" nil "/usr/bin/slock")))
          ;; Toggle char-line modes
          ([?\s-q] . exwm-input-toggle-keyboard)
          ([?\s-Q] . (lambda ()
                       (interactive)
                       (message "Actual input mode: %s" exwm--input-mode)))
          ;; Display datetime
          ([?\s-a] . display-time-mode)
          ;; Workspaces
          ([?\s-n] . exwm-workspace-next)
          ([?\s-p] . exwm-workspace-prev)
          ([?\s-S] . exwm-workspace-swap)
          ([?\s-M] . exwm-randr-workspace-move-current)
          ;; windows
          ([?\s-m] . exwm-layout-toggle-mode-line)
          ([?\s-f] . exwm-layout-toggle-fullscreen)
          ([?\s-l] . exwm-floating-toggle-floating)
          ([?\s-s ?6] . exwm-display-buffer-cycle)
          ;; ace-window
          ([?\s-o] . exwm-ace-window)
          ;; Switch to minibuffer window
          ([?\s-s ?0] . switch-to-minibuffer-window)
          ;; switch buffer
          ([?\s-b] . switch-to-buffer)
          ;; Bind lock screen
          (,(kbd "<s-escape>") . exwm-screensaver-lock)
          (,(kbd "<C-s-escape>") . exwm-screensaver-interrupt)
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
        '(((member exwm-class-name
                   '("Emacs" "st-256color" "Alacritty" "URxvt" "XTerm"))
           char-mode t
           tiling-mode-line nil
           floating-mode-line nil)
          ((member exwm-class-name
                   '("darkplaces" "doom" "gzdoom"))
           floating nil
           tiling-mode-line nil
           floating-mode-line nil)
          (t tiling-mode-line nil
             floating-mode-line nil))))

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

(add-hook 'exwm-init-hook 'symon-mode)

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
(add-hook 'exwm-init-hook
          (lambda ()
            (dolist (program-and-args-list '(("compton")
                                             ("volumeicon")
                                             ("nm-applet")))
              (let ((executable (car program-and-args-list)))
                (if (executable-find executable)
                    (apply 'start-process
                           (concat " *" executable)
                           (concat " *" executable " outputs*")
                           program-and-args-list)
                  (message "Unable to find `%s' executable." executable))))))

(when (load "helm-exwm" t t)
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

(when (featurep 'ace-window)
  (defun aw-select-advice (orig-fun &rest args)
    (let ((exwm-buffer-list (exwm-buffer-list)))
      (mapc (lambda (buffer)
              (exwm-set-window-transparency buffer 0.2))
            exwm-buffer-list)
      (unwind-protect
          (apply orig-fun args)
        (mapc (lambda (buffer)
                (exwm-set-window-transparency buffer exwm-default-transparency))
              exwm-buffer-list))))
  (advice-add 'aw-select :around 'aw-select-advice))

;; gaps
;; (let ((color (face-attribute 'default :background)))
;;   (set-face-attribute 'window-divider nil :foreground color)
;;   (set-face-attribute 'window-divider-first-pixel nil :foreground "#353024")
;;   (set-face-attribute 'window-divider-last-pixel nil :foreground "#353024"))
;; (window-divider-mode)


;; minibuffer
(when (load "mini-frame" t t)
  (setq mini-frame-show-parameters
        (if (featurep 'helm)
            '((left . -1) (top . -1) (width . 0.75) (height . 1) (alpha . 75)
              (border-width . 0) (internal-border-width . 0)
              (background-color . "black"))
          (setq mini-frame-completions-show-parameters
                (defun mini-frame-completions-show-parameters-dwim ()
                  (let ((workarea (nth exwm-workspace-current-index
                                       exwm-workspace--workareas)))
                    `((parent-frame . nil)
                      (z-group . above)
                      (left . ,(+ (aref workarea 0) 20))
                      ;; (height . ,(cons 'text-pixels (round (* (aref workarea 3) 0.3))))
                      (height . ,(round (* (aref workarea 3) 0.023)))
                      ;; [ in this fuction 'text-pixels then white mini frame
                      (width . ,(round (* (aref workarea 2) 0.13635)))
                      ;; (width . ,(cons 'text-pixels (- (aref workarea 2) 60)))
                      ;; ]
                      (background-color . "black")))))
          (defun mini-frame-show-parameters-dwim ()
            (let* ((workarea (nth exwm-workspace-current-index
                                  exwm-workspace--workareas))
                   (workarea-width (aref workarea 2)))
              `((parent-frame . nil)
                (z-group . above)
                (top . ,(+ (aref workarea 1) 10))
                (left . ,(round (+ (aref workarea 0) (* workarea-width 0.05))))
                (height . 1)
                (width . ,(round (* workarea-width 0.1278)))
                ;; (width . ,(cons 'text-pixels (round (* workarea-width 0.9))))
                (background-color . "black")))))
        mini-frame-resize t  ;; nil when icomplete-exhibit advice
        ;; fix not resizing mini frame on gnome
        ;; x-gtk-resize-child-frames 'resize-mode
        resize-mini-frames t
        mini-frame-ignore-commands '(debugger-eval-expression
                                     objed-ipipe
                                     "edebug-eval-expression"
                                     "exwm-workspace-"))

  (defun mini-frame--resize-mini-frame (mini-frame-frame)
    (when (eq mini-frame-frame (selected-frame))
      (modify-frame-parameters
       mini-frame-frame
       `((height
          .
          ,(min
            40
            (count-visual-lines-in-string
             (concat
              (minibuffer-prompt)
              ;; (minibuffer-contents-no-properties)
              (when (and icomplete-mode
                         (icomplete-simple-completing-p))
                (overlay-get icomplete-overlay 'after-string)))
             (frame-width mini-frame-frame)))))))
    (when (and (frame-live-p mini-frame-completions-frame)
               (frame-visible-p mini-frame-completions-frame))
      (modify-frame-parameters
       mini-frame-completions-frame
       `((top
          .
          ,(+ (* 2 (frame-parameter mini-frame-frame 'internal-border-width))
              (frame-parameter mini-frame-frame 'top)
              (cdr (window-text-pixel-size
                    (frame-selected-window mini-frame-frame)))))))))

  (add-hook 'exwm-init-hook 'mini-frame-mode)

  ;; [ fix not resizing mini frame
  ;; (defun mini-frame-icomplete-exhibit-advice ()
  ;;   (when (and (bound-and-true-p mini-frame-frame)
  ;;              (frame-live-p mini-frame-frame)
  ;;              (frame-visible-p mini-frame-frame))
  ;;     (modify-frame-parameters
  ;;      mini-frame-frame
  ;;      `((height . ,(count-visual-lines-in-string
  ;;                    (concat
  ;;                     (buffer-substring-no-properties (point-min) (point-max))
  ;;                     (overlay-get icomplete-overlay 'after-string))
  ;;                    (frame-width mini-frame-frame)))))
  ;;     (when (and (frame-live-p mini-frame-completions-frame)
  ;;                (frame-visible-p mini-frame-completions-frame))
  ;;       (modify-frame-parameters
  ;;        mini-frame-completions-frame
  ;;        `((top
  ;;           .
  ;;           ,(+ (* 2 (frame-parameter mini-frame-frame 'internal-border-width))
  ;;               (frame-parameter mini-frame-frame 'top)
  ;;               (cdr (window-text-pixel-size
  ;;                     (frame-selected-window mini-frame-frame))))))))))
  ;; (advice-add 'icomplete-exhibit :after 'mini-frame-icomplete-exhibit-advice)
  ;; ]

  (defun mini-frame-toggle-resize ()
    (interactive)
    (if (setq mini-frame-resize (null mini-frame-resize))
        (advice-remove 'icomplete-exhibit 'mini-frame-icomplete-exhibit-advice)
      (advice-add 'icomplete-exhibit :after 'mini-frame-icomplete-exhibit-advice))
    (message "Custom mini frame resize: %s" (nu mini-frame-resize)))
  (global-set-key (kbd "M-s 7 0") 'mini-frame-toggle-resize)


  ;; only one minibuffer
  (defun common-minibuffer-all-frames ()
    (let ((frame (car (minibuffer-frame-list))))
      (setf (alist-get 'minibuffer default-frame-alist)
            (if frame nil t))))
  (add-hook 'before-make-frame-hook 'common-minibuffer-all-frames))


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
(global-set-key (kbd "M-s 7 T") 'exwm-toggle-transparency)


(provide 'exwm-startup-config)
;;; exwm-startup-config.el ends here
