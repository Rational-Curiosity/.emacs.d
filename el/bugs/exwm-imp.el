(with-eval-after-load 'exwm-layout
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
        (set-window-dedicated-p (get-buffer-window) nil))))

  (when (bug-check-function-bytecode
         'exwm-layout-set-fullscreen
         "xjKdAAiDHADHyAmDEwAJIIIUAMnKBIYaAMskiImEJgDMzSGDKwDOIIMwAM/G0CKIcomDQACJ0QEKIkGyAYJCANIgcYjTCyHUDNUD1iLVBNci1QXYItUGBtkiJbYC2g3b3N0M3t8OKQ4qIuDL4Q4rJgkiiA4s4gEOLSKDgwAOLYiCiQCJDi1CFi2I4wEhiOQNIYjl5iDnIojoDCEpMIc=")
    (cl-defun exwm-layout-set-fullscreen (&optional id)
      "Make window ID fullscreen."
      (interactive)
      (exwm--log "id=#x%x" (or id 0))
      (unless (and (or id (derived-mode-p 'exwm-mode))
                   (not (exwm-layout--fullscreen-p)))
        (cl-return-from exwm-layout-set-fullscreen))
      (with-current-buffer (if id (exwm--id->buffer id) (window-buffer))
        ;; Expand the X window to fill the whole screen.
        (with-slots (x y width height) (exwm-workspace--get-geometry exwm--frame)
          (exwm--set-geometry exwm--id x y width height))
        ;; Raise the X window.
        (xcb:+request exwm--connection
            (make-instance 'xcb:ConfigureWindow
                           :window exwm--id
                           :value-mask (logior xcb:ConfigWindow:BorderWidth
                                               xcb:ConfigWindow:StackMode)
                           :border-width 0
                           :stack-mode xcb:StackMode:Above))
        (cl-pushnew xcb:Atom:_NET_WM_STATE_FULLSCREEN exwm--ewmh-state)
        (xcb:+request exwm--connection
            (make-instance 'xcb:ewmh:set-_NET_WM_STATE
                           :window exwm--id
                           :data exwm--ewmh-state))
        (exwm-layout--set-ewmh-state id)
        (xcb:flush exwm--connection)
        (set-window-dedicated-p (get-buffer-window) t)))))

(with-eval-after-load 'exwm-randr
  (when (bug-check-function-bytecode
         'exwm-randr-refresh
         "CIMTAMbHCYMPAAkgghAAyMkjiAqDHADKIIIeAMsgicycAc2cAs6cz4kEhRsBA4UbAQuDNwDMEwxHzIkCV4OXAInQDQIi0QEGCSJBDAOc0gHTIgKDYQDRBAYLIkGyBIJuAAYLsgTRBgwGDCJBsgOJ1AUhQkMGCaSyCYkCQkMGCKSyCNUC1gYGI4jVAtcFI7YGiVSyAYI6ALYC2CCIDImDrgCJQNkBIYgBQbaCgp0AiNoOLCGI2yCDwQDcIIPBAN0giN4giAyJg9cAiUDfAc8iiAFBtoKCxQCI4OHiDizjDizk5eYOLSMizyNA5yIhiYMSAYlAiQSeQYmDCgHoAQYGIrIF3wIFnkHpIoiIAUG2goLtAIjaDiwhiOrrIYc=")
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
          (run-hooks 'exwm-randr-refresh-hook))))))