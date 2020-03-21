(with-eval-after-load 'exwm-input
  (when (bug-check-function-bytecode
         'exwm-input--update-mode-line
         "CIMUAMXGCYMPAAkgghAAx8gEJIjJiYkKyreCTgDLsgHMsgPNzs/Q0QYIIdIi08mJJgayAoJOANSyAdWyA83Oz9DRBggh1iLTyYkmBrICcgPXAQsiQbIBcYjY2QLaBgbb3N3e397gBg1CRUSvCEQU4SAphw==")
    (require 'exwm-core)
    (defun exwm-input--update-mode-line (id)
      "Update the propertized `mode-line-process' for window ID."
      (exwm--log "#x%x" id)
      (let (help-echo cmd mode)
        (with-current-buffer (exwm--id->buffer id)
          (cl-case exwm--input-mode
            (line-mode
             (setq mode "line"
                   help-echo "mouse-1: Switch to char-mode"
                   cmd (lambda ()
                         (interactive)
                         (exwm-input-release-keyboard id))))
            (char-mode
             (setq mode "char"
                   help-echo "mouse-1: Switch to line-mode"
                   cmd (lambda ()
                         (interactive)
                         (exwm-input-grab-keyboard id)))))
          (setq mode-line-process
                `(":"
                  (:propertize ,mode
                               help-echo ,help-echo
                               mouse-face mode-line-highlight
                               local-map
                               (keymap
                                (mode-line
                                 keymap
                                 (down-mouse-1 . ,cmd))))))
          (force-mode-line-update))))))