(with-eval-after-load 'mini-frame
  (when (bug-check-function-bytecode
         'mini-frame-read-from-minibuffer
         "xiCEPwDHMjwACImFOQCJQIk7gycAicgJIcnKGssDAwMjKbaDgioAiQk9gzIAzMfKIogBQbaCggoAsgEwg0QAzQICIofOCyGDVQDPCyGDVQDQAgIih8kMhVsA0Q2DZwDS0w4YIoJpAA4Y1A4ZQtQOGkLVDhtC1skeHB4dHhseGh4ZHhgeHh4fDhyIDh6IDh2I147QAgIiLgmH")
    (defun mini-frame-read-from-minibuffer (fn &rest args)
      "Show minibuffer-only child frame (if needed) and call FN with ARGS."
      (cond
       ((or (minibufferp)
            (and (symbolp this-command)
                 (catch 'ignored
                   (dolist (ignored-command mini-frame-ignore-commands)
                     (when
                         (if (stringp ignored-command)
                             (string-match-p ignored-command (symbol-name this-command))
                           (eq ignored-command this-command))
                       (throw 'ignored t))))))
        (apply fn args))
       ((and (frame-live-p mini-frame-frame)
             (frame-visible-p mini-frame-frame))
        (select-frame-set-input-focus mini-frame-frame)
        (apply fn args))
       (t
        (let ((after-make-frame-functions nil)
              (resize-mini-frames (when mini-frame-resize
                                    #'mini-frame--resize-mini-frame))
              (display-buffer-alist
               (if mini-frame-handle-completions
                   (append
                    '(("\\(\\*\\(Ido \\)?Completions\\)\\|\\(\\*Isearch completions\\)\\*" mini-frame--display-completions))
                    display-buffer-alist)
                 display-buffer-alist))
              (completion-setup-hook
               (cons #'mini-frame--completions-setup completion-setup-hook))
              (temp-buffer-window-show-hook
               (cons #'mini-frame--completions-setup temp-buffer-window-show-hook))
              (delete-frame-functions
               (cons #'mini-frame--delete-frame delete-frame-functions))
              ;; FIXME which-key is not working in mini frame
              (which-key-popup-type 'frame)
              (ivy-fixed-height-minibuffer nil))
          (ignore ivy-fixed-height-minibuffer)
          (ignore resize-mini-frames)
          (ignore which-key-popup-type)
          (unwind-protect
              (mini-frame--display fn args)
            (when (frame-live-p mini-frame-completions-frame)
              (make-frame-invisible mini-frame-completions-frame))
            (when (frame-live-p mini-frame-selected-frame)
              (select-frame-set-input-focus mini-frame-selected-frame))
            (when (frame-live-p mini-frame-frame)
              (if (eq system-type 'windows-nt)
                  ;; FIXME sometime buffer is not visible on windows
                  (delete-frame mini-frame-frame)
                (make-frame-invisible mini-frame-frame))))))))))
