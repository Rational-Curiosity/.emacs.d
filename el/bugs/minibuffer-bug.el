(with-eval-after-load 'minibuffer
  (when (bug-check-function-bytecode
         'completion--capf-wrapper
         ;; old 26.2
         ;; "icK3ghYAghoAAQidghcAAQmdP4IXAMOFeAABIIk6g1UAxAEhhFUAAgidhDEAAghCEMXGApvHIsg9g28AycoCQGAiywM4xcYFm8wiI4RvAMOyAYJvAIk8hG8AxAEhhG8AAgmdhG8Azc4EIogCCUIRiYV2AAIBQrIBhw=="
         ;; new 26.3
         "icI9hCMAicM9gxIAAQidgiAAicQ9gx8AAQmdP4IgAMWFgQABIIk6g14AxgEhhF4AAgidhDoAAghCEMfIApvJIso9g3gAy8wCQGAizQM4x8gFm84iI4R4AMWyAYJ4AIk8hHgAxgEhhHgAAgmdhHgAz9AEIogCCUIRiYV/AAIBQrIBhw==")
    (defvar minibuffer-bug-capf-message t)
    (defun toggle-minibuffer-bug-capf-message ()
      (interactive)
      (setq minibuffer-bug-capf-message (null minibuffer-bug-capf-message)))
    (defun completion--capf-wrapper (fun which)
      ;; FIXME: The safe/misbehave handling assumes that a given function will
      ;; always return the same kind of data, but this breaks down with functions
      ;; like comint-completion-at-point or mh-letter-completion-at-point, which
      ;; could be sometimes safe and sometimes misbehaving (and sometimes neither).
      (if (pcase which
            (`all t)
            (`safe (member fun completion--capf-safe-funs))
            (`optimist (not (member fun completion--capf-misbehave-funs))))
          (let ((res (condition-case raised-error
                         (funcall fun)
                       (error
                        ;; (message "completion-capf fun backtrace:")
                        ;; (backtrace)
                        (if minibuffer-bug-capf-message
                            (message "completion-capf: function `%s' signals: %s"
                                 (symbol-name fun) raised-error))
                        nil))))
            (cond
             ((and (consp res) (not (functionp res)))
              (unless (member fun completion--capf-safe-funs)
                (push fun completion--capf-safe-funs))
              (and (eq 'no (plist-get (nthcdr 3 res) :exclusive))
                   ;; FIXME: Here we'd need to decide whether there are
                   ;; valid completions against the current text.  But this depends
                   ;; on the actual completion UI (e.g. with the default completion
                   ;; it depends on completion-style) ;-(
                   ;; We approximate this result by checking whether prefix
                   ;; completion might work, which means that non-prefix completion
                   ;; will not work (or not right) for completion functions that
                   ;; are non-exclusive.
                   (null (try-completion (buffer-substring-no-properties
                                          (car res) (point))
                                         (nth 2 res)
                                         (plist-get (nthcdr 3 res) :predicate)))
                   (setq res nil)))
             ((not (or (listp res) (functionp res)))
              (unless (member fun completion--capf-misbehave-funs)
                (message
                 "Completion function %S uses a deprecated calling convention" fun)
                (push fun completion--capf-misbehave-funs))))
            (if res (cons fun res)))))))
