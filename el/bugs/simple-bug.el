(with-eval-after-load 'simple
  (when (bug-check-function-bytecode
         'repeat-complex-command
         "iVMIOMUBgzQAxQPGIFQZGhvHjsjJygQhDMvABghCJSyyAczAAiKIzc4CQM/QBUEiI4JCAAiDPwDR0gQigkIA0dMhhw==")
    (defun repeat-complex-command (arg)
      "Edit and re-evaluate last complex command, or ARGth from last.
A complex command is one that used the minibuffer.
The command is placed in the minibuffer as a Lisp form for editing.
The result is executed, repeating the command as changed.
If the command has been changed or is not the most recent previous
command it is added to the front of the command history.
You can use the minibuffer history commands \
\\<minibuffer-local-map>\\[next-history-element] and \\[previous-history-element]
to get different commands to edit and resubmit."
      (interactive "p")
      (let ((elt (nth (1- arg) command-history))
            newcmd)
        (if elt
            (progn
              (setq newcmd
                    (let ((print-level nil)
                          (minibuffer-completing-symbol t)
                          (minibuffer-history-position arg)
                          (minibuffer-history-sexp-flag (1+ (minibuffer-depth))))
                      (unwind-protect
                          (minibuffer-with-setup-hook
                              (lambda ()
                                ;; FIXME: call emacs-lisp-mode (see also
                                ;; `eldoc--eval-expression-setup')?
                                (add-hook 'completion-at-point-functions
                                          #'elisp-completion-at-point nil t)
                                (run-hooks 'eval-expression-minibuffer-setup-hook))
                            (read-from-minibuffer
                             "Redo: " (prin1-to-string elt) read-expression-map t
                             (cons 'command-history arg)))

                        ;; If command was added to command-history as a
                        ;; string, get rid of that.  We want only
                        ;; evaluable expressions there.
                        (when (stringp (car command-history))
                          (pop command-history)))))

              (add-to-history 'command-history newcmd)
              (apply #'funcall-interactively
                     (car newcmd)
                     (mapcar (lambda (e) (eval e t)) (cdr newcmd))))
          (if command-history
              (error "Argument %d is beyond length of command history" arg)
            (error "There are no previous complex commands to repeat")))))))