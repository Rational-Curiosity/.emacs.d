(with-eval-after-load 'em-cmpl
  (when (bug-check-function-bytecode
         'eshell-complete-parse-arguments
         "CIMRAAmDEQDDxCGIxcbHIojIIIrJIIhgKcdDyokKyz6DNwDMzQUGByOIAwVVgzMAyn+IyCCyBc4yRADKzwUGByKyAzCJsgKDfACJQNA+g2EAiUFAVLIEzwQGBiKyAoJ8AIlA0T2DcwDSIIjFxsciiIJ8AMPEIYjFxsciiNMFU9Qig40Aw8QhiMXGxyKIA4kGBleDqADTAdUig6EAAwFDpIiJVLIBgo4AiAJBsgMBRwNHVYS5ANbXIYgB2MoCg9wAAkA6g9EAAkBA2T2D0QABsgECQbIDAVSyAoK8AImD7QCJVAWbsgWJVAYGm7IGtgMBRwNHVYT7ANbaIYgBgyMBBIYEAWBTZnrbPYMjAQRThhIBYFNm3D2EIwEB3UOkiAJgQ6SI3t8DIgNChw==")
    (defun eshell-complete-parse-arguments ()
      "Parse the command line arguments for `pcomplete-argument'."
      (when (and eshell-no-completion-during-jobs
                 (eshell-interactive-process))
        ;; (insert-and-inherit "\t") ;; -
        (throw 'pcompleted t))
      (let ((end (point-marker))
            (begin (save-excursion (eshell-bol) (point)))
            (posns (list t))
            args delim)
        (when (memq this-command '(pcomplete-expand
                                   pcomplete-expand-and-complete))
          (run-hook-with-args 'eshell-expand-input-functions begin end)
          (if (= begin end)
              (end-of-line))
          (setq end (point-marker)))
        (if (setq delim
                  (catch 'eshell-incomplete
                    (ignore
                     (setq args (eshell-parse-arguments begin end)))))
            (cond ((memq (car delim) '(?\{ ?\<))
                   (setq begin (1+ (cadr delim))
                         args (eshell-parse-arguments begin end)))
                  ((eq (car delim) ?\()
                   (eshell-complete-lisp-symbol)
                   (throw 'pcompleted t))
                  (t
                   ;; (insert-and-inherit "\t") ;; -
                   (throw 'pcompleted t))))
        (when (get-text-property (1- end) 'comment)
          ;; (insert-and-inherit "\t") ;; -
          (throw 'pcompleted t))
        (let ((pos begin))
          (while (< pos end)
            (if (get-text-property pos 'arg-begin)
                (nconc posns (list pos)))
            (setq pos (1+ pos))))
        (setq posns (cdr posns))
        (cl-assert (= (length args) (length posns)))
        (let ((a args)
              (i 0)
              l)
          (while a
            (if (and (consp (car a))
                     (eq (caar a) 'eshell-operator))
                (setq l i))
            (setq a (cdr a) i (1+ i)))
          (and l
               (setq args (nthcdr (1+ l) args)
                     posns (nthcdr (1+ l) posns))))
        (cl-assert (= (length args) (length posns)))
        (when (and args (eq (char-syntax (char-before end)) ? )
                   (not (eq (char-before (1- end)) ?\\)))
          (nconc args (list ""))
          (nconc posns (list (point))))
        (cons (mapcar
               (function
                (lambda (arg)
                  (let ((val
                         (if (listp arg)
                             (let ((result
                                    (eshell-do-eval
                                     (list 'eshell-commands arg) t)))
                               (cl-assert (eq (car result) 'quote))
                               (cadr result))
                           arg)))
                    (if (numberp val)
                        (setq val (number-to-string val)))
                    (or val ""))))
               args)
              posns)))))

(cl-eval-when 'compile
  (require 'esh-util)) ;; eshell-condition-case
(with-eval-after-load 'esh-mode
  (when (bug-check-function-bytecode
         'eshell-send-input
         "CIUGAAE/xokZGomFFADHCCHIPT8/hVUBiYQiAGALWYMoAGRiiIIzAMkEIQtiiMoBIbYCAYRDAAyDPwCJhEMAy8whiImDawDNCyGIDIRWAA0OJVWDYgABP4VVAc4IzyKCVQHQCA0OJSOCVQELYFWDdwDR0iGCVQHT1DEMAdUxywDWC2BTIrIB19gLYFMjiNkLYFMiiYXEAM0LIYjWDQ4lUyKyAtHaIYjbMr8A09wCIYO5AN0CIYiCvgDeAgQiiDCFxADfILIBMDCCUwEwxmRiiOAgFeAgFiXgIBYmYBYn4CATigtiiG6G8ADP4dMCIrIBKYiJP4X9ANHSIYhkYrYC0dIhiOLj0yKyAYJTAcZkYojgIBXgIBYl4CAWJmAWJ+AgE4oLYohuhjABz+HTAiKyASmIiT+FPQHR0iGIZGK2AuQBIc9Q4dMCIrYC0dIhiMoCIbIBsgEqhw==")
    (defun eshell-send-input (&optional use-region queue-p no-newline)
      "Send the input received to Eshell for parsing and processing.
After `eshell-last-output-end', sends all text from that marker to
point as input.  Before that marker, calls `eshell-get-old-input' to
retrieve old input, copies it to the end of the buffer, and sends it.

If USE-REGION is non-nil, the current region (between point and mark)
will be used as input.

If QUEUE-P is non-nil, input will be queued until the next prompt,
rather than sent to the currently active process.  If no process, the
input is processed immediately.

If NO-NEWLINE is non-nil, the input is sent without an implied final
newline."
      (interactive "P")
      ;; Note that the input string does not include its terminal newline.
      (let ((proc-running-p (and (eshell-interactive-process)
                                 (not queue-p)))
            (inhibit-point-motion-hooks t)
            (inhibit-modification-hooks t))
        (unless (and proc-running-p
                     (not (eq (process-status
                               (eshell-interactive-process))
                              'run)))
          (if (or proc-running-p
                  (>= (point) eshell-last-output-end))
              (goto-char (point-max))
            (let ((copy (eshell-get-old-input use-region)))
              (goto-char eshell-last-output-end)
              (insert-and-inherit copy)))
          (unless (or no-newline
                      (and eshell-send-direct-to-subprocesses
                           proc-running-p))
            (insert-before-markers-and-inherit ?\n))
          (if proc-running-p
              (progn
                (eshell-update-markers eshell-last-output-end)
                (if (or eshell-send-direct-to-subprocesses
                        (= eshell-last-input-start eshell-last-input-end))
                    (unless no-newline
                      (process-send-string (eshell-interactive-process) "\n"))
                  (process-send-region (eshell-interactive-process)
                                       eshell-last-input-start
                                       eshell-last-input-end)
                  (run-hooks 'eshell-input-filter-functions))) ;; +
            (if (= eshell-last-output-end (point))
                (run-hooks 'eshell-post-command-hook)
              (let (input)
                (eshell-condition-case err
                    (progn
                      (setq input (buffer-substring-no-properties
                                   eshell-last-output-end (1- (point))))
                      (run-hook-with-args 'eshell-expand-input-functions
                                          eshell-last-output-end (1- (point)))
                      (let ((cmd (eshell-parse-command-input
                                  eshell-last-output-end (1- (point)))))
                        (when cmd
                          (eshell-update-markers eshell-last-output-end)
                          (setq input (buffer-substring-no-properties
                                       eshell-last-input-start
                                       (1- eshell-last-input-end)))
                          (run-hooks 'eshell-input-filter-functions)
                          (and (catch 'eshell-terminal
                                 (ignore
                                  (if (eshell-invoke-directly cmd)
                                      (eval cmd)
                                    (eshell-eval-command cmd input))))
                               (eshell-life-is-too-much)))))
                  (quit
                   (eshell-reset t)
                   (run-hooks 'eshell-post-command-hook)
                   (signal 'quit nil))
                  (error
                   (eshell-reset t)
                   (eshell-interactive-print
                    (concat (error-message-string err) "\n"))
                   (run-hooks 'eshell-post-command-hook)
                   (insert-and-inherit input)))))))))))

(with-eval-after-load 'em-term
  (when (bug-check-function-bytecode
         'eshell-exec-visual
         "xRjGAUACQSKJQMfIA0EFQSLJygIisgEhy8zNBCHMUSFwcs4CIYjPIIjQwiGICRLQwyGIiRPRAgWJxQYHJYjSAiGJg1UA0wEh1D2DVQDVAdYiiIJZANfYIYiI2SCIDINlANrbIYgqtgXFhw==")
    ;; thanks to: https://gist.github.com/ralt/a36288cd748ce185b26237e6b85b27bb
    (defun eshell-exec-visual (&rest args)
      "Run the specified PROGRAM in a terminal emulation buffer.
 ARGS are passed to the program.  At the moment, no piping of input is
 allowed."
      (let* (eshell-interpreter-alist
             (original-args args)
             (interp (eshell-find-interpreter (car args) (cdr args)))
             (in-ssh-tramp (and (tramp-tramp-file-p default-directory)
                                (equal (tramp-file-name-method
                                        (tramp-dissect-file-name default-directory))
                                       "ssh")))
             (program (if in-ssh-tramp
                          "ssh"
                        (car interp)))
             (args (if in-ssh-tramp
                       (let ((dir-name (tramp-dissect-file-name default-directory)))
                         (eshell-flatten-list
                          (list
                           "-t"
                           (tramp-file-name-host dir-name)
                           (format
                            "export TERM=xterm-256color; cd %s; exec %s"
                            (tramp-file-name-localname dir-name)
                            (string-join
                             (append
                              (list (tramp-file-name-localname (tramp-dissect-file-name (car interp))))
                              (cdr args))
                             " ")))))
                     (eshell-flatten-list
                      (eshell-stringify-list (append (cdr interp)
                                                     (cdr args))))))
             (term-buf
              (generate-new-buffer
               (concat "*"
                       (if in-ssh-tramp
                           (format "%s %s" default-directory (string-join original-args " "))
                         (file-name-nondirectory program))
                       "*")))
             (eshell-buf (current-buffer)))
        (save-current-buffer
          (switch-to-buffer term-buf)
          (term-mode)
          (set (make-local-variable 'term-term-name) eshell-term-name)
          (make-local-variable 'eshell-parent-buffer)
          (setq eshell-parent-buffer eshell-buf)
          (term-exec term-buf program program nil args)
          (let ((proc (get-buffer-process term-buf)))
            (if (and proc (eq 'run (process-status proc)))
                (set-process-sentinel proc 'eshell-term-sentinel)
              (error "Failed to invoke visual command")))
          (term-char-mode)
          (if eshell-escape-control-x
              (term-set-escape-char ?\C-x))))
      nil)))
