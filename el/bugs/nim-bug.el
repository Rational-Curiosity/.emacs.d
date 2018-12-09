(when (and
       (bug-check-function-bytecode
        'nimsuggest--call-epc
        "xCDFCMYigw0Ax4KOAMgIySKDJQDKywIiiMzNCQpfzqUDI4KOAMgIzyKDagDQAwPRBCEjiNIyjgALx4kCOoNeAAJAsgGJAUGyAqKyAgMCmoNXANPS1CKIAkGyA4I7AMrVBSKIx7aDMIKOAMgI1iKDiwDXwCGI2BDH2doDISGyAdsB3CKyAYmyAYKOAN3eIYc=")
       (bug-check-function-bytecode
        'nimsuggest--start-epc-deferred
        "wcIIAyLDxMXGxwYHIcgiycolIoc="))
  (defun nimsuggest--call-epc (method callback)
    "Call the nimsuggest process on point.

Call the nimsuggest process responsible for the current buffer.
All commands work with the current cursor position.  METHOD can be
one of:

sug: suggest a symbol
con: suggest, but called at fun(_ <-
def: where the symbol is defined
use: where the symbol is used
dus: def + use

The CALLBACK is called with a list of ‘nim--epc’ structs."
    (let ((file (buffer-file-name)))
      (cl-case nimsuggest--state
        ((never connecting) nil) ; do nothing
        (no-response
         (nimsuggest--set-state 'never file)
         ;; Maybe M-x `epc:pop-to-last-server-process-buffer' would be
         ;; helpful to check the cause.
         (message "nimsuggest-mode reached timeout (about %dsec) due to no response from server.
This feature will be blocked on this %s."
                  (/ (* nimsuggest-accept-process-delay
                        nimsuggest-accept-process-timeout-count)
                     1000)
                  file))
        (ready
         (nimsuggest--query method callback (nimsuggest--get-epc-process file))
         ;; Reset `nimsuggest--state' if all epc processes for the file are dead.
         ;; Not sure if this is related to --refresh option.
         (catch 'exit
           (cl-loop for (f . _) in nimsuggest--epc-processes-alist
                    if (equal file f)
                    do (throw 'exit t)
                    finally (nimsuggest--set-state 'not-started file))))
        (not-started
         (setq-local nimsuggest--state 'connecting)
         (deferred:$
           (deferred:next
             (nimsuggest--start-epc-deferred file method callback))
           (deferred:error it
             (lambda (err) (nim-log "EPC(startup) ERROR %s" (error-message-string err))))))
        (t
         (error (format "This shouldn't happen: nimsuggest--state is %s" nimsuggest--state))))))

  (defun nimsuggest--start-epc-deferred (file method callback)
    "Start EPC process for FILE."
    (deferred:nextc (nimsuggest--start-server-deferred nimsuggest-path file)
      (lambda (mngr)
        (push (cons file mngr) nimsuggest--epc-processes-alist)
        (nimsuggest--set-state 'ready file)
        (when (eq method 'chk)
          (nimsuggest--query method callback (nimsuggest--get-epc-process file))
          ;; Reset `nimsuggest--state' if all epc processes for the file are dead.
          ;; Not sure if this is related to --refresh option.
          (catch 'exit
            (cl-loop for (f . _) in nimsuggest--epc-processes-alist
                     if (equal file f)
                     do (throw 'exit t)
                     finally (nimsuggest--set-state 'not-started file))))))))


(provide 'nim-bug)