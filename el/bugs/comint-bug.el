(with-eval-after-load 'comint
  (when (bug-check-function-bytecode
         'comint-term-environment
         "wsAhgxYACIMWAMPECSLFw8bHICJFh8jDyccgIkSH")

    (require 'typing-config)
    (defun comint-term-environment ()
      (if (and (boundp 'system-uses-terminfo) system-uses-terminfo)
          (list (format "TERM=%s" comint-terminfo-terminal)
                "TERMCAP="
                (format "COLUMNS=%d" (window-width-without-margin)))
        (list "TERM=emacs"
              (format "TERMCAP=emacs:co#%d:tc=unknown:" (window-width-without-margin)))))))
