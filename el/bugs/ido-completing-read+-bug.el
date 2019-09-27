(with-eval-after-load 'ido-completing-read+
  (when (bug-check-function-bytecode
         'ido-completing-read@ido-cr+-replace
         "CMIgWYQLAAmEEADDAgIih8PEAiKH")
    (defun ido-completing-read@ido-cr+-replace (orig-fun prompt choices &optional
                                                         predicate require-match
                                                         initial-input hist def
                                                         inherit-input-method)
      "This advice allows ido-cr+ to completely replace `ido-completing-read'.

See the varaible `ido-cr+-replace-completely' for more information."
      (if (or (ido-cr+-active)
              (not ido-cr+-replace-completely))
          ;; ido-cr+ has either already activated or isn't going to
          ;; activate, so just run the function as normal
          (if def
              (let ((result (funcall orig-fun prompt choices predicate require-match
                                     initial-input hist def inherit-input-method)))
                (if (or (null result)
                        (and (seqp result)
                             (= 0 (length result))))
                    def
                  result))
           (funcall orig-fun prompt choices predicate require-match
                    initial-input hist def inherit-input-method))
        ;; Otherwise, we need to activate ido-cr+.
        (funcall #'ido-completing-read+ prompt choices predicate require-match
                 initial-input hist def inherit-input-method))))

  (when (bug-check-function-bytecode
         'ido-completing-read+
         "BgcGBwYHBgcGBwYHBgcGB68IGMYJIQQ6gyAABECCOAAEO4MpAASCOAAEhDEAx4I4AMjJygYHRCLLGgs/hUgAzAYJIYVIAAYIHMsdDINdAM3OIYNdAM7P0CEhgl4A0B5ADINyAM3OIYNyAM7P0SEhgnMA0R5BDkI/y9IxBQUEg4sADkODiwDI09QiiMwGCyGDqgHVBgshg7IABgo5g6wAyNPW1wYOIkMiiIKyAMjT2EMiiAGE3gDZBgshg94ABgo5g8sA1toGDCKCzADbyw5EhdkA3N3eBFADI7YD37ICBgiDqgEFhKoBDkXL38sDOoN+AQNAsgMCg3MBAjmDNAECBg89hGkB4DEQAc8DITCCEgGIy+ExHgHPBhAhMIIgAYjLiYUvAQGFLwHPAiHPAiE9toKCZgECO4NTAQYOOYNzAQLiBhAhy98eRuMDAwMjKbaDgmYB5OUERA5EhWMB3N3eBFADI7YDy4NzAQKyAcuJsgOCdAHfg34BA0GyBILsAAE/hYQBibaEg6oBBgo5g5cB1uYGDCKCmAHnyw5EhaUB3N3eBFADI7YD37IBDIO5AQ5AAwYMBgwjgsAB0ccGDAYMI7ILBgpH6FWD0wEMhNMByNPpIogOR4PrAQYKRw5HVoPrAcjT1uoORyJDIojrIIMiBA5IDklEywE6g/MCAUCyAdUBIYMMAsjT7ANEIogDhCoC2QEhgyoC7QFDDkSFJQLc3d4EUAMjtgPfsgQGCoPsAgYHhOwCAoTsAg5Fy9/LAzqDzQIDQLIDAoPCAgI5g4QCAgU9hLgC7jFgAs8DITCCYgKIy+8xbgLPBgYhMIJwAojLiYV/AgGFfwLPAiHPAiE9toKCtQICO4OiAgQ5g8ICAuIGBiHL3x5G4wMDAyMptoOCtQLk5QREDkSFsgLc3d4EUAMjtgPLg8ICArIBy4myA4LDAt+DzQIDQbIEgj0CAT+F0wKJtoSD7ALwAUMORIXnAtzd3gRQAyO2A9+yAwFBsgKC9gG2AvHL8gLzIkFAsgGJgyAEzwEh9D6EIATVASGDIwPI0wI5gx8D1vUEIoIgA/ZDIogDhEwD2QEhg0wDiTmDOQPW9wIigjoD+MsORIVHA9zd3gRQAyO2A9+yBAYKgxkEBgeEGQQChBkEDkXL38sDOoPvAwNAsgMCg+QDAjmDpgMCBT2E2gP5MYIDzwMhMIKEA4jL+jGQA88GBiEwgpIDiMuJhaEDAYWhA88CIc8CIT22goLXAwI7g8QDBDmD5AMC4gYGIcvfHkbjAwMDIym2g4LXA+TlBEQORIXUA9zd3gRQAyO2A8uD5AMCsgHLibIDguUD34PvAwNBsgSCXwMBP4X1A4m2hIMZBIk5gwYE1vsCIoIHBPzLDkSFFATc3d4EUAMjtgPfsgMBVLICgvcCtgIBhCwEyNP9QyKIBgiDYAQFhGAEiYNMBP7LDkSFRwTc3d4EUAMjtgOCYAT/yw5EhVoE3N3eBFADI7YDx0OyBgU8hGkEBUOyBgWDjgSBUQCBUgDWgVMAIgYHIrIGgVQAgVUABgcGDSIhsgvLsgYOSoOtBIFWAA5LgVcAIoOtBMcGC52DrQTI04FYACKIBgc6g8gEgVYADkuBWQAig8gEBgeJAUFUobYCgVoAIFTL38sbHkweTR5OgVsAjoFcAAYMBgwGDAYMBgwGDAYMBgwmCC0OT4FdAD2DAQXI04FeACKIMII/BQTLGxmJDkSFNQWJPIMgBYlA0z2DIAWJQUCyAYFfAA5QAkQORIUzBdzd3gRQAyO2grYC3A5QCCIqsgEuBoc=")
    (defun ido-completing-read+ (prompt collection &optional predicate
                                        require-match initial-input
                                        hist def inherit-input-method)
      "ido-based method for reading from the minibuffer with completion.

See `completing-read' for the meaning of the arguments.

This function is a wrapper for `ido-completing-read' designed to
be used as the value of `completing-read-function'. Importantly,
it detects edge cases that ido cannot handle and uses normal
completion for them."
      (let* (;; Save the original arguments in case we need to do the
             ;; fallback
             (ido-cr+-orig-completing-read-args
              (list prompt collection predicate require-match
                    initial-input hist def inherit-input-method))
             ;; Need to save a copy of this since activating the
             ;; minibuffer once will clear out any temporary minibuffer
             ;; hooks, which need to get restored before falling back so
             ;; that they will trigger again when the fallback function
             ;; uses the minibuffer. We make a copy in case the original
             ;; list gets modified in place.
             (orig-minibuffer-setup-hook (cl-copy-list minibuffer-setup-hook))
             ;; Need just the string part of INITIAL-INPUT
             (initial-input-string
              (cond
               ((consp initial-input)
                (car initial-input))
               ((stringp initial-input)
                initial-input)
               ((null initial-input)
                "")
               (t
                (signal 'wrong-type-argument (list 'stringp initial-input)))))
             (ido-cr+-active-restrictions nil)
             ;; If collection is a function, save it for later, unless
             ;; instructed not to
             (ido-cr+-dynamic-collection
              (when (and (not ido-cr+-assume-static-collection)
                         (functionp collection))
                collection))
             (ido-cr+-last-dynamic-update-text nil)
             ;; Only memoize if the collection is dynamic.
             (ido-cr+-all-prefix-completions-memoized
              (if (and ido-cr+-dynamic-collection (featurep 'memoize))
                  (memoize (indirect-function 'ido-cr+-all-prefix-completions))
                'ido-cr+-all-prefix-completions))
             (ido-cr+-all-completions-memoized
              (if (and ido-cr+-dynamic-collection (featurep 'memoize))
                  (memoize (indirect-function 'all-completions))
                'all-completions))
             ;; If the whitelist is empty, everything is whitelisted
             (whitelisted (not ido-cr+-function-whitelist))
             ;; If non-nil, we need alternate nil DEF handling
             (alt-nil-def nil))
        (condition-case sig
            (progn
              ;; Check a bunch of fallback conditions
              (when (and inherit-input-method current-input-method)
                (signal 'ido-cr+-fallback
                        '("ido cannot handle alternate input methods")))

              ;; Check for black/white-listed collection function
              (when (functionp collection)
                ;; Blacklist
                (when (ido-cr+-function-is-blacklisted collection)
                  (if (symbolp collection)
                      (signal 'ido-cr+-fallback
                              (list (format "collection function `%S' is blacklisted" collection)))
                    (signal 'ido-cr+-fallback
                            (list "collection function is blacklisted"))))
                ;; Whitelist
                (when (and (not whitelisted)
                           (ido-cr+-function-is-whitelisted collection))
                  (ido-cr+--debug-message
                   (if (symbolp collection)
                       (format "Collection function `%S' is whitelisted" collection)
                     "Collection function is whitelisted"))
                  (setq whitelisted t))
                ;; nil DEF list
                (when (and
                       require-match (null def)
                       (ido-cr+-function-is-in-list
                        collection
                        ido-cr+-nil-def-alternate-behavior-list))
                  (ido-cr+--debug-message
                   (if (symbolp collection)
                       (format "Using alternate nil DEF handling for collection function `%S'" collection)
                     "Using alternate nil DEF handling for collection function"))
                  (setq alt-nil-def t)))

              ;; Expand all currently-known completions.
              (setq collection
                    (if ido-cr+-dynamic-collection
                        (funcall ido-cr+-all-prefix-completions-memoized
                                 initial-input-string collection predicate)
                      (all-completions "" collection predicate)))
              ;; No point in using ido unless there's a collection
              (when (and (= (length collection) 0)
                         (not ido-cr+-dynamic-collection))
                (signal 'ido-cr+-fallback '("ido is not needed for an empty collection")))
              ;; Check for excessively large collection
              (when (and ido-cr+-max-items
                         (> (length collection) ido-cr+-max-items))
                (signal 'ido-cr+-fallback
                        (list
                         (format
                          "there are more than %i items in COLLECTION (see `ido-cr+-max-items')"
                          ido-cr+-max-items))))

              ;; If called from `completing-read', check for
              ;; black/white-listed commands/callers
              (when (ido-cr+--called-from-completing-read)
                ;; Check calling command and `ido-cr+-current-command'
                (cl-loop
                 for cmd in (list this-command ido-cr+-current-command)

                 if (ido-cr+-function-is-blacklisted cmd)
                 do (signal 'ido-cr+-fallback
                            (list "calling command `%S' is blacklisted" cmd))

                 if (and (not whitelisted)
                         (ido-cr+-function-is-whitelisted cmd))
                 do (progn
                      (ido-cr+--debug-message "Command `%S' is whitelisted" cmd)
                      (setq whitelisted t))

                 if (and
                     require-match (null def) (not alt-nil-def)
                     (ido-cr+-function-is-in-list
                      cmd ido-cr+-nil-def-alternate-behavior-list))
                 do (progn
                      (ido-cr+--debug-message
                       "Using alternate nil DEF handling for command `%S'" cmd)
                      (setq alt-nil-def t)))

                ;; Check every function in the call stack starting after
                ;; `completing-read' until to the first
                ;; `funcall-interactively' (for a call from the function
                ;; body) or `call-interactively' (for a call from the
                ;; interactive form, in which the function hasn't actually
                ;; been called yet, so `funcall-interactively' won't be on
                ;; the stack.)
                (cl-loop for i upfrom 1
                         for caller = (cadr (backtrace-frame i 'completing-read))
                         while caller
                         while (not (memq (indirect-function caller)
                                          '(internal--funcall-interactively
                                            (indirect-function 'call-interactively))))

                         if (ido-cr+-function-is-blacklisted caller)
                         do (signal 'ido-cr+-fallback
                                    (list (if (symbolp caller)
                                              (format "calling function `%S' is blacklisted" caller)
                                            "a calling function is blacklisted")))

                         if (and (not whitelisted)
                                 (ido-cr+-function-is-whitelisted caller))
                         do (progn
                              (ido-cr+--debug-message
                               (if (symbolp caller)
                                   (format "Calling function `%S' is whitelisted" caller)
                                 "A calling function is whitelisted"))
                              (setq whitelisted t))

                         if (and require-match (null def) (not alt-nil-def)
                                 (ido-cr+-function-is-in-list
                                  caller ido-cr+-nil-def-alternate-behavior-list))
                         do (progn
                              (ido-cr+--debug-message
                               (if (symbolp caller)
                                   (format "Using alternate nil DEF handling for calling function `%S'" caller)
                                 "Using alternate nil DEF handling for a calling function"))
                              (setq alt-nil-def t))))

              (unless whitelisted
                (signal 'ido-cr+-fallback
                        (list "no functions or commands matched the whitelist for this call")))

              (when (and require-match (null def))
                ;; Replace nil with "" for DEF if match is required, unless
                ;; alternate nil DEF handling is enabled
                (if alt-nil-def
                    (ido-cr+--debug-message
                     "Leaving the default at nil because alternate nil DEF handling is enabled.")
                  (ido-cr+--debug-message
                   "Adding \"\" as the default completion since no default was provided.")
                  (setq def (list ""))))

              ;; In ido, the semantics of "default" are simply "put it at
              ;; the front of the list". Furthermore, ido can't handle a
              ;; list of defaults, nor can it handle both DEF and
              ;; INITIAL-INPUT being non-nil. So, just pre-process the
              ;; collection to put the default(s) at the front and then
              ;; set DEF to nil in the call to ido to avoid these issues.
              (unless (listp def)
                ;; Ensure DEF is a list
                (setq def (list def)))
              (when def
                ;; Ensure DEF are strings
                (setq def (mapcar (apply-partially #'format "%s") def))
                ;; Prepend DEF to COLLECTION and remove duplicates
                (setq collection (delete-dups (append def collection))
                      ;; def nil))     ;; -
                      def (car def)))  ;; +

              ;; Check for a specific bug
              (when (and ido-enable-dot-prefix
                         (version< emacs-version "26.1")
                         (member "" collection))
                (signal 'ido-cr+-fallback
                        '("ido cannot handle the empty string as an option when `ido-enable-dot-prefix' is non-nil; see https://debbugs.gnu.org/cgi/bugreport.cgi?bug=26997")))

              ;; Fix ido's broken handling of cons-style INITIAL-INPUT on
              ;; Emacsen older than 27.1.
              (when (and (consp initial-input)
                         (version< emacs-version "27.1"))
                ;; `completing-read' uses 0-based index while
                ;; `read-from-minibuffer' uses 1-based index.
                (cl-incf (cdr initial-input)))

              ;; Finally ready to do actual ido completion
              (prog1
                  (let ((ido-cr+-minibuffer-depth (1+ (minibuffer-depth)))
                        (ido-cr+-dynamic-update-timer nil)
                        (ido-cr+-exhibit-pending t)
                        ;; Reset this for recursive calls to ido-cr+
                        (ido-cr+-assume-static-collection nil))
                    (unwind-protect
                        (ido-completing-read
                         prompt collection
                         predicate require-match initial-input hist def
                         inherit-input-method)
                      (when ido-cr+-dynamic-update-timer
                        (cancel-timer ido-cr+-dynamic-update-timer)
                        (setq ido-cr+-dynamic-update-timer nil))))
                ;; This detects when the user triggered fallback mode
                ;; manually.
                (when (eq ido-exit 'fallback)
                  (signal 'ido-cr+-fallback '("user manually triggered fallback")))))

          ;; Handler for ido-cr+-fallback signal
          (ido-cr+-fallback
           (let (;; Reset `minibuffer-setup-hook' to original value
                 (minibuffer-setup-hook orig-minibuffer-setup-hook)
                 ;; Reset this for recursive calls to ido-cr+
                 (ido-cr+-assume-static-collection nil))
             (ido-cr+--explain-fallback sig)
             (apply ido-cr+-fallback-function ido-cr+-orig-completing-read-args))))))))


  (provide 'ido-completing-read+-bug)