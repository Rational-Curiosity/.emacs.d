(with-eval-after-load 'flymake
  (when (bug-check-function-bytecode
         'flymake--mode-line-format
         "CMVDxsfIycrLBgYhzCLNziUDIoiJop+2gs8g0CDRINIgAoUnAAM/0wUEIsbHyNTKywYIIdUi1tclCCKI2Nna29zd3gYNRyLd3wYNRyLd4AYNRyLhUuLjIOQB5QkjiOQB5ucjiImyAa8I6AYIhHMA6YKMAAKDgwDq693sBgZHIkWCjAADg4sA7YKMAO6JQAFBiUABQYlAAUEBBAYHiYW4AO/YAvAF3AYI4uMg5AHx8iOIibIBrwhEQ7aDtocEhsMABgk/P4UNAgXFQ8bHyMnKywYGIcwizc4lAyKIiaKftoLzxQE6g/oAAUCyAfQBBPX2JLIDAUGyAoLiAMW2gvcC+PX2JMWJiYn5BTqD0wEFQLIF+gUGDyKyBPsF/P0jsgMDhDsBCvk9hMkBCoM3AfYFIf4KIVmCOAH5g8kBAdjd/wYHRyLwBgba2+LjIAYN5ALLgUAACyLHgUEAgUIAyssGCCGBQwAigUQAgUUAgUYAJgYjiOQCy4FAAAwix4FBAIFCAMrLBgghgUcAIoFEAIFFAIFGACYGI4gBtoLc3YFIAIFJAN3/BhJHIvAGESOBSQDdgUoABhQi8AYSIyPdgUsACwwjUK8KQ6SyAgVBsgbFsgGCCAGBTADoA8WJiQM6gwICA7IDAolBsgSisgIBAUKyAQKD+wGBTQABQrIBA0GyBILbAYmftoSBTgAiQraIIkKH")
    (defun flymake--mode-line-format ()
      "Produce a pretty minor mode indicator."
      (let* ((known (hash-table-keys flymake--backend-state))
             (running (flymake-running-backends))
             (disabled (flymake-disabled-backends))
             (reported (flymake-reporting-backends))
             (diags-by-type (make-hash-table))
             (all-disabled (and disabled (null running)))
             (some-waiting (cl-set-difference running reported)))
        (maphash (lambda (_b state)
                   (mapc (lambda (diag)
                           (push diag
                                 (gethash (flymake--diag-type diag)
                                          diags-by-type)))
                         (flymake--backend-state-diags state)))
                 flymake--backend-state)
        `((:propertize "!"
                       mouse-face mode-line-highlight
                       help-echo
                       ,(concat (format "%s known backends\n" (length known))
                                (format "%s running\n" (length running))
                                (format "%s disabled\n" (length disabled))
                                "mouse-1: Display minor mode menu\n"
                                "mouse-2: Show help for minor mode")
                       keymap
                       ,(let ((map (make-sparse-keymap)))
                          (define-key map [mode-line down-mouse-1]
                            flymake-menu)
                          (define-key map [mode-line mouse-2]
                            (lambda ()
                              (interactive)
                              (describe-function 'flymake-mode)))
                          map))
          ,@(pcase-let ((`(,ind ,face ,explain)
                         (cond ((null known)
                                '("?" mode-line "No known backends"))
                               (some-waiting
                                `("Wait" compilation-mode-line-run
                                  ,(format "Waiting for %s running backend(s)"
                                           (length some-waiting))))
                               (all-disabled
                                '("!" compilation-mode-line-run
                                  "All backends disabled"))
                               (t
                                '(nil nil nil)))))
              (when ind
                `((":"
                   (:propertize ,ind
                                face ,face
                                help-echo ,explain
                                keymap
                                ,(let ((map (make-sparse-keymap)))
                                   (define-key map [mode-line mouse-1]
                                     'flymake-switch-to-log-buffer)
                                   map))))))
          ,@(unless (or all-disabled
                        (null known))
              (cl-loop
               with types = (hash-table-keys diags-by-type)
               with _augmented = (cl-loop for extra in '(:error :warning)
                                          do (cl-pushnew extra types
                                                         :key #'flymake--severity))
               for type in (cl-sort types #'> :key #'flymake--severity)
               for diags = (gethash type diags-by-type)
               for face = (flymake--lookup-type-property type
                                                         'mode-line-face
                                                         'compilation-error)
               when (or diags
                        (cond ((eq flymake-suppress-zero-counters t)
                               nil)
                              (flymake-suppress-zero-counters
                               (>= (flymake--severity type)
                                   (warning-numeric-level
                                    flymake-suppress-zero-counters)))
                              (t t)))
               collect `(:propertize
                         ,(format "%d" (length diags))
                         face ,face
                         mouse-face mode-line-highlight
                         keymap
                         ,(let ((map (make-sparse-keymap))
                                (type type))
                            (define-key map (vector 'mode-line
                                                    mouse-wheel-down-event)
                              `(lambda (event)
                                 (interactive "e")
                                 (with-selected-window (posn-window (event-start event))
                                   (flymake-goto-prev-error 1 (list ,type) t))))
                            (define-key map (vector 'mode-line
                                                    mouse-wheel-up-event)
                              `(lambda (event)
                                 (interactive "e")
                                 (with-selected-window (posn-window (event-start event))
                                   (flymake-goto-next-error 1 (list ,type) t))))
                            map)
                         help-echo
                         ,(concat (format "%s diagnostics of type %s\n"
                                          (propertize (format "%d"
                                                              (length diags))
                                                      'face face)
                                          (propertize (format "%s" type)
                                                      'face face))
                                  (format "%s/%s: previous/next of this type"
                                          mouse-wheel-down-event
                                          mouse-wheel-up-event)))
               into forms
               finally return
               `((:propertize "{")
                 ,@(cl-loop for (a . rest) on forms by #'cdr
                            collect a when rest collect
                            '(:propertize " "))
                 (:propertize "}")))))))))
