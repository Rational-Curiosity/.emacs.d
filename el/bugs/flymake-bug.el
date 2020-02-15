(with-eval-after-load 'flymake
  (when (bug-check-function-bytecode
         'flymake--mode-line-format
         "CMRDxcbHyMnKBgYhyyLMzSUDIoiJop+2gs4gzyDQINEgAoUnAAM/0gUEIsXGx9PJygYIIdQi1dYlCCKI19jZ2tvc3QYNRyLc3gYNRyLc3wYNRyLg4bAF4uMg5AHlCSOI5AHm5yOIibIBrwjoBgiEdQDpgo4AAoOFAOrr3OwGBkciRYKOAAODjQDtgo4A7olAAUGJQAFBiUABQQEEBgeJhboA79cC8AXbBgji4yDkAfHyI4iJsgGvCERDtoO2hwSGxQAGCT8/hfIB8/T19gYJxEPFxsfIycoGBiHLIszNJQMiiImin7aC9yIi+Pn6JMSJiYmJBTqDuAEFQLIFBIlBsgaisgT7BAYNIrID/AT9/iOyAgKEIAEE/4FAACFZg7EBidfcgUEABgZHIvAF2dri4yAGDOQCyoFCAAoixoFDAIFEAMnKBgghgUUAIoFGAIFHAIFIACYGI4jkAsqBQgALIsaBQwCBRADJygYIIYFJACKBRgCBRwCBSAAmBiOIAbaC29yBSgCBSwDcgUEABhFHIvAGECOBSwDcgUwABhMi8AYRIyPcgU0ACgsjUK8KQ6SyAQVBsgaC8wCBTgDoAsSJiQM6g+cBA7IDAolBsgSisgIBAUKyAQKD4AGBTwABQrIBA0GyBILAAYmftoSBUAAiQraGIkKH")
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
                                `("?" mode-line "No known backends"))
                               (some-waiting
                                `("Wait" compilation-mode-line-run
                                  ,(format "Waiting for %s running backend(s)"
                                           (length some-waiting))))
                               (all-disabled
                                `("!" compilation-mode-line-run
                                  "All backends disabled"))
                               (t
                                `(nil nil nil)))))
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
               for (type . severity)
               in (cl-sort (mapcar (lambda (type)
                                     (cons type (flymake--lookup-type-property
                                                 type
                                                 'severity
                                                 (warning-numeric-level :error))))
                                   (cl-union (hash-table-keys diags-by-type)
                                             '(:error :warning)))
                           #'>
                           :key #'cdr)
               for diags = (gethash type diags-by-type)
               for face = (flymake--lookup-type-property type
                                                         'mode-line-face
                                                         'compilation-error)
               when (or diags
                        (>= severity (warning-numeric-level :warning)))
               collect `(:propertize
                         ,(format "%d" (length diags))
                         face ,face
                         mouse-face mode-line-highlight
                         keymap
                         ,(let ((map (make-sparse-keymap))
                                (type type))
                            (define-key map (vector 'mode-line
                                                    mouse-wheel-down-event)
                              (lambda (event)
                                (interactive "e")
                                (with-selected-window (posn-window (event-start event))
                                  (flymake-goto-prev-error 1 (list type) t))))
                            (define-key map (vector 'mode-line
                                                    mouse-wheel-up-event)
                              (lambda (event)
                                (interactive "e")
                                (with-selected-window (posn-window (event-start event))
                                  (flymake-goto-next-error 1 (list type) t))))
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
               `((:propertize "[")
                 ,@(cl-loop for (a . rest) on forms by #'cdr
                            collect a when rest collect
                            '(:propertize " "))
                 (:propertize "]")))))))))
