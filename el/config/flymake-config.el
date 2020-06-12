;;; flymake-config.el --- Configure and improve flymake

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'flymake
;;   (require 'flymake-config))
;; never:
;; (require 'flymake-config)

;; Do not include in this file:
;; (require 'flymake)

;;; Code:

(message "Importing flymake-config")

(setq flymake-no-changes-timeout ;; If nil, never start checking buffer automatically like this.
      2.0)

;; thanks to: stackoverflow.com/questions/6110691/is-there-a-way-to-make-flymake-to-compile-only-when-i-save
;; (defun flymake-after-change-function (start stop len)
;;   "Start syntax check for current buffer if it isn't already running.
;; START and STOP and LEN are as in `after-change-functions'."
;;     ;; Do nothing, don't want to run checks until I save.
;;   )

(when (require 'flymake-diagnostic-at-point nil 'noerror)
  (add-hook 'flymake-mode-hook #'flymake-diagnostic-at-point-mode)
  (setq flymake-diagnostic-at-point-timer-delay 2.0))

;; custom modeline
(when (bug-check-function-bytecode
       'flymake--mode-line-format
       "CMVDxsfIycrLBgYhzCLNziUDIoiJop+2gs8g0CDRINIgAoUnAAM/0wUEIsbHyNTKywYIIdUi1tclCCKI2Nna29zd3gYNRyLd3wYNRyLd4AYNRyLhUuLjIOQB5QkjiOQB5ucjiImyAa8I6AYIhHMA6YKMAAKDgwDq693sBgZHIkWCjAADg4sA7YKMAO6JQAFBiUABQYlAAUEBBAYHiYW4AO/YAvAF3AYI4uMg5AHx8iOIibIBrwhEQ7aDtocEhsMABgk/P4UIAgXFQ8bHyMnKywYGIcwizc4lAyKIiaKftoLzxQE6g/oAAUCyAfQBBPX2JLIDAUGyAoLiAMW2gvcC+PX2JMWJiYkEOoPOAQRAsgT5BAYOIrID+gT7/COyAgKEOgEK/T2ExwEKgzYB9gQh/gohWYI3Af2DxwGJ2N3/BgZHIvAF2tvi4yAGDOQCy4FAAAsix4FBAIFCAMrLBgghgUMAIoFEAIFFAIFGACYGI4jkAsuBQAAMIseBQQCBQgDKywYIIYFHACKBRACBRQCBRgAmBiOIAbaC3N2BSACBSQDd/wYRRyLwBhAjgUkA3YFKAAYTIvAGESMj3YFLAAsMI1CvCkOksgEEQbIFggcBgUwA6ALFiYkDOoP9AQOyAwKJQbIEorICAQFCsgECg/YBgU0AAUKyAQNBsgSC1gGJn7aEgU4AIkK2hyJChw==")
  (defun flymake--mode-line-format ()
    "Produce a pretty minor mode indicator."
    (let* ((known (hash-table-keys flymake--backend-state))
           (running (flymake-running-backends))
           (disabled (flymake-disabled-backends))
           (reported (flymake-reporting-backends))
           (diags-by-type (make-hash-table))
           (all-disabled (and disabled (null running)))
           (some-waiting (cl-set-difference running reported)))
      (maphash `(lambda (_b state)
                  (mapc (lambda (diag)
                          (push diag
                                (gethash (flymake--diag-type diag)
                                         ,diags-by-type)))
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
               (:propertize "}"))))))))

(define-key flymake-mode-map (kbd "M-g n") #'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-g M-n") #'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-g p") #'flymake-goto-prev-error)
(define-key flymake-mode-map (kbd "M-g M-p") #'flymake-goto-prev-error)
(define-key flymake-mode-map (kbd "C-c ! c") #'flymake-start)


(provide 'flymake-config)
;;; flymake-config.el ends here
