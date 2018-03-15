(require 'comint)

(defun comint-snapshot-last-prompt ()
  "Snapshot the current `comint-last-prompt'.
Freezes the `font-lock-face' text property in place."
  (when comint-last-prompt
    (with-silent-modifications
      (font-lock-prepend-text-property
       (car comint-last-prompt)
       (cdr comint-last-prompt)
       'font-lock-face 'comint-highlight-prompt))
    ;; Reset comint-last-prompt so later on comint-output-filter does
    ;; not remove the font-lock-face text property of the previous
    ;; (this) prompt.
    (setq comint-last-prompt nil)))

;; The purpose of using this filter for comint processes
;; is to keep comint-last-input-end from moving forward
;; when output is inserted.
(defun comint-output-filter (process string)
  (let ((oprocbuf (process-buffer process)))
    ;; First check for killed buffer or no input.
    (when (and string oprocbuf (buffer-name oprocbuf))
      (with-current-buffer oprocbuf
        ;; Run preoutput filters
        (let ((functions comint-preoutput-filter-functions))
          (while (and functions string)
            (if (eq (car functions) t)
                (let ((functions
                       (default-value 'comint-preoutput-filter-functions)))
                  (while (and functions string)
                    (setq string (funcall (car functions) string))
                    (setq functions (cdr functions))))
              (setq string (funcall (car functions) string)))
            (setq functions (cdr functions))))

        ;; Insert STRING
        (let ((inhibit-read-only t)
              ;; The point should float after any insertion we do.
              (saved-point (copy-marker (point) t)))

          ;; We temporarily remove any buffer narrowing, in case the
          ;; process mark is outside of the restriction
          (save-restriction
            (widen)

            (goto-char (process-mark process))
            (set-marker comint-last-output-start (point))

            ;; Try to skip repeated prompts, which can occur as a result of
            ;; commands sent without inserting them in the buffer.
            (let ((bol (save-excursion (forward-line 0) (point)))) ;No fields.
              (when (and (not (bolp))
                         (looking-back comint-prompt-regexp bol))
                (let* ((prompt (buffer-substring bol (point)))
                       (prompt-re (concat "\\`" (regexp-quote prompt))))
                  (while (string-match prompt-re string)
                    (setq string (substring string (match-end 0)))))))
            (while (string-match (concat "\\(^" comint-prompt-regexp
                                         "\\)\\1+")
                                 string)
              (setq string (replace-match "\\1" nil nil string)))

            ;; insert-before-markers is a bad thing. XXX
            ;; Luckily we don't have to use it any more, we use
            ;; window-point-insertion-type instead.
            (insert string)

            ;; Advance process-mark
            (set-marker (process-mark process) (point))

            (unless comint-inhibit-carriage-motion
              ;; Interpret any carriage motion characters (newline, backspace)
              (comint-carriage-motion comint-last-output-start (point)))

            ;; Run these hooks with point where the user had it.
            (goto-char saved-point)
            (run-hook-with-args 'comint-output-filter-functions string)
            (set-marker saved-point (point))

            (goto-char (process-mark process)) ; In case a filter moved it.

            (unless comint-use-prompt-regexp
              (with-silent-modifications
                (add-text-properties comint-last-output-start (point)
                                     '(front-sticky
                                       (field inhibit-line-move-field-capture)
                                       rear-nonsticky t
                                       field output
                                       inhibit-line-move-field-capture t))))

            ;; Highlight the prompt, where we define `prompt' to mean
            ;; the most recent output that doesn't end with a newline.
            (let ((prompt-start (save-excursion (forward-line 0) (point)))
                  (inhibit-read-only t))
              (when comint-prompt-read-only
                (with-silent-modifications
                  (or (= (point-min) prompt-start)
                      (get-text-property (1- prompt-start) 'read-only)
                      (put-text-property (1- prompt-start)
                                         prompt-start 'read-only 'fence))
                  (add-text-properties prompt-start (point)
                                       '(read-only t front-sticky (read-only)))))
              (when comint-last-prompt
                (let ((start (car comint-last-prompt))
                      (limit (cdr comint-last-prompt))
                      face end)
                  (with-silent-modifications
                    (while
                        (progn
                          (setq end
                                (next-single-property-change start
                                                             'font-lock-face
                                                             nil
                                                             limit))
                          (setq face (get-text-property start 'font-lock-face))
                          (put-text-property
                           start end 'font-lock-face
                           (if (and (consp face)
                                    (not (or
                                          (eq (car face) 'foreground-color)
                                          (eq (car face) 'background-color)
                                          (keywordp (car face)))))
                               (remove 'comint-highlight-prompt face)
                             (unless (eq face 'comint-highlight-prompt)
                               face)))
                          (< (setq start end) limit))))))
              (setq comint-last-prompt
                    (cons (copy-marker prompt-start) (point-marker)))
              (with-silent-modifications
                (font-lock-prepend-text-property prompt-start (point)
                                                 'font-lock-face
                                                 'comint-highlight-prompt)
                (add-text-properties prompt-start (point) '(rear-nonsticky t))))
            (goto-char saved-point)))))))



(provide 'comint-bug)