;;; completing-read-at-point.el --- completion-at-point with completing-read

;;; Commentary:

;; thanks to: https://github.com/katspaugh/ido-at-point

;; Usage:

;;
;; (require 'completing-read-at-point)

;;; Code:

(defvar completing-read-at-point-backend nil)
(defun completion--capf-wrapper-advice (orig-fun fun which)
  (let* ((res (funcall orig-fun fun which))
         (fn (car res)))
    (if fn (setq completing-read-at-point-backend fn))
    res))

(defun completing-read-at-point-complete (start end collection &optional predicate)
  "Completion for symbol at point using `completing-read'."
  (let* ((string (buffer-substring-no-properties start end))
         (comps (completion-all-completions
                 string
                 collection predicate (- end start))))
    ;; No candidates
    (if (null comps)
        (message "No matches")
      (let* ((base-size (cdr (last comps)))
             (common (substring-no-properties string base-size)))
        ;; Remove the last non-nil element of a possibly improper list
        (nconc comps nil)
        (if (null (cdr comps))
            ;; Single candidate
            (completing-read-at-point-insert start end base-size (car comps))
          ;; Many candidates
          (completing-read-at-point-do-complete start end base-size comps common))))))

(defun completing-read-at-point-do-complete (start end base-size comps common)
  (run-with-idle-timer
   0 nil
   `(lambda ()
      (let ((choice (completing-read ,(if completing-read-at-point-backend
                                          (format "`%s' " completing-read-at-point-backend)
                                        "") (quote ,comps) nil t ,common)))
        (when (stringp choice)
          (completing-read-at-point-insert ,start ,end ,base-size choice))))))

(defun completing-read-at-point-insert (start end base-size completion)
  "Replaces text in buffer from END back to COMMON-LEN
with COMPLETION."
  (goto-char end)
  (delete-region (+ start base-size)
                 end)
  (insert (substring-no-properties completion)))

(defun completing-read-at-point-completion-in-region (&rest args)
  (if (boundp 'completion-in-region-function)
      (if (window-minibuffer-p)
          (with-no-warnings
            (apply #'completion--in-region args))
        (apply #'completing-read-at-point-complete args))
    (if (window-minibuffer-p)
        (apply (car args) (cdr args))
      (apply #'completing-read-at-point-complete (cdr args)))))

(defvar completing-read-at-point-previous-completion-in-region-function nil)

(defun completing-read-at-point-mode-set (enable)
  (if (boundp 'completion-in-region-function)
      (if enable
          (prog1
              (setq completing-read-at-point-previous-completion-in-region-function
                    completion-in-region-function
                    completion-in-region-function
                    'completing-read-at-point-completion-in-region)
            (advice-add 'completion--capf-wrapper :around
                        'completion--capf-wrapper-advice))
        (advice-remove 'completion--capf-wrapper
                       'completion--capf-wrapper-advice)
        (setq completion-in-region-function
              completing-read-at-point-previous-completion-in-region-function))
    (with-no-warnings
      (if enable
          (prog1
              (add-to-list 'completion-in-region-functions
                           'completing-read-at-point-completion-in-region)
            (advice-add 'completion--capf-wrapper :around
                        'completion--capf-wrapper-advice))
        (advice-remove 'completion--capf-wrapper
                       'completion--capf-wrapper-advice)
        (setq completion-in-region-functions
              (delq 'completing-read-at-point-completion-in-region
                    completion-in-region-functions))))))

;;;###autoload
(define-minor-mode completing-read-at-point-mode
  "Global minor mode to use ido for `completion-at-point'.

When called interactively, toggle `completing-read-at-point-mode'.  With
prefix ARG, enable `completing-read-at-point-mode' if ARG is positive,
otherwise disable it.

When called from Lisp, enable `completing-read-at-point-mode' if ARG is
omitted, nil or positive.  If ARG is `toggle', toggle
`completing-read-at-point-mode'.  Otherwise behave as if called
interactively.

With `completing-read-at-point-mode' use ido for `completion-at-point'."
  :variable ((if (boundp 'completion-in-region-function)
                 (eq completion-in-region-function
                     'completing-read-at-point-completion-in-region)
               (with-no-warnings
                 (memq 'completing-read-at-point-completion-in-region
                       completion-in-region-functions)))
             . completing-read-at-point-mode-set))


(provide 'completing-read-at-point)
;;; completing-read-at-point.el ends here
