;;; fido-at-point.el --- fido-style completion-at-point

;;; Commentary:

;; thanks to: https://github.com/katspaugh/ido-at-point

;; Usage:

;;
;; (require 'fido-at-point)

;;; Code:

(defun fido-at-point-complete (start end collection &optional predicate)
  "Completion for symbol at point using `completing-read'."
  (let ((comps (completion-all-completions
                (buffer-substring-no-properties start end)
                collection predicate (- end start))))
    ;; No candidates
    (if (null comps)
        (message "No matches")
      (let* ((first (car comps))
             (common-len (fido-at-point-common-length first)))
        ;; Remove the last non-nil element of a possibly improper list
        (nconc comps nil)
        (if (null (cdr comps))
            ;; Single candidate
            (fido-at-point-insert start end common-len first)
          ;; Many candidates
          (let ((common (substring-no-properties first 0 common-len)))
            (fido-at-point-do-complete start end common-len comps common)))))))

(defun fido-at-point-do-complete (start end common-len comps common)
  (run-with-idle-timer
   0 nil
   `(lambda ()
      (let ((choice (completing-read "" (quote ,comps) nil nil ,common)))
        (when (stringp choice)
          (fido-at-point-insert ,start ,end ,common-len choice))))))

(defun fido-at-point-common-length (candidate)
  ;; Completion text should have a property of
  ;; `(face completions-common-part)'
  ;; which we'll use to determine whether the completion
  ;; contains the common part.
  (let ((pos 0)
        (len (length candidate)))
    (while (and (<= pos len)
                (let ((prop (get-text-property pos 'face candidate)))
                  (not (eq 'completions-common-part
                           (if (listp prop) (car prop) prop)))))
      (setq pos (1+ pos)))
    (if (< pos len)
        (or (next-single-property-change pos 'face candidate) len)
      0)))

(defun fido-at-point-insert (start end common-part-length completion)
  "Replaces text in buffer from END back to COMMON-PART-LENGTH
with COMPLETION."
  (let ((reg-start (- end common-part-length)))
    (goto-char end)
    (delete-region (max start reg-start) end)
    (insert (substring-no-properties completion))))

(defun fido-at-point-completion-in-region (&rest args)
  (if (boundp 'completion-in-region-function)
      (if (window-minibuffer-p)
          (with-no-warnings
            (apply #'completion--in-region args))
        (apply #'fido-at-point-complete args))
    (if (window-minibuffer-p)
        (apply (car args) (cdr args))
      (apply #'fido-at-point-complete (cdr args)))))

(defvar fido-at-point-previous-completion-in-region-function nil)

(defun fido-at-point-mode-set (enable)
  (if (boundp 'completion-in-region-function)
      (if enable
          (setq fido-at-point-previous-completion-in-region-function
                completion-in-region-function
                completion-in-region-function
                'fido-at-point-completion-in-region)
        (setq completion-in-region-function
              fido-at-point-previous-completion-in-region-function))
    (with-no-warnings
      (if enable
          (add-to-list 'completion-in-region-functions
                       'fido-at-point-completion-in-region)
        (setq completion-in-region-functions
              (delq 'fido-at-point-completion-in-region
                    completion-in-region-functions))))))

(define-minor-mode fido-at-point-mode
  "Global minor mode to use ido for `completion-at-point'.
When called interactively, toggle `fido-at-point-mode'.  With
prefix ARG, enable `fido-at-point-mode' if ARG is positive,
otherwise disable it.
When called from Lisp, enable `fido-at-point-mode' if ARG is
omitted, nil or positive.  If ARG is `toggle', toggle
`fido-at-point-mode'.  Otherwise behave as if called
interactively.
With `fido-at-point-mode' use ido for `completion-at-point'."
  :variable ((if (boundp 'completion-in-region-function)
                 (eq completion-in-region-function
                     'fido-at-point-completion-in-region)
               (with-no-warnings
                 (memq 'fido-at-point-completion-in-region
                       completion-in-region-functions)))
             .
             fido-at-point-mode-set))


(provide 'fido-at-point)
;;; fido-at-point.el ends here
