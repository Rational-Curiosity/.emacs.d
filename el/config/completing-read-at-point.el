;;; completing-read-at-point.el --- completion-at-point with completing-read

;;; Commentary:
;; TAB (translated from <tab>) runs the command minibuffer-complete
;; (found in minibuffer-local-must-match-map), which is an interactive
;; native compiled Lisp function in ‘minibuffer.el’.

;; It is bound to TAB, <menu-bar> <minibuf> <tab>.

;; (minibuffer-complete)

;; Complete the minibuffer contents as far as possible.
;; Return nil if there is no valid completion, else t.
;; If no characters can be completed, display a list of possible completions.
;; If you repeat this command after it displayed such a list,
;; scroll the window of possible completions.



;; TAB (translated from <tab>) runs the command completion-at-point
;; (found in completion-in-region-mode-map), which is an interactive
;; native compiled Lisp function in ‘minibuffer.el’.

;; It is bound to TAB.

;; (completion-at-point)

;;   Probably introduced at or before Emacs version 23.2.

;; Perform completion on the text around point.
;; The completion method is determined by ‘completion-at-point-functions’.
;; Usage:

;;
;; (require 'completing-read-at-point)

;;; Code:

(defvar crap-backend nil)
(defun completion--capf-wrapper-advice (orig-fun fun which)
  (let* ((res (funcall orig-fun fun which))
         (fn (car res)))
    (if fn (setq crap-backend fn))
    res))

(defun crap-insert (start end base-size completion)
  (with-current-buffer (marker-buffer start)
        (goto-char end)
        (delete-region (+ start base-size) end)
        (insert (substring-no-properties completion))))

(defvar crap-completion-in-region nil)
(defun crap-completion-in-region
    (start end collection &optional predicate)
  (if (or crap-completion-in-region
          (window-minibuffer-p))
      (with-no-warnings
        (let ((completion-in-region-function
               crap-previous-completion-in-region-function))
          (completion-in-region start end collection predicate)))
    (setq completion-extra-properties
          (plist-put completion-extra-properties :exit-function
                     `(lambda (proxy status)))
          completion-in-region-mode-predicate
          (lambda ()
            t)
          crap-completion-in-region t)
    (unwind-protect
        (let* ((string (buffer-substring-no-properties start end))
               (comps (completion-all-completions
                       string
                       collection predicate (- end start))))
          ;; No candidates
          (if (null comps)
              (message "No matches")
            (let* ((base-size (cdr (last comps)))
                   (common (substring-no-properties string base-size)))
              (nconc comps nil)
              (if (null (cdr comps))
                  ;; Single candidate
                  (crap-insert start end base-size (car comps))
                ;; Many candidates
                (let ((choice
                       (completing-read
                        (if crap-backend
                            (format
                             "`%s' "
                             crap-backend)
                          "")
                        (let ((fn (plist-get completion-extra-properties
                                             :annotation-function)))
                          (if (functionp fn)
                              `(lambda (probe pred action)
                                 (cond
                                  ((eq (car-safe action) 'boundaries) nil)
                                  ((eq action 'metadata)
                                   '(metadata
                                     (annotation-function
                                      .
                                      (lambda (proxy)
                                        (concat
                                         (make-string (+
                                                       2
                                                       (- ,(apply
                                                            'max
                                                            (mapcar
                                                             'string-width
                                                             comps))
                                                          (string-width proxy)))
                                                      ? )
                                         (funcall ,fn proxy))))
                                     (display-sort-function . identity)
                                     (cycle-sort-function . identity)))
                                  (t
                                   (funcall
                                    (cond
                                     ((null action) 'try-completion)
                                     ((eq action t) 'all-completions)
                                     (t 'test-completion))
                                    probe (quote ,comps) pred))))
                            comps))
                        nil t
                        common)))
                  (when (stringp choice)
                    (crap-insert start end base-size choice)))))))
      (setq crap-completion-in-region nil))))

(defvar crap-previous-completion-in-region-function nil)

(defun completing-read-at-point-mode-set (enable)
  (if enable
      (prog1
          (setq crap-previous-completion-in-region-function
                (if (eq completion-in-region-function
                        'crap-completion-in-region)
                    'completion--in-region
                  completion-in-region-function)
                completion-in-region-function
                'crap-completion-in-region)
        (advice-add 'completion--capf-wrapper :around
                    'completion--capf-wrapper-advice))
    (advice-remove 'completion--capf-wrapper
                   'completion--capf-wrapper-advice)
    (setq completion-in-region-function
          crap-previous-completion-in-region-function
          crap-previous-completion-in-region-function nil)))

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
  :variable ((eq completion-in-region-function
                 'completing-read-at-point-completion-in-region)
             . completing-read-at-point-mode-set))


(provide 'completing-read-at-point)
;;; completing-read-at-point.el ends here
