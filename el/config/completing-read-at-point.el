;;; completing-read-at-point.el --- completion-at-point with completing-read

;;; Commentary:

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

(defun crap-completion-in-region
    (start end collection &optional predicate)
  (completion-in-region-mode -1)
  (if (window-minibuffer-p)
      (with-no-warnings
        (funcall crap-previous-completion-in-region-function
                 start end collection predicate))
    (setq completion-extra-properties
          (plist-put completion-extra-properties :exit-function
                     `(lambda (proxy status)))
          completion-in-region-mode-predicate
          (lambda ()
            t))
    (let* ((string (buffer-substring-no-properties start end))
           (comps (completion-all-completions
                   string
                   collection predicate (- end start)))
           (base-size (cdr (last comps)))
           (common (substring-no-properties string base-size))
           (proxy (completing-read
                   (if crap-backend
                       (format
                        "`%s' "
                        crap-backend)
                     "")
                   `(lambda (string pred action)
                      (with-current-buffer
                          (if (minibufferp)
                              (window-buffer (minibuffer-selected-window))
                            (current-buffer))
                        (if (eq action 'metadata)
                            '(metadata
                              (annotation-function
                               .
                               (lambda (proxy)
                                 (concat
                                  ;; (make-string (max 2 (- 20 (length proxy))) ? )
                                  " -"
                                  (funcall ,(plist-get completion-extra-properties
                                                      :annotation-function)
                                          proxy)))))
                        (funcall ,collection
                               string pred action))))
                   predicate t
                   common)))
      (with-current-buffer (marker-buffer start)
        (goto-char end)
        (delete-region (+ start base-size) end)
        (insert (substring-no-properties proxy))))))

(defvar crap-previous-completion-in-region-function nil)

(defun completing-read-at-point-mode-set (enable)
  (if enable
          (prog1
              (setq crap-previous-completion-in-region-function
                    completion-in-region-function
                    completion-in-region-function
                    'crap-completion-in-region)
            (advice-add 'completion--capf-wrapper :around
                        'completion--capf-wrapper-advice))
        (advice-remove 'completion--capf-wrapper
                       'completion--capf-wrapper-advice)
        (setq completion-in-region-function
              crap-previous-completion-in-region-function)))

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
