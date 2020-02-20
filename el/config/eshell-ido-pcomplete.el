;;; eshell-ido-pcomplete.el --- use ido as eshell completion

;;; Commentary:

;;; Code:

(defun eshell-ido-pcomplete--input-lisp-p()
  (= ?\( (elt (eshell-get-old-input) 0)))

(defun eshell-ido-pcomplete--incomplete-input-lisp-function-p()
  (when (eshell-ido-pcomplete--input-lisp-p)
    (save-excursion
      (let ((cur-point (point))
            (delim-point (search-backward-regexp  "([ \t\r\n]*\\|'\\|[^( \t\r\n][ \t\r\n]+")))
        (if (= ?\( (char-after delim-point))
            (string-trim (buffer-substring-no-properties (+ 1 delim-point) cur-point))
          nil)))))

(defun eshell-ido-pcomplete--incomplete-input-lisp-symbol-p()
  (when (eshell-ido-pcomplete--input-lisp-p)
    (save-excursion
      (let ((cur-point (point))
            (delim-point (search-backward-regexp "([ \t\r\n]*\\|'\\|[^( \t\r\n][ \t\r\n]+")))
        (if (= ?\' (char-after delim-point))
            (string-trim (buffer-substring-no-properties (+ 1 delim-point) cur-point))
          nil)))))

(defun eshell-ido-pcomplete--incomplete-input-lisp-variable-p()
  (when (eshell-ido-pcomplete--input-lisp-p)
    (save-excursion
      (let ((cur-point (point))
            (delim-point (search-backward-regexp "([ \t\r\n]*\\|'\\|[^( \t\r\n][ \t\r\n]+")))
        (if (find (char-after (1+ delim-point)) " )")
            (string-trim (buffer-substring-no-properties (+ 1 delim-point) cur-point))
          nil)))))

(defun eshell-ido-pcomplete--special-form-or-function-p (object)
  (or (functionp object)
      (special-form-p object)))

(defun eshell-ido-pcomplete--pcomplete-completions()
  (let (completions)
    (cond ((setq pcomplete-stub (eshell-ido-pcomplete--incomplete-input-lisp-function-p))
           (mapatoms (lambda (x)
                       (when (eshell-ido-pcomplete--special-form-or-function-p x)
                         (push x completions))))
           completions)
          ((setq pcomplete-stub (eshell-ido-pcomplete--incomplete-input-lisp-variable-p))
           (mapatoms (lambda (x)
                       (unless (eshell-ido-pcomplete--special-form-or-function-p x)
                         (push x completions))))
           completions)
          ((setq pcomplete-stub (eshell-ido-pcomplete--incomplete-input-lisp-symbol-p))
           (mapatoms (lambda (x)
                       (push x completions)))
           completions)
          (t (pcomplete-completions)))))

(defun eshell-ido-pcomplete ()
  (interactive)
  ;; @ To simplify completion function logic, the tag `pcompleted' may be thrown with a value of nil in order to abort the function.  It means that there were no completions available.
  (catch 'pcompleted
    (let* (completion-result
           (completions (eshell-ido-pcomplete--pcomplete-completions))
           (candidates (all-completions pcomplete-stub completions))
           (pcomplete-stub (replace-regexp-in-string ".*[\/]" "" pcomplete-stub))
           )
      (cond ((null candidates)
             (error "Candidates not found"))
            ((= 1 (length candidates))
             (setq completion-result (car candidates)))
            (t (setq completion-result (ido-completing-read ": " candidates nil nil pcomplete-stub))))
      (delete-char (- (length pcomplete-stub)))
      (insert (eshell-quote-argument completion-result)))))


(provide 'eshell-ido-pcomplete)
;;; eshell-ido-pcomplete.el ends here
