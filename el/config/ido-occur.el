;;; ido-occur.el --- Another `occur' with `ido'.

;;; Code:

(require 'ido)
(require 'dash)

(defgroup ido-occur nil
  "Yet another `occur' with `ido'."
  :group 'help
  :group 'convenience)

(defcustom ido-occur--prompt "List lines matching: "
  "Minibuffer prompt."
  :type 'string
  :group 'ido-occur)

(defcustom ido-occur--decorations
  '("\n-> " "" "\n   " "\n   ..." "[" "]"
    " [No match]" " [Matched]" " [Not readable]"
    " [Too big]" " [Confirm]")
  "Decorations for when ther is no vertical mode."
  :type 'list
  :group 'ido-occur)

(defun ido-occur--lines-as-string (buffer)
  "Get all lines with properties of the `BUFFER'."
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (buffer-substring (point-min) (point-max)))))

(defun ido-occur--decorate-lines-list (lines)
  "Add line number to each string in `LINES'."
  (let* ((lines-count (number-to-string (length lines))))
    (-map-indexed (lambda (index str)
                    "Transform `INDEX' to number and add to `STR'."
                    (let* ((line-number (+ index 1))
                           (number-length (number-to-string (length lines-count)))
                           (formated-line-number (format (concat "%" number-length "s")
                                                         line-number)))
                      (format "%s %s" formated-line-number str)))
                  lines)))

(defun ido-occur--lines-as-list (buffer line-number)
  "List all lines of `BUFFER' with respects to current `LINE-NUMBER'.
List lines from `LINE-NUMBER' to end of `BUFFER'
and from end of `BUFFER' to beginning of `BUFFER'."
  (let ((line-number (line-number-at-pos))
        (lines (ido-occur--decorate-lines-list
                (split-string (ido-occur--lines-as-string buffer) "\n"))))
    (-concat (-drop (- line-number 1) lines)
             (-take line-number lines))))

(defun ido-occur--strip-text-properties (txt)
  "Strip text properties from `TXT'."
  (set-text-properties 0 (length txt) nil txt) txt)

(defun ido-occur--visible-momentary-highlight-region (beg end)
  (mapc (lambda (overlay) (overlay-put overlay 'invisible nil)) (overlays-in beg end))
  (pulse-momentary-highlight-region beg end))

(defun ido-occur--run (&optional query)
  "Yet another `occur' with `ido'.
When non-nil, QUERY is the initial search pattern."
  (let ((initial-point (point))
        (ido-occur--line-number (line-number-at-pos)))
    (unwind-protect
        (let* ((line (ido-occur--strip-text-properties
                      (ido-completing-read ido-occur--prompt
                                           (ido-occur--lines-as-list (current-buffer)
                                                                     ido-occur--line-number)
                                           nil
                                           t
                                           query)))
               (line-number (string-to-number (car (split-string line)))))
          (if (< line-number 1)
              (user-error "Wrong selection `%s' corresponds to line %s" line line-number)
            (push-mark initial-point)
            (setq initial-point nil)
            (forward-line (- line-number ido-occur--line-number))
            (re-search-forward (if ido-enable-regexp ido-text (regexp-quote ido-text)))
            (ido-occur--visible-momentary-highlight-region (match-beginning 0) (match-end 0))))
      (if initial-point (goto-char initial-point)))))

(defun ido-preview ()
  (interactive)
  (when (< 0 ido-occur--line-number)
    (let ((line-number (string-to-number (car (split-string (car ido-matches))))))
      (with-selected-window (minibuffer-selected-window)
        (forward-line (- line-number ido-occur--line-number))
        (re-search-forward (if ido-enable-regexp ido-text (regexp-quote ido-text)))
        (ido-occur--visible-momentary-highlight-region (match-beginning 0) (match-end 0)))
      (setq ido-occur--line-number line-number))))

(defun ido-preview-prev ()
  (interactive)
  (ido-prev-match)
  (ido-preview))

(defun ido-preview-next ()
  (interactive)
  (ido-next-match)
  (ido-preview))

;;;###autoload
(defun ido-occur (&optional query)
  "Yet another `occur' with `ido'.
When non-nil, QUERY is the initial search pattern."
  (interactive)
  ;; Because in original "Ido" matcher preserves candidates sort order.
  (when (fboundp 'ido-clever-match-disable) (ido-clever-match-disable))
  (cond ((bound-and-true-p ido-vertical-mode)
         (ido-occur--run query))
        ((bound-and-true-p ido-grid-mode)
         (let ((ido-grid-mode-max-columns 1)
               (ido-grid-mode-max-rows 8)
               (ido-grid-mode-prefix-scrolls t))
           (ido-occur--run query)))
        (t
         (let ((ido-decorations ido-occur--decorations))
           (ido-occur--run query))))
  (when (fboundp 'ido-clever-match-enable) (ido-clever-match-enable)))

;;;###autoload
(defun ido-occur-dwim (arg)
  "Open `ido-occur' at point."
  (interactive "P")
  (ido-occur (if arg
                 (pcase arg
                   ('(4) ido-text)
                   ('(16) (if (eq isearch-regexp ido-enable-regexp)
                              isearch-string
                            (if ido-enable-regexp
                                (regexp-quote isearch-string)
                              isearch-string)))
                   (_ arg))
               (let ((symbol (symbol-at-point)))
                 (if symbol
                     (symbol-name symbol)
                   "")))))


(provide 'ido-occur)
;;; ido-occur.el ends here
