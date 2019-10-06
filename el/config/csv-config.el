;;; csv-config.el --- Configure csv mode

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'csv
;;   (require 'csv-config))

;;; Code:
(setq csv-separators '("," ";" "	" "|")
      csv-field-quotes '("\"")  ;; ("\"" "'" "`")
      csv-comment-start-default nil  ;; "#"
      csv-comment-start nil  ;; "#"
      csv-align-style 'auto
      csv-align-padding 1
      csv-header-lines 1
      csv-invisibility-default nil)

(defun count-occurrences-in-current-line (char)
  "Count occurrences of CHAR in current line."
  (cl-count char (buffer-substring-no-properties (line-beginning-position) (line-end-position))))

(defun csv-count-occurrences-separators-in-current-line ()
  "Return list of count occurrences of csv separators in current line."
  (mapcar #'count-occurrences-in-current-line (mapcar #'string-to-char (default-value 'csv-separators))))

(defun csv-separators-max (&optional line)
  "Return max occurrence separator."
  (save-excursion
    (goto-char (point-min))
    (if line (forward-line (1- line)))
    (let* ((frec (csv-count-occurrences-separators-in-current-line))
           (assoc-list (cl-mapcar #'cons frec (default-value 'csv-separators))))
      (while (and (cl-every (lambda (number)
                              (= 0 number))
                            frec)
                  (= 0 (forward-line 1)))
        (setq frec (csv-count-occurrences-separators-in-current-line)
              assoc-list (cl-mapcar #'cons frec (default-value 'csv-separators))))
      (if (cl-every (lambda (number)
                      (= 0 number))
                    frec)
          nil
        (cdr (assoc (seq-max frec) assoc-list))))))

(defun csv-detect-separator (&optional line)
  "Detect csv separator in current buffer and update csv variables."
  (interactive (list (line-number-at-pos)))
  ;; (make-local-variable 'csv-separators)
  (let ((csv-sep-max (csv-separators-max line)))
    (if (not csv-sep-max)
        (message "CSV separator not found, call `csv-detect-separator' or restart `csv-mode'")
      (csv-set-comment-start nil)
      (setq-local csv-separators (list csv-sep-max))
      (setq-local csv-separator-chars (mapcar #'string-to-char csv-separators))
      (setq-local csv--skip-regexp (concat "^\n" csv-separator-chars))
      (setq-local csv-separator-regexp (concat "[" csv-separator-chars "]"))
      (setq-local csv-font-lock-keywords (list (list csv-separator-regexp '(0 'csv-separator-face))))
      (message "CSV separator detected: %s" csv-separators))))

;;(add-hook 'csv-mode-hook #'csv-detect-separator)

;; Keys
(define-key csv-mode-map (kbd "<tab>") 'csv-forward-field)
(define-key csv-mode-map (kbd "<backtab>") 'csv-backward-field)


(provide 'csv-config)
;;; csv-config.el ends here
