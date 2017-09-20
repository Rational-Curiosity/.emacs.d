;;; csv-config.el --- Configure csv mode

;;; Commentary:

;; Usage:
;; (require 'csv-config)

;;; Code:
(require 'csv-mode)

(setq csv-separators '("," ";" "\t" "|")
      csv-field-quotes '("\"" "'" "`")
      csv-comment-start-default "#"
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

(defun csv-separators-max ()
  "Return max occurrence separator in current line."
  (let* ((frec (csv-count-occurrences-separators-in-current-line))
         (assoc-list (cl-mapcar #'cons frec (default-value 'csv-separators))))
    (cdr (assoc (seq-max frec) assoc-list))))

(defun csv-detect-separator ()
  "Detect csv separator in current line and update csv variables."
  ;; (make-local-variable 'csv-separators)
  (setq-local csv-separators (list (csv-separators-max)))
  (setq-local csv-separator-chars (mapcar #'string-to-char csv-separators))
  (setq-local csv--skip-regexp (concat "^\n" csv-separator-chars))
  (setq-local csv-separator-regexp (concat "[" csv-separator-chars "]"))
  (setq-local csv-font-lock-keywords (list (list csv-separator-regexp '(0 'csv-separator-face)))))

(add-hook 'csv-mode-hook #'csv-detect-separator)

(provide 'csv-config)
;;; csv-config.el ends here
