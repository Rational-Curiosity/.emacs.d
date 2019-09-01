;;; csv-config.el --- Configure csv mode

;;; Commentary:

;; Usage:
;; (require 'csv-config)

;;; Code:
(require 'csv-mode)

(setq csv-separators '("," ";" "\t" "|")
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

(defun csv-separators-max ()
  "Return max occurrence separator in current line."
  (let* ((frec (csv-count-occurrences-separators-in-current-line))
         (assoc-list (cl-mapcar #'cons frec (default-value 'csv-separators))))
    (when (cl-every (lambda (number)
                      (= 0 number))
                    frec)
      (message "CSV separator not found, restart csv-mode"))
    (cdr (assoc (seq-max frec) assoc-list))))

(defun csv-detect-separator ()
  "Detect csv separator in current buffer and update csv variables."
  ;; (make-local-variable 'csv-separators)
  (csv-set-comment-start nil)
  (setq-local csv-separators (list (csv-separators-max)))
  (setq-local csv-separator-chars (mapcar #'string-to-char csv-separators))
  (setq-local csv--skip-regexp (concat "^\n" csv-separator-chars))
  (setq-local csv-separator-regexp (concat "[" csv-separator-chars "]"))
  (setq-local csv-font-lock-keywords (list (list csv-separator-regexp '(0 'csv-separator-face))))
  (message "CSV separator detected: %s" csv-separators))

(add-hook 'csv-mode-hook #'csv-detect-separator)

;; Keys
(define-key csv-mode-map (kbd "<tab>") 'csv-forward-field)
(define-key csv-mode-map (kbd "<backtab>") 'csv-backward-field)


(provide 'csv-config)
;;; csv-config.el ends here
