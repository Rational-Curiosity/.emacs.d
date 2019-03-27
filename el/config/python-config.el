;;; python-config.el --- Configure and improve python

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'python-mode
;;   (require 'python-config))
;; or:
;; (with-eval-after-load 'python-config
;;   )
;; never:
;; (require 'python-config)

;; Do not include in this file:
;; (require 'python-mode)

;;; Code:

(message "Importing python-config")
(require 'python) ;; inferior-python-mode-map
(require 'flycheck)


(defun ipython-to-python-ring-save (&optional arg)
  (interactive "P")
  (if (use-region-p)
      (let* ((ipython-str (filter-buffer-substring (region-beginning) (region-end)))
             (python-str (ipython-to-python-text ipython-str)))
        (kill-new python-str)
        (setq deactivate-mark t))))

(if (string-equal "i" (substring-no-properties python-shell-interpreter 0 1))
    ;; ipython or ipython3
    (progn
      (define-key inferior-python-mode-map (kbd "M-w") #'ipython-to-python-ring-save)
      (setq python-shell-interpreter-args "-i --simple-prompt"
            python-command-version (substring-no-properties python-shell-interpreter 1)))
  ;; python or python3
  (setq python-shell-interpreter-args "-i"
        python-command-version python-shell-interpreter))


(setq py-custom-temp-directory temporary-file-directory
      python-indent-guess-indent-offset nil)

(defvar python-syntax-check-command (executable-find "flake8"))

(defun toggle-python-version ()
  (interactive)
  (if (string-equal "3" (substring-no-properties python-shell-interpreter -1))
      (setq python-shell-interpreter (substring-no-properties python-shell-interpreter 0 -1)
            python-command-version (if (string-equal "i" (substring-no-properties python-shell-interpreter 0 1))
                                       (substring-no-properties python-shell-interpreter 1)
                                     python-shell-interpreter)
            python-syntax-check-command (or (executable-find "~/.emacs.d/cache/python/flake82")
                                            "flake8"))
    (setq python-shell-interpreter (concat python-shell-interpreter "3")
          python-command-version (if (string-equal "i" (substring-no-properties python-shell-interpreter 0 1))
                                     (substring-no-properties python-shell-interpreter 1)
                                   python-shell-interpreter)
          python-syntax-check-command (or (executable-find "~/.emacs.d/cache/python/flake83")
                                          "flake8"))))

;;;;;;;;;;;;;;;
;; Functions ;;
;;;;;;;;;;;;;;;
(defun python-doctest-to-message (&optional msg)
  (interactive "p")
  (let ((process (python-shell-get-process-or-error msg))
        (string (python-shell-buffer-substring (point-min) (point-max) t)))
    
    (message (python-shell-send-string-no-output (concat string "

if __name__ == \"__main__\":
    import doctest
    print(doctest.testmod())
")))))

(defun python-timeit-to-message (&optional msg)
  (interactive "p")
  (let ((process (python-shell-get-process-or-error msg))
        (string (python-shell-buffer-substring (point-min) (point-max) t)))
    
    (message (python-shell-send-string-no-output (concat string "

if __name__ == \"__main__\":
    import timeit
    for k, v in dict(locals()).items():
        if k[:4] == '_ti_' and callable(v):
            print(v, timeit.timeit(v))
")))))


(defun python-insert-start-debuger (arg)
  (interactive "P")
  (if arg
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "[^\n][ \t]*import +pdb *; *pdb.set_trace *() *#?[^\n]*" nil t)
          (replace-match "" nil nil)))
    (insert "import pdb; pdb.set_trace()")))

(defun run-python-in-directory ()
  (interactive)
  (let ((default-directory (read-directory-name "working folder: ")))
    (run-python)))

(defun python-change-working-directory ()
  (interactive)
  (let ((default-directory (read-directory-name "working folder: ")))
    (python-shell-send-string (concat "import os;os.chdir('" default-directory "')"))))

(defun ipython-to-python-text (string)
  (replace-regexp-in-string
   "-*\n.*Traceback (most recent call last)" "Traceback (most recent call last):"
   (replace-regexp-in-string
    "In \[[0-9]+\]: " ">>> "
    (replace-regexp-in-string
     "Out\[[0-9+]\]: " ""
     (replace-regexp-in-string
      "   .+: " "... " string)))))


(defun python-import-to-multiline ()
  (interactive)
  (let* ((beg (progn (beginning-of-line) (point)))
         (end (progn (end-of-line) (point)))
         (import-string (buffer-substring-no-properties beg end)))
    (when (string-match "from +\\([^ ]+\\) +import +\\([^,]+\\), *\\(.*\\)" import-string)
      (let ((common-string (match-string 1 import-string))
            (first-string (match-string 2 import-string))
            (import-list (match-string 3 import-string)))
        (delete-and-extract-region beg end)
        (insert (concat "from " common-string " import " first-string))
        (dolist (item (split-string import-list ", *"))
          (newline)
          (insert (concat "from " common-string " import " item)))))))

(defun python-import-to-oneline ()
  (interactive)
  (let* ((beg (progn (beginning-of-line) (point)))
         (end (progn (end-of-line) (point)))
         (import-string (buffer-substring-no-properties beg end)))
    (when (string-match "from +\\([^ ]+\\) +import +.*" import-string)
      (let* ((common-string (match-string 1 import-string))
            (pattern (concat "from +" common-string " +import +\\(.*\\)
"))
            (import-list ""))
        (goto-char (point-min))
        (re-search-forward pattern nil t)
        (setq end (1- (match-end 0)))
        (while (re-search-forward pattern nil t)
          (setq import-list (concat import-list ", " (match-string-no-properties 1)))
          (replace-match ""))
        (goto-char end)
        (insert import-list)))))
;;;;;;;;;;;;
;; Sphinx ;;
;;;;;;;;;;;;
(with-eval-after-load 'sphinx-doc
  (require 'sphinx-frontend-config))

;;;;;;;;;;
;; Keys ;;
;;;;;;;;;;
(define-key python-mode-map [(control backspace)] nil)
;; (define-key python-mode-map (kbd "C-c t d")
;;   #'python-doctest-to-message)
;; (define-key python-mode-map (kbd "C-c t t")
;;   #'python-timeit-to-message)
;; (define-key python-mode-map (kbd "C-c d h")
;;   #'sphinx-build-html)

(bind-keys :map python-mode-map
           ("C-c t d" . python-doctest-to-message)
           ("C-c t t" . python-timeit-to-message)
           ("C-c d h" . sphinx-build-html)
           ("<f7> p"  . toggle-python-version))


(provide 'python-config)
;;; python-config.el ends here

