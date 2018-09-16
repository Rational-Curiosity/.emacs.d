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

(setq py-custom-temp-directory temporary-file-directory
      python-shell-interpreter-args (cond
                                     ((or (string-equal python-shell-interpreter "ipython")
                                          (string-equal python-shell-interpreter "ipython3"))
                                      "-i --simple-prompt")
                                     (t "-i"))
      python-indent-guess-indent-offset nil)

(defvar python-command-version (cond
                                ((or
                                  (string-equal python-shell-interpreter "python3")
                                  (string-equal python-shell-interpreter "ipython3"))
                                 "python3")
                                (t "python")))
(defvar python-syntax-check-command (executable-find "flake8"))

(defun toggle-python-version ()
  (interactive)
  (cond
   ((string-equal python-shell-interpreter "python3")
    (setq python-shell-interpreter "python"
          python-command-version "python"
          python-syntax-check-command
          (or (executable-find "~/.emacs.d/cache/python/flake82")
              "flake8")))
   ((string-equal python-shell-interpreter "ipython3")
    (setq python-shell-interpreter "ipython"
          python-command-version "python"
          python-syntax-check-command
          (or (executable-find "~/.emacs.d/cache/python/flake82")
              "flake8")))
   ((string-equal python-shell-interpreter "python")
    (setq python-shell-interpreter "python3"
          python-command-version "python3"
          python-syntax-check-command
          (or (executable-find "~/.emacs.d/cache/python/flake83")
              "flake8")))
   ((string-equal python-shell-interpreter "ipython")
    (setq python-shell-interpreter "ipython3"
          python-command-version "python3"
          python-syntax-check-command
          (or (executable-find "~/.emacs.d/cache/python/flake83")
              "flake8")))
   (t (setq python-shell-interpreter "python3"
            python-command-version "python3"
            python-syntax-check-command
            (or (executable-find "~/.emacs.d/cache/python/flake83")
              "flake8"))))
  (setq flycheck-python-flake8-executable python-syntax-check-command))
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

(defun ipython-to-python-ring-save (&optional arg)
  (interactive "P")
  (if (use-region-p)
      (let* ((ipython-str (filter-buffer-substring (region-beginning) (region-end)))
             (python-str (ipython-to-python-text ipython-str)))
        (kill-new python-str)
        (setq deactivate-mark t))))

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
(require 'sphinx-frontend-config)

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

(cond
 ((or
   (string-equal python-shell-interpreter "ipython")
   (string-equal python-shell-interpreter "ipython3"))
  (define-key inferior-python-mode-map (kbd "M-w")
    #'ipython-to-python-ring-save)))

(provide 'python-config)
;;; python-config.el ends here

