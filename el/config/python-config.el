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

;;;;;;;;;;;;;
;; Helpers ;;
;;;;;;;;;;;;;
(defun ipython-to-python-ring-save (&optional arg)
  (interactive "P")
  (if (use-region-p)
      (let* ((ipython-str (filter-buffer-substring (region-beginning) (region-end)))
             (python-str (ipython-to-python-text ipython-str)))
        (kill-new python-str)
        (setq deactivate-mark t))))

(defun pypy-to-python-ring-save (&optional arg)
  (interactive "P")
  (if (use-region-p)
      (let* ((pypy-str (filter-buffer-substring (region-beginning) (region-end)))
             (python-str (pypy-to-python-text pypy-str)))
        (kill-new python-str)
        (setq deactivate-mark t))))

(defun set-python-interpreter-args ()
  (cond
   ((string-match-p "ipython" python-shell-interpreter)
    ;; ipython or ipython3
    (define-key inferior-python-mode-map (kbd "M-w") #'ipython-to-python-ring-save)
    (setq python-shell-interpreter-args "-i --simple-prompt"))
   ((string-match-p "pypy" python-shell-interpreter)
    (define-key inferior-python-mode-map (kbd "M-w") #'pypy-to-python-ring-save)
    (setq python-shell-interpreter-args "-i -S"))
   (t
    ;; python or python3
    (define-key inferior-python-mode-map (kbd "M-w") nil)
    (setq python-shell-interpreter-args "-i"))))

(defun set-python-interpreter (interpreter)
  (if (string-match-p "[^0-9]3[.0-9]*$" interpreter)
      (setq python-syntax-check-command (or (executable-find "~/.emacs.d/cache/python3/flake8")
                                            "flake8")
            flycheck-python-flake8-executable python-syntax-check-command
            flycheck-python-pylint-executable (or (executable-find "~/.emacs.d/cache/python3/pylint")
                                                  "pylint")
            flycheck-python-mypy-executable (or (executable-find "~/.emacs.d/cache/python3/mypy")
                                                "mypy"))
    (setq python-syntax-check-command (or (executable-find "~/.emacs.d/cache/python/flake8")
                                          "flake8")
          flycheck-python-flake8-executable python-syntax-check-command
          flycheck-python-pylint-executable (or (executable-find "~/.emacs.d/cache/python/pylint")
                                                "pylint")
          flycheck-python-mypy-executable nil))
  (setq python-shell-interpreter interpreter)
  (set-python-interpreter-args))

(defun detect-python-project-version ()
  (let* ((python-version-filename ".python-version")
         (python-version-directory (locate-dominating-file default-directory python-version-filename)))
    (when python-version-directory
      (message "%s file founded in %s" python-version-filename python-version-directory)
      (with-temp-buffer
        (insert-file-contents (concat python-version-directory python-version-filename))
        (cond
         ((search-forward-regexp "^[^0-9]*3[.0-9]*$" nil t)
          (set-python-interpreter (or (executable-find (match-string 0))
                                      (executable-find "pypy3")
                                      (executable-find "python3"))))
         ((search-forward-regexp "^[^0-9]*2[.0-9]*$" nil t)
          (set-python-interpreter (or (executable-find (match-string 0))
                                      (executable-find "pypy")
                                      (executable-find "python")))))))))

(defun toggle-python-version ()
  (interactive)
  (if (string-match-p "[^0-9]3[.0-9]*$" python-shell-interpreter)
      (setq python-shell-interpreter (or
                                      (executable-find (replace-regexp-in-string "\\([^0-9]\\)3[.0-9]*$" "\\1"
                                                                                 python-shell-interpreter))
                                      (executable-find "pypy")
                                      "python")
            python-syntax-check-command (or (executable-find "~/.emacs.d/cache/python/flake8")
                                            "flake8")
            flycheck-python-flake8-executable python-syntax-check-command
            flycheck-python-pylint-executable (or (executable-find "~/.emacs.d/cache/python/pylint")
                                                  "pylint")
            flycheck-python-mypy-executable nil)
    (setq python-shell-interpreter (or
                                    (executable-find (concat python-shell-interpreter "3"))
                                    (executable-find "pypy3")
                                    "python3")
          python-syntax-check-command (or (executable-find "~/.emacs.d/cache/python3/flake8")
                                          "flake8")
          flycheck-python-flake8-executable python-syntax-check-command
          flycheck-python-pylint-executable (or (executable-find "~/.emacs.d/cache/python3/pylint")
                                                "pylint")
          flycheck-python-mypy-executable (or (executable-find "~/.emacs.d/cache/python3/mypy")
                                              "mypy")))
  (set-python-interpreter-args))

;;;;;;;;;;;;;;;;;;;
;; Configuration ;;
;;;;;;;;;;;;;;;;;;;
(setq python-shell-extra-pythonpaths (let ((python-paths (getenv "PYTHONPATH")))
                                       (and
                                        python-paths
                                        (mapcar 'expand-file-name
                                                (split-string python-paths ":"))))
      py-custom-temp-directory temporary-file-directory
      python-indent-guess-indent-offset nil)

(set-python-interpreter (or (executable-find "pypy3")
                            (executable-find "python3")
                            (executable-find "pypy")
                            (executable-find "python")
                            "python"))

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
    "^In \\[[0-9]+\\]: " ">>> "
    (replace-regexp-in-string
     "^Out\\[[0-9]+\\]: " ""
     (replace-regexp-in-string
      "^  +\\.\\.\\.: " "... " string)))))

(defun pypy-to-python-text (string)
   (replace-regexp-in-string
    "^>>>> " ">>> "
     (replace-regexp-in-string
      "^\\.\\.\\.\\. " "... " string)))

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
           ("<f7> v"  . toggle-python-version))


(provide 'python-config)
;;; python-config.el ends here

