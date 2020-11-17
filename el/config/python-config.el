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

;;;;;;;;;;;;;;;;;;;
;; Configuration ;;
;;;;;;;;;;;;;;;;;;;
(setq python-shell-extra-pythonpaths (let ((python-paths (getenv "PYTHONPATH")))
                                       (and
                                        python-paths
                                        (mapcar 'expand-file-name
                                                (split-string python-paths ":"))))
      python-indent-guess-indent-offset nil)

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
          (replace-match "" t t)))
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
  (let ((case-fold-search nil))
    (replace-regexp-in-string
     "-*\n.*Traceback (most recent call last)" "Traceback (most recent call last):"
     (replace-regexp-in-string
      "^In \\[[0-9]+\\]: " ">>> "
      (replace-regexp-in-string
       "^Out\\[[0-9]+\\]: " ""
       (replace-regexp-in-string
        "^  +\\.\\.\\.: " "... " string t t) t t) t t) t t)))

(defun pypy-to-python-text (string)
   (replace-regexp-in-string
    "^>>>> " ">>> "
     (replace-regexp-in-string
      "^\\.\\.\\.\\. " "... " string t t) t t))

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
          (replace-match "" t t))
        (goto-char end)
        (insert import-list)))))

(defun python-find-test (text &optional arg)
  (interactive
   (list
    (save-excursion
      (re-search-backward "^_\\{10,\\} [_A-Z][_a-zA-Z0-9\\., ]* _\\{10,\\}$")
      (re-search-forward "[A-Z]")
      (let ((def (buffer-substring-no-properties-thing "symbol")))
        (read-string
         (if def
             (concat "Test (" def "): ")
           "Test <class>.<fun>: ") nil nil
         def)))
    (prefix-numeric-value current-prefix-arg)))
  (let ((strings (split-string text "\\." t)))
    (set-process-filter
     (start-process "*python-find-test*" nil
                    "rg" "--no-heading" "--color=never" "-lUH0"
                    "--multiline-dotall"
                    (mapconcat 'identity strings ".*?"))
     `(lambda (proc line)
        (,(cl-case arg
            ((2 4) 'find-file-other-window)
            ((3 5 16) 'find-file-other-frame)
            (otherwise 'find-file))
         line)
        (goto-char (point-min))
        (dolist (string (quote ,strings))
          (search-forward string))))))

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

(define-key python-mode-map (kbd "C-c t d") #'python-doctest-to-message)
(define-key python-mode-map (kbd "C-c t t") #'python-timeit-to-message)
(define-key python-mode-map (kbd "C-c d h") #'sphinx-build-html)
(define-key python-mode-map (kbd "C-c a") #'python-nav-beginning-of-statement)
(define-key python-mode-map (kbd "C-c e") #'python-nav-end-of-statement)
(define-key python-mode-map (kbd "M-s 7 v") #'toggle-python-version)


(provide 'python-config)
;;; python-config.el ends here
