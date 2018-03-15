;;; elpy-env-config.el --- Configure elpy and utils

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'elpy
;;   (require 'elpy-env-config))
;; or:
;; (with-eval-after-load 'elpy-env-config
;;   )
;; never:
;; (require 'elpy-env-config)

;; Do not include in this file:
;; (require 'elpy-env-mode)


;; Install python dependencies
;;;; Python3
;; sudo apt install flake8
;; sudo apt install python3-flake8
;; sudo apt install python3-jedi
;; sudo pip3 install rope_py3k
;; sudo pip3 install importmagic
;; sudo pip3 install autopep8
;; sudo pip3 install yapf
;;;; Python
;; sudo apt install flake8
;; sudo pip install flake8 --upgrade
;; sudo apt install python-jedi
;; sudo pip install jedi --upgrade
;; sudo apt install python-flake8
;; sudo pip install rope
;; sudo pip install importmagic
;; sudo pip install autopep8
;; sudo pip install yapf

;;; Code:

(message "Importing elpy-env-config")
(require 'python)
(require 'python-config)
(require 'elpy)
(setcar (cdr (assq 'elpy-mode minor-mode-alist)) "Ep")

(defun elpy-version ()
  (setq elpy-syntax-check-command python-syntax-check-command
        python-check-command elpy-syntax-check-command)
  (cond
   ((or
     (string-equal python-shell-interpreter "ipython")
     (string-equal python-shell-interpreter "ipython3"))
    (elpy-use-ipython python-shell-interpreter))))
(elpy-version)
(advice-add 'toggle-python-version :after #'elpy-version)

(advice-add 'elpy-goto-definition :before #'push-mark)

;; [ backend rope insert parents always
;; (defun elpy-company-post-complete-parens (annotation name)
;;   "Complete functions, classes, and callable instances with parentheses.

;; Add parentheses in case ANNOTATION is \"class\", \"function\", or \"instance\",
;; unless the completion is already looking at a left parenthesis,
;; or unless NAME is a Python exception outside a reasonably formed raise statement,
;; or unless NAME is no callable instance."
;;   (when (not (looking-at-p "\("))
;;     (cond ((string= annotation "function")
;;            (insert "()")
;;            (backward-char 1))
;;           ((string= annotation "class")
;;            (cond ((elpy-company--python-exception-p name)
;;                   (when (save-excursion
;;                           (backward-word 2)
;;                           (looking-at "\\_<raise\\_>"))
;;                     (insert "()")
;;                     (backward-char 1)))
;;                  (t
;;                   (insert "()")
;;                   (backward-char 1))))
;;           ;; ((string= annotation "instance")
;;           ;;  ;; The jedi backend annotates some callables as instances (e.g. numpy
;;           ;;  ;; and scipy) and `elpy-company--cache' does not allow to identify
;;           ;;  ;; callable instances.
;;           ;;  ;; It looks easy to modify `elpy-company--cache' cheaply for the jedi
;;           ;;  ;; backend to eliminate the `elpy-rpc-get-calltip' call below, but
;;           ;;  ;; not for the rope backend.
;;           ;;  (insert "()")
;;           ;;  (backward-char 1)
;;           ;;  (when (not (elpy-rpc-get-calltip))
;;           ;;    (backward-char 1)
;;           ;;    (delete-char 2)))
;;           )))
;; ]

(setq elpy-rpc-ignored-buffer-size 300000
      elpy-rpc-python-command python-command-version
      elpy-rpc-backend "jedi" ;; "jedi" or "rope"
      elpy-rpc-timeout 6
      elpy-company-post-completion-function #'elpy-company-post-complete-parens
      elpy-test-discover-runner-command `(,elpy-rpc-python-command "-m" "unittest")
      elpy-modules '(elpy-module-sane-defaults
                          elpy-module-company
                          elpy-module-eldoc
                          ;; [ flymake xor flycheck
                          ;; elpy-module-flymake
                          ;; ]
                          ;; [ poor performance
                          ;; elpy-module-highlight-indentation
                          ;; ]
                          elpy-module-pyvenv
                          elpy-module-yasnippet
                          elpy-module-django))

(elpy-modules-global-init)

(define-key elpy-mode-map (kbd "M-TAB") nil)
(define-key elpy-mode-map (kbd "<M-down>") nil)
(define-key elpy-mode-map (kbd "<M-up>") nil)
(define-key elpy-mode-map (kbd "<C-left>") nil)
(define-key elpy-mode-map (kbd "<C-right>") nil)
(define-key elpy-mode-map (kbd "<C-down>") #'forward-paragraph)
(define-key elpy-mode-map (kbd "<C-up>") #'backward-paragraph)
(define-key elpy-mode-map [(control tab)] #'elpy-company-backend);python-mode-map
(define-key inferior-python-mode-map (kbd "C-c C-z") #'elpy-shell-switch-to-buffer)


(provide 'elpy-env-config)
;;; elpy-env-config.el ends here
