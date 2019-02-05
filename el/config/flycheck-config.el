;;; flycheck-config.el --- Configure flycheck and improve it

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'flycheck
;;   (require 'flycheck-config))
;; or:
;; (with-eval-after-load 'flycheck-config
;;   )
;; never:
;; (require 'flycheck-config)

;; Do not include in this file:
;; (require 'flycheck)

;;; Code:

(message "Importing flycheck-config")
;;;;;;;;;;;
;; Julia ;;
;;;;;;;;;;;
(with-eval-after-load 'julia-mode
  (require 'flycheck-julia)
  (flycheck-julia-setup))
;;;;;;;;;;;;
;; Python ;;
;;;;;;;;;;;;
(setq flycheck-python-flake8-executable "flake8"
      flycheck-python-pylint-executable "pylint"
      flycheck-python-mypy-executable "mypy")
;;;;;;;;;;;;;;
;; Flycheck ;;
;;;;;;;;;;;;;;
;; Enable flycheck globaly
;;(add-hook 'after-init-hook #'global-flycheck-mode)
;; Enable flycheck localy
;;(add-hook 'prog-mode-hook 'flycheck-mode)
(add-hook 'c++-mode-hook
          (lambda ()
            (setq flycheck-gcc-language-standard "c++11"
                  flycheck-clang-language-standard "c++11")))

(setq flycheck-check-syntax-automatically '(save mode-enabled))

;; (require 'semantic)
;; (setq flycheck-clang-system-path (list))
;; (require 'semantic/bovine/gcc)
;; (let ((dirs (semantic-gcc-get-include-paths "c++")))
;;     (dolist (dir dirs)
;;       (add-to-list 'flycheck-clang-system-path dir)))

;; ;; Disable clang check, gcc check works better
;; (setq-default flycheck-disabled-checkers
;;               (append flycheck-disabled-checkers
;;                       '(c/c++-clang)))
(with-eval-after-load 'c-c++-config
  (dolist (path c-c++-include-paths)
    (add-to-list 'flycheck-gcc-include-path path)
    (add-to-list 'flycheck-clang-include-path path)))

;; hide 'In included' messages
(defconst flycheck-fold-include-levels-include
  (symbol-function 'flycheck-fold-include-levels))

(defun flycheck-fold-include-levels-exclude (errors sentinel-message)
  "Exclude ERRORS with SENTINEL-MESSAGE from included files."
  (unless (or (stringp sentinel-message) (functionp sentinel-message))
    (error "Sentinel must be string or function: %S" sentinel-message))
  (let ((sentinel (if (functionp sentinel-message)
                      sentinel-message
                    (lambda (err)
                      (string-match-p sentinel-message
                                      (flycheck-error-message err))))))
    (setq errors (cl-remove-if sentinel errors)))
  errors)
(defconst flycheck-fold-include-levels-exclude
  (symbol-function 'flycheck-fold-include-levels-exclude))

(defun flycheck-toggle-includes ()
  "Toggle errors in included files."
  (interactive)
  (if (eq (symbol-function 'flycheck-fold-include-levels)
          (indirect-function flycheck-fold-include-levels-include))
      (fset 'flycheck-fold-include-levels flycheck-fold-include-levels-exclude)
    (fset 'flycheck-fold-include-levels flycheck-fold-include-levels-include))
  (flycheck-buffer))

;; warning options
(defun flycheck-toggle-warnings ()
  "Toggle warnings."
  (interactive)
  (if (member "extra" flycheck-clang-warnings)
      (delete "extra" flycheck-clang-warnings)
    (add-to-list 'flycheck-clang-warnings "extra"))
  (if (member "extra" flycheck-gcc-warnings)
      (delete "extra" flycheck-gcc-warnings)
    (add-to-list 'flycheck-gcc-warnings "extra"))
  (flycheck-buffer))

(when (member "extra" flycheck-clang-warnings)
  (delete "extra" flycheck-clang-warnings))
(when (member "extra" flycheck-gcc-warnings)
  (delete "extra" flycheck-gcc-warnings))

;; Error notification
(add-hook 'flycheck-mode-hook 'flycheck-popup-tip-mode)

;; Mode-line
(defun my-flycheck-mode-line-status-text (&optional status)
  "Get a text describing STATUS for use in the mode line."
  (format
   "{%s}" (pcase (or status flycheck-last-status-change)
            (`not-checked "")
            (`no-checker "∅")
            (`running "↻")
            (`errored "✘")
            (`finished
             (let ((errors-warnings (flycheck-count-errors flycheck-current-errors)))
               (let* ((errors (cdr (assq 'error errors-warnings)))
                      (warnings (cdr (assq 'warning errors-warnings)))
                      (errors-str (when errors (format "⚐%d" errors)))
                      (warnings-str (when warnings (format "⚠%d" warnings))))
                 (cond
                  ((and errors warnings) (concat errors-str " " warnings-str))
                  (warnings warnings-str)
                  (errors errors-str)
                  (t "✓ ")))))
            (`interrupted "-")
            (`suspicious "?"))))
(setq-default flycheck-mode-line '(:eval (my-flycheck-mode-line-status-text)))

;; Keys
(bind-keys :map flycheck-mode-map
           ("C-c ! t w" . flycheck-toggle-warnings)
           ("C-c ! t i" . flycheck-toggle-includes))


(provide 'flycheck-config)
;;; flycheck-config.el ends here
