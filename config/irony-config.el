;;; irony-config.el --- Configure irony

;;; Commentary:

;; Usage:
;; (require 'irony-config)

;;; Code:

(message "Importing irony-config")
;; (require 'company)
(require 'irony)

(setcar (cdr (assq 'irony-mode minor-mode-alist)) "Ir")

;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  "Hook for irony.
Replace the `completion-at-point' and `complete-symbol' bindings in
irony-mode's buffers by irony-mode's function."
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(with-eval-after-load 'c-c++-config
  (dolist (path c-c++-include-paths)
    (add-to-list 'irony-additional-clang-options
                 (concat "-I" path))))



(require 'company-irony-c-headers)
(with-eval-after-load 'company
;;  (add-to-list 'company-backends 'company-irony)
  (add-to-list
    'company-backends '(company-irony-c-headers company-irony)))

(provide 'irony-config)
;;; irony-config.el ends here
