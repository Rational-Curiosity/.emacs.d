;;; yasnippet-config.el --- Configure and improve yasnippet

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'yasnippet
;;   (require 'yasnippet-config))
;; or:
;; (with-eval-after-load 'yasnippet-config
;;   )
;; never:
;; (require 'yasnippet-config)

;; Do not include in this file:
;; (require 'yasnippet)

;;; Code:

(message "Importing yasnippet-config")
(defface mode-line-yasnippet-mode
  '((t :inherit (mode-line) :foreground "medium slate blue" :weight bold))
  "Project name" :group 'mode-line)
(setcar (cdr (assq 'yas-minor-mode minor-mode-alist)) (propertize "Y"
                                                                  'face
                                                                  'mode-line-yasnippet-mode))

(require 'yasnippet-snippets)
(setq yas-snippet-dirs
      (list (expand-file-name "~/.emacs.d/cache/snippets")))

(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)
(define-key yas-minor-mode-map (kbd "<backtab>") 'yas-expand-from-trigger-key)
(define-key yas-keymap [tab] nil)
(define-key yas-keymap (kbd "TAB") nil)
(define-key yas-keymap [(control tab)] #'yas-next-field-or-maybe-expand)
(define-key yas-keymap (kbd "<C-tab>") #'yas-next-field-or-maybe-expand)

(defun python-args-to-google-docstring (text &optional make-fields)
  "Return a reST docstring format for the python arguments in yas-text.
TEXT - list of python args
MAKE-FIELDS - t or nil"
  (let* ((indent (concat "\n" (make-string (current-column) 32)))
         (args (python-split-args text))
         (nr 0)
         (formatted-args
          (mapconcat
           (lambda (x)
             (concat "   " (nth 0 x)
                     (when make-fields
                       (format " (${%d:arg%d}): ${%d:arg%d}"
                               (cl-incf nr) nr (cl-incf nr) nr))
             (when (nth 1 x) (concat " \(default " (nth 1 x) "\)"))))
       args
       indent)))
    (unless (string= formatted-args "")
      (concat
       (mapconcat 'identity
          (list "" "Args:" formatted-args)
          indent)
       "\n"))))

(yasnippet-snippets-initialize)
(yas-reload-all)


(provide 'yasnippet-config)
;;; yasnippet-config.el ends here
