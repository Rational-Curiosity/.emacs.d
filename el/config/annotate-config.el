;;; annotate-config.el --- Configure annotate

;;; Commentary:

;; Usage:
;; (require 'annotate-config)

;;; Code:
(require 'annotate)
(setcar (cdr (assq 'annotate-mode minor-mode-alist)) "📓")

(add-hook 'find-file-hook 'annotate-mode)

(when (boundp 'personal-notes-directory)
  (setq annotate-file (expand-file-name
                       ".annotations"
                       personal-notes-directory)))

(setq annotate-use-messages nil
      annotate-annotation-column 80
      annotate-annotation-max-size-not-place-new-line 10)

(defun annotate-refresh ()
  (interactive)
  (if (null annotate-mode)
      (annotate-mode)
    (annotate-with-inhibit-modification-hooks
     (annotate-mode -1)
     (annotate-mode 1))))

(defun org-annotate-file-search (arg)
  (interactive "P")
  (if arg
      (let ((org-annotate-file-add-search t))
        (org-annotate-file))
    (org-annotate-file)))

(setcdr annotate-mode-map nil)
(define-key annotate-mode-map (kbd "C-c A a") 'annotate-annotate)
(define-key annotate-mode-map (kbd "C-c A s") 'annotate-show-annotation-summary)
(define-key annotate-mode-map (kbd "C-c A n") 'annotate-goto-next-annotation)
(define-key annotate-mode-map (kbd "C-c A p") 'annotate-goto-previous-annotation)
(define-key annotate-mode-map (kbd "C-c A r") 'annotate-refresh)
(global-set-key (kbd "C-c A f") 'org-annotate-file-search)


(provide 'annotate-config)
;;; annotate-config.el ends here
