;;; org-brain-config.el --- Configure and improve brain

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'brain
;;   (require 'org-brain-config))
;; never:
;; (require 'org-brain-config)

;; Do not include in this file:
;; (require 'brain)

;;; Code:

(message "Importing org-brain-config")

(setq org-brain-path (or (cl-loop for folder in
                                  '("~/var/Dropbox/Brain"
                                    "~/Prog/org/brain")
                                  when (file-exists-p folder)
                                  return folder)
                         org-brain-path)
      org-id-track-globally t
      ;; <only headlines entries>
      ;; org-brain-include-file-entries nil
      ;; org-brain-file-entries-use-title nil
      org-brain-title-max-length 80)

(push 'org-brain-entry-todo-state org-brain-vis-current-title-prepend-functions)
(push 'org-brain-entry-tags-string org-brain-vis-current-title-append-functions)


(when (require 'helm-org-rifle nil t)
  (defun helm-org-rifle-brain ()
    "Rifle files in `org-brain-path'."
    (interactive)
    (let ((helm-org-rifle-close-unopened-file-buffers nil))
      (helm-org-rifle-directories (list org-brain-path))))

  (defun helm-org-rifle-open-in-brain (candidate)
    (-let (((buffer . pos) candidate))
      (with-current-buffer buffer
        (goto-char pos)
        (org-brain-visualize-entry-at-pt))))

  (add-to-list 'helm-org-rifle-actions
               (cons "Show entry in org-brain" 'helm-org-rifle-open-in-brain)
               t))

(when (require 'org-noter nil t)
  (add-hook 'org-noter-insert-heading-hook #'org-id-get-create)
  (defun org-brain-open-org-noter (entry)
    "Open `org-noter' on the ENTRY.
If run interactively, get ENTRY from context."
    (interactive (list (org-brain-entry-at-pt)))
    (org-with-point-at (org-brain-entry-marker entry)
      (org-noter)))

  (define-key org-brain-visualize-mode-map (kbd "\C-c n") 'org-brain-open-org-noter))


(provide 'org-brain-config)
;;; org-brain-config.el ends here
