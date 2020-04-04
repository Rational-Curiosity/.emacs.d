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
                                  '("~/var/Dropbox/Brain")
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


(provide 'org-brain-config)
;;; org-brain-config.el ends here
