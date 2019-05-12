;;; highlight-thing-config.el --- Configure highlight thing

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'highlight-thing
;;   (require 'highlight-thing-config))
;; or:
;; (with-eval-after-load 'highlight-thing-config
;;   )
;; never:
;; (require 'highlight-thing-config)

;; Do not include in this file:
;; (require 'highlight-thing)

;;; Code:

(message "Importing highlight-thing-config")
(setcar (cdr (assq 'highlight-thing-mode minor-mode-alist)) "")

(setq highlight-thing-what-thing 'symbol
      highlight-thing-limit-to-defun nil
      highlight-thing-case-sensitive-p t
      highlight-thing-exclude-thing-under-point t
      highlight-thing-delay-seconds 0.4)

(face-spec-set 'highlight-thing '((t (:background "dark slate gray" :inherit nil))))


(provide 'highlight-thing-config)
;;; highlight-thing-config.el ends here
