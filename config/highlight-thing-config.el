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
(setq highlight-thing-what-thing 'symbol
      highlight-thing-limit-to-defun nil
      highlight-thing-case-sensitive-p t
      highlight-thing-exclude-thing-under-point t
      highlight-thing-delay-seconds 0.4)

(custom-set-faces
 '(highlight-thing ((t (:background "dark slate gray")))))


(provide 'highlight-thing-config)
;;; highlight-thing-config.el ends here
