;; [ Limit semantic parser
;; (defvar semantic-fetch-tags-limit-default 15)
;; (defvar semantic-fetch-tags-limit semantic-fetch-tags-limit-default)
;; (run-with-idle-timer 120 60 #'(lambda ()
;;                                 (set 'semantic-fetch-tags-limit semantic-fetch-tags-limit-default)))
;; (defun semantic-fetch-tags-advice (orig-fun &rest args)
;;   "Only advice `semantic-fetch-tags' ORIG-FUN.  ARGS have to be nil."
;;   (if (and
;;        (semantic-parse-tree-needs-rebuild-p)
;;        (not noninteractive))
;;       (progn
;;         (cl-decf semantic-fetch-tags-limit)
;;         (if (< semantic-fetch-tags-limit 0)
;;             (let ((number (read-number "Limit reached. New limit: "
;;                                        semantic-fetch-tags-limit-default)))
;;               (if (<= number 0)
;;                   (progn
;;                     (set 'semantic-fetch-tags-limit semantic-fetch-tags-limit-default)
;;                     (error "Parsing limit reached"))
;;                 (set 'semantic-fetch-tags-limit number))))))
;;   (apply orig-fun args))
;; <xor>
(defun semantic-fetch-tags-advice (orig-fun &rest args)
  "Only advice `semantic-fetch-tags' ORIG-FUN.  ARGS have to be nil."
  (if (and
       (semantic-parse-tree-needs-rebuild-p)
       (not noninteractive))
      (let ((event (read-event nil nil 0.001)))
        (when event
          (push event unread-command-events)
          (error "Parsing while typing"))))
  (apply orig-fun args))
(advice-add 'semantic-fetch-tags :around #'semantic-fetch-tags-advice)
;; ]


(provide 'semantic-bug)