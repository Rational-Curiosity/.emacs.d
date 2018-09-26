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
(defun et/semantic-remove-hooks ()
    (remove-hook 'completion-at-point-functions
                 'semantic-analyze-completion-at-point-function)
    (remove-hook 'completion-at-point-functions
                 'semantic-analyze-notc-completion-at-point-function)
    (remove-hook 'completion-at-point-functions
                 'semantic-analyze-nolongprefix-completion-at-point-function))
(add-hook 'semantic-mode-hook #'et/semantic-remove-hooks)

(when (bug-check-function-bytecode
       'semantic-fetch-tags
       "CIOwAMbHIYOwAMbIIYOwAAnJPYSwAAmDsAAKyl0ay4kbHMwgiAnNt4KvAM4gFAnPPYM6ANAgiIJCANEgiNLTDCKIyxWCrwAOJINUANRlZCIUgpQAZA4lWYWCAA4m1T2FggDW1yAeJ9gOJ4VyANnaDiciDiiFfADZ2w4oItxSKd3eIx4p1GVkIhQOKYOTAN8OKSGIKcuJiR4qHiseLOAgiCsMHi3h4g4tIogp4wwhiCsOLoc=")
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
  (advice-add 'semantic-fetch-tags :around #'semantic-fetch-tags-advice))
;; ]


(provide 'semantic-bug)
