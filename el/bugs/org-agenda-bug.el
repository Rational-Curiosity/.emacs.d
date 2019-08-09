(with-eval-after-load 'org-agenda
  (when (bug-check-function-bytecode
         'org-compile-prefix-format
         "xokQEcaJEhPGFA07gxMADYIjAA4qDZ6DIgAOKg2eQYIjAMfIxomJiYmJiR4rHiweLR4uHi8eMB4xHjIeM8nKDjMOMiODNwHLzM0OMyLOIkGGVADPFi/M0A4zIoZfANEWLdKUFivIlFQWMg4v07eCggDUEIKCANQRgoIA1BOCggDUFNXM1g4zItdRFiwOL9g9g8UA2drbzNYOMyIhISESzNYOMyIeNNwONMbUHjXJAwMDIym2g4XCANvMyA40ItLGTyEpFjYOL889g94A3Q4s3t/MzQ4zIiFERRYxgiQBDiuDAQHg4eLRDi9F4sYOL0VF0d0OLOMOLw4tRUVGFjGCJAHdDizg4eIOL+RCQuIOL+VCQkXR4w4vDi3myOcOL0ZGRkUWMejp1MYOMyQWMw4xDjBCFjCCPQAOMJ8WMHION4ZDAXBxiMAIRMEJRMIKRMMLRMQMRK8F3Q4zDjBCQkSJFjguCoc=")
    (defun org-compile-prefix-format (key)
      "Compile the prefix format into a Lisp form that can be evaluated.
The resulting form and associated variable bindings is returned
and stored in the variable `org-prefix-format-compiled'."
      (setq org-prefix-has-time nil
            org-prefix-has-tag nil
            org-prefix-category-length nil
            org-prefix-has-effort nil
            org-prefix-has-breadcrumbs nil)
      (let ((s (cond
                ((stringp org-agenda-prefix-format)
                 org-agenda-prefix-format)
                ((assq key org-agenda-prefix-format)
                 (cdr (assq key org-agenda-prefix-format)))
                (t "  %-12:c%?-12t% s")))
            (start 0)
            varform vars var e c f opt)
        (while (string-match "%\\(\\?\\)?\\([-+]?[0-9.]*\\)\\([ .;,:!?=|/<>]?\\)\\([cltseib]\\|(.+)\\)"
                             s start)
          (setq var (or (cdr (assoc (match-string 4 s)
                                    '(("c" . category) ("t" . time) ("l" . level) ("s" . extra)
                                      ("i" . category-icon) ("T" . tag) ("e" . effort) ("b" . breadcrumbs))))
                        'eval)
                c (or (match-string 3 s) "")
                opt (match-beginning 1)
                start (1+ (match-beginning 0)))
          (cl-case var
            (time        (setq org-prefix-has-time        t))
            (tag         (setq org-prefix-has-tag         t))
            (effort      (setq org-prefix-has-effort      t))
            (breadcrumbs (setq org-prefix-has-breadcrumbs t)))
          (setq f (concat "%" (match-string 2 s) "s"))
          (when (eq var 'category)
            (setq org-prefix-category-length
                  (floor (abs (string-to-number (match-string 2 s)))))
            (setq org-prefix-category-max-length
                  (let ((x (match-string 2 s)))
                    (save-match-data  ;; +
                      (when (string-match "\\.[0-9]+" x)
                        (string-to-number (substring (match-string 0 x) 1)))))))  ;; +
;;                     (when (string-match-p "\\.[0-9]+" x)  ;; -
;;                       (string-to-number (substring (match-string 0 x) 1))))))  ;; -
          (if (eq var 'eval)
              (setq varform `(format ,f (org-eval ,(read (match-string 4 s)))))
            (if opt
                (setq varform
                      `(if (or (equal "" ,var) (equal nil ,var))
                           ""
                         (format ,f (concat ,var ,c))))
              (setq varform
                    `(format ,f (if (or (equal ,var "")
                                        (equal ,var nil)) ""
                                  (concat ,var ,c (get-text-property 0 'extra-space ,var)))))))
          (setq s (replace-match "%s" t nil s))
          (push varform vars))
        (setq vars (nreverse vars))
        (with-current-buffer (or org-agenda-buffer (current-buffer))
          (setq org-prefix-format-compiled
                (list
                 `((org-prefix-has-time ,org-prefix-has-time)
                   (org-prefix-has-tag ,org-prefix-has-tag)
                   (org-prefix-category-length ,org-prefix-category-length)
                   (org-prefix-has-effort ,org-prefix-has-effort)
                   (org-prefix-has-breadcrumbs ,org-prefix-has-breadcrumbs))
                 `(format ,s ,@vars))))))))


(provide 'ox-odt-bug)
