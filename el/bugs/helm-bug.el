(when (bug-check-function-bytecode
       'helm-goto-char
       "wsMhiIliiAjEPYMRAMWCHADGwSGFHAAJhRwAx8gxKgCJhSYAiSAwgiwAiMmH")
  (defun helm-goto-char (loc)
    "Go to char, revealing if necessary."
    (goto-char loc)
    (let ((fn (cond ((eq major-mode 'org-mode) #'org-reveal)
                    ((and (boundp 'outline-minor-mode)
                          outline-minor-mode)
                     (lambda () (outline-flag-subtree nil))))))
      ;; outline may fail in some conditions e.g. with markdown enabled
      ;; (issue #1919).
      (condition-case nil
          (and fn (funcall fn))
        (error nil)))))


(provide 'helm-bug)
