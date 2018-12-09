(when (bug-check-function-bytecode
       'elpy-eldoc-documentation
       "wSCJgwoAiYIPAMLDIYgIhw==")
  (defun elpy-eldoc-documentation ()
    "Return some interesting information for the code at point.
This will return flymake errors for the line at point if there
are any. If not, this will do an asynchronous call to the RPC
backend to get a call tip, and display that using
`eldoc-message'. If the backend has no call tip, this will
display the current class and method instead."
    (let ((flymake-error (elpy-flymake-error-at-point)))
      (if flymake-error
          flymake-error
        (elpy-rpc-get-calltip
         (lambda (calltip)
           (eldoc-message
            (cond
             ((not calltip)
              (when elpy-eldoc-show-current-function
                (let ((current-defun (python-info-current-defun)))
                  (when current-defun
                    (format "In: %s()" current-defun)))))
             ((stringp calltip)
              calltip)
             (t
              (let ((name (cdr (assq 'name calltip)))
                    (index (cdr (assq 'index calltip)))
                    ;; Strip 'param' added by jedi at the beggining of
                    ;; parameter names. Should be unecessary for jedi > 0.13.0
                    (params (mapcar (lambda (param)
                                      (car (split-string param "^param " t)))
                                    (cdr (assq 'params calltip)))))
                (when (and index (nth index params))
                  (setf (nth index params)
                        (propertize (nth index params)
                                    'face
                                    'eldoc-highlight-function-argument)))
                (let ((prefix (propertize name 'face
                                          'font-lock-function-name-face))
                      (args (format "(%s)" (mapconcat #'identity params ", "))))
                  ;; for emacs < 25, eldoc function do not accept string
                  (if (version<= emacs-version "25")
                      (format "%s%s" prefix args)
                    (eldoc-docstring-format-sym-doc prefix args nil)))))))))
        ;; Return the last message until we're done
        eldoc-last-message))))


(provide 'elpy-bug)