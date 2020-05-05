(with-eval-after-load 'virtualenvwrapper
  (when (bug-check-function-bytecode
         'venv-mkvirtualenv-using
         "xiCICIMNAMfIIYIOAAkaCzuDHADJygshIYIdAAwdCoUlAMsKUB4gDiGDMQAOIYI1AMzNIUOJHiHOHiKJHiODtQAOI0AeJM/QIA4kIoNTANHSIYjT1CGI1Q4l1g4g1g0OJLAGIYgLPIOBAA0OJFCJHiYLnYN7AAuIgoAADiYLQhMpDA4nHigeKdcOJCGI2A4qIYjZjtPaIYgr29whg6YA3d4OJFAhiCkOIlQWIg4jQYkWI4RBACrX3w4hIUAhLIc=")
    (defun venv-mkvirtualenv-using (interpreter &rest names)
      "Create new virtualenvs NAMES using INTERPRETER. If venv-location
is a single directory, the new virtualenvs are made there; if it
is a list of directories, the new virtualenvs are made in the
current `default-directory'."
      (interactive '(nil))
      (venv--check-executable)
      (let* ((interpreter (if (or current-prefix-arg
                                  (null interpreter))
                              (read-string "Python executable: ")
                            interpreter))
             (parent-dir (if (stringp venv-location)
                             (file-name-as-directory
                              (expand-file-name venv-location))
                           default-directory))
             (python-exe-arg (when interpreter
                               (concat "--python=" interpreter)))
             (names (if names names
                      (list (read-from-minibuffer "New virtualenv: ")))))
        ;; map over all the envs we want to make
        (--each names
          ;; error if this env already exists
          (when (-contains? (venv-get-candidates) it)
            (error "A virtualenv with this name already exists!"))
          (run-hooks 'venv-premkvirtualenv-hook)
          (shell-command (concat venv-virtualenv-command " " python-exe-arg " " parent-dir it))
          (when (listp venv-location)
            (add-to-list 'venv-location (concat parent-dir it)))
          (venv-with-virtualenv it
                                (run-hooks 'venv-postmkvirtualenv-hook))
          (when (called-interactively-p 'interactive)
            (message (concat "Created virtualenv: " it))))
        ;; workon the last venv we made
        (venv-workon (car (last names)))))))
