(with-eval-after-load 'ellocate
  (when (bug-check-function-bytecode
         'ellocate
         "CIYFAAkZxcYKIkFAiYMgAMcLIcjJAgQGBiMhsgGCOwDFygwiiYMvAMsBIYiCOADMzSGDOADNIIiIziAphw==")
    (defun ellocate-db ()
      (let* ((config-dir (completing-read
                          "Select folder: "
                          (mapcar 'car ellocate-scan-dirs)
                          nil t))
             (dir (expand-file-name config-dir))
             (search (nth 1 (cl-find-if (lambda (list)
                                          (file-in-directory-p
                                           dir (nth 0 list)))
                                        ellocate-scan-cache))))
        (if search
            (find-file
             (ellocate-completing-read
              dir
              search
              t))
          (ellocate-cache-dir (assoc config-dir ellocate-scan-dirs))
          (find-file
           (ellocate-completing-read
            dir
            (car (cdr (assoc dir ellocate-scan-cache)))
            t)))))

    (defun ellocate (&optional ignore-scope)
      "Displays any files below the current dir.
If IGNORE-SCOPE is non-nil, search the entire database instead of just every
file under the current directory."
      (interactive "P")
      (let ((gc-cons-threshold (or ellocate-gc-mem gc-cons-threshold)))
        (if (equal ignore-scope '(16))
            (ellocate-db)
          (let ((search
                 ;; Load data from cached search corresponding to this default-directory
                 (nth 1 (cl-find-if (lambda (list)
                                      (file-in-directory-p
                                       default-directory (nth 0 list)))
                                    ellocate-scan-cache)))
                (dir (expand-file-name default-directory)))
            (if search
                (find-file
                 (ellocate-completing-read dir search ignore-scope))
              (let ((found-dir (cl-find-if
                                (lambda (list)
                                  (file-in-directory-p dir (nth 0 list)))
                                ellocate-scan-dirs)))
                (if found-dir
                    (progn
                      (ellocate-cache-dir found-dir)
                      (find-file
                       (ellocate-completing-read
                        dir
                        ;; re-search
                        (nth 1 (cl-find-if (lambda (list)
                                             (file-in-directory-p
                                              default-directory (nth 0 list)))
                                           ellocate-scan-cache))
                        ignore-scope)))
                  (if (fboundp 'counsel-file-jump)
                      (counsel-file-jump)
                    (ellocate-db)))))))))))
