(with-eval-after-load 'org-brain
  (when (bug-check-function-bytecode
         'org-brain-choose-entries
         "CIQHAMIgiAYGwz2DEwDEIIIYAMXGBggixwYJAgYJBgkGCQYJBgkGCSYIxcjJysvMBgchzSLOzyUJg0MA0AMJIoJFAAJDIoc=")
    (defun org-brain-choose-entries (prompt entries &optional predicate require-match initial-input hist def inherit-input-method)
      "PROMPT for one or more ENTRIES, separated by `org-brain-entry-separator'.
ENTRIES can be a list, or 'all which lists all headline and file entries.
Return the prompted entries in a list.
Very similar to `org-brain-choose-entry', but can return several entries.

For PREDICATE, REQUIRE-MATCH, INITIAL-INPUT, HIST, DEF and INHERIT-INPUT-METOD see `completing-read'."
      (unless org-id-locations (org-id-locations-load))
      (let* ((targets (if (eq entries 'all)
                          (org-brain--all-targets)
                        (mapcar (lambda (x)
                                  (cons (org-brain-entry-name x)
                                        (if (org-brain-filep x)
                                            x
                                          (nth 2 x))))
                                entries)))
             (choices (completing-read prompt targets
                                       predicate require-match initial-input hist def inherit-input-method)))
        (mapcar (lambda (title)
                  (let ((id (or (cdr (assoc title targets))
                                title)))
                    (or
                     ;; Headline entry exists, return it
                     (org-brain-entry-from-id id)
                     ;; File entry
                     (progn
                       (setq id (split-string id "::" t))
                       (let* ((entry-path (org-brain-entry-path (car id) t))
                              (entry-file (org-brain-path-entry-name entry-path)))
                         (unless (file-exists-p entry-path)
                           (if (and org-brain-default-file-parent (equal (length id) 1))
                               (setq entry-file org-brain-default-file-parent
                                     id `(,org-brain-default-file-parent ,(car id)))
                             (make-directory (file-name-directory entry-path) t)
                             (write-region "" nil entry-path)))
                         (if (or (not org-brain-include-file-entries)
                                 (equal (length id) 2)
                                 (not (equal (car id) entry-file)))
                             ;; Create new headline entry in file
                             (org-with-point-at (org-brain-entry-marker entry-file)
                               (if (and (not org-brain-include-file-entries)
                                        (or
                                         ;; Search heading without tags
                                         (save-excursion
                                           (re-search-forward (concat "\n\\* +" (car id) "[ \t]*$") nil t))
                                         ;; Search heading with tags
                                         (save-excursion
                                           (re-search-forward (concat "\n\\* +" (car id) "[ \t]+:.*:$") nil t))))
                                   (org-brain-entry-at-pt)
                                 (goto-char (point-max))
                                 (insert (concat "\n* " (or (cadr id) (car id))))
                                 (let ((new-id (org-id-get-create)))
                                   (run-hooks 'org-brain-new-entry-hook)
                                   (save-buffer)
                                   (list entry-file (or (cadr id) (car id)) new-id))))
                           entry-file))))))
                (if org-brain-entry-separator
                    (split-string choices org-brain-entry-separator)
                  (list choices)))))))