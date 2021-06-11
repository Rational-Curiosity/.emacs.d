(require 'redmine-api)
(require 'gitlab-api)

;;;###autoload
(defun redmine-api-org-version-status ()
  (interactive)
  (org-set-multiline-property
   "ISSUES_STATUS"
   (mapconcat
    (lambda (alist)
      (let ((data (assoc-default
                   'issue
                   (request-response-data
                    (request (concat redmine-api-url-base "/issues/"
                                     (int-to-string (assoc-default 'id alist))
                                     ".json")
                      :type "GET"
                      :encoding 'utf-8
                      :headers (list (cons "X-Redmine-API-Key" redmine-api-token))
                      :parser 'json-read
                      :sync t)))))
        (concat
         redmine-api-url-base "/issues/"
         (int-to-string (assoc-default 'id data))
         "  "
         (assoc-default 'name (assoc-default 'status data)))))
    (redmine-api-issues-from-version
     (replace-regexp-in-string
      "^https?://.*/" ""
      (org-entry-get nil "ORIGIN")))
    "\n")))

;;;###autoload
(defun gitlab-api-org-get-from-redmine-id (level &optional redmine-id property)
  (interactive
   (list (and current-prefix-arg
              (prefix-numeric-value current-prefix-arg))
         (completing-read
          "Redmine issue id: "
          (mapcar
           (lambda (str-or-url)
             (replace-regexp-in-string "^https?://.*/" "" str-or-url))
           (remove
            nil
            (mapcar
             (lambda (key) (org-entry-get nil key))
             '("ORIGIN" "BUG_IN")))))))
  (or level (setq level (1+ (org-outline-level))))
  (setq redmine-id (if (and
                        (stringp redmine-id)
                        (not (string-empty-p redmine-id)))
                       redmine-id
                     (replace-regexp-in-string
                      "^https?://.*/" ""
                      (org-entry-get nil (or property "ORIGIN")))))
  (let* ((issues (gitlab-api-data-all-pages
                  "/issues" "GET"
                  `(("scope" . "all")
                    ("state" . "all")
                    ("search" . ,(concat
                                  "\""
                                  redmine-id
                                  "\""))
                    ("in" . "description"))))
         (level-mr (1+ level))
         (result (mapconcat
                  (lambda (issue)
                    (concat
                     (gitlab-api-org-convert
                      (list issue) "/projects/{project_id}/issues/{iid}" level)
                     (let ((issue-id (concat
                                      "#"
                                      (int-to-string (cdr (assoc 'iid issue))))))
                       (gitlab-api-org-convert
                        (gitlab-api-data-all-pages
                         (concat
                          "/projects/"
                          (int-to-string (cdr (assoc 'project_id issue)))
                          "/merge_requests")
                         "GET"
                         `(("scope" . "all")
                           ("state" . "all")
                           ("search" . ,(concat "\"" issue-id "\""))
                           ("in" . "description")))
                        "/projects/{project_id}/merge_requests/{iid}" level-mr nil
                        (lambda (data)
                          (string-match-p
                           (concat
                            issue-id "\\([ \t\n]\\|$\\)")
                           (cdr (assoc 'description data))))))))
                  issues
                  "")))
    (if (called-interactively-p 'any)
        (insert result)
      result)))

;;;###autoload
(defun gitlab-api-org-get-from-version-id (level &optional version-id property)
  (interactive
   (list (and current-prefix-arg
              (prefix-numeric-value current-prefix-arg))
         (completing-read
          "Redmine version id: "
          (mapcar
           (lambda (str-or-url)
             (replace-regexp-in-string "^https?://.*/" "" str-or-url))
           (remove
            nil
            (mapcar
             (lambda (key) (org-entry-get nil key))
             '("ORIGIN" "BUG_IN")))))))
  (or level (setq level (1+ (org-outline-level))))
  (setq version-id (if (and
                        (stringp version-id)
                        (not (string-empty-p version-id)))
                       version-id
                     (replace-regexp-in-string
                      "^https?://.*/" ""
                      (org-entry-get nil (or property "ORIGIN")))))
  (let* ((level-gitlab (1+ level))
         (result (mapconcat
                  (lambda (issue)
                    (concat
                     (redmine-api-org-convert (list issue) "/issues/{id}" level)
                     (gitlab-api-org-get-from-redmine-id
                      level-gitlab
                      (int-to-string (assoc-default 'id issue))
                      property)))
                  (redmine-api-issues-from-version version-id)
                  "")))
    (if (called-interactively-p 'any)
        (insert result)
      result)))


(provide 'redmine-gitlab-api)