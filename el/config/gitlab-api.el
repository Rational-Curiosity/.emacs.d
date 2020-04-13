;;; gitlab-api.el --- Gitlab interface

;;; Commentary:

;; (let ((urls
;;        (mapcar
;;         (lambda (name)
;;           (concat "/devel/" (replace-regexp-in-string "-" "_" name nil 'literal) "/merge_requests"))
;;         (append
;;          (directory-files "~/Prog/gigas" nil "^gigas")
;;          (directory-files "~/Prog/gigas/libs" nil "-lib"))))
;;       (domain "https://gitlab.gigas.local")
;;       (url-mime-language-string "es")
;;       (url-request-method "GET")
;;       (url-request-extra-headers '(("Private-Token" . "xxxxx")))
;;       (result ""))
;;   (dolist (url urls)
;;     (with-current-buffer
;;         (url-retrieve-synchronously (concat domain url))
;;       (goto-char (point-min))
;;       (while (re-search-forward (concat "<a href=\"\\(" url "/[0-9]+\\)\">\\(.*?*\\)</a>") nil t)
;;         (save-excursion
;;           (setq result (concat result "\n* " (match-string 2) "\n  " domain (match-string 1) "\n"))
;;           (if (re-search-forward "<time[^>]*>\\([^<]*\\)</time>" nil t)
;;               (setq result (concat result "  /" (match-string 1) "/")))
;;           (if (re-search-forward "<span class=\"author\">\\([^<]*\\)</span>" nil t)
;;               (setq result (concat result "  " (match-string 1))))))))
;;   (decode-coding-string (substring result 1) 'utf-8))

;;; Code:
(require 'request)
(require 'map)
(require 'json)

(require 'config-lib)

(defcustom gitlab-api-token ""
  "Private-Token")

(defcustom gitlab-api-url-base ""
  "Url base, example https://gitlab.domain.com/api/v4")

(defcustom gitlab-api-per-page 100
  "Pagination")

(defcustom gitlab-api-debug nil
  "Debug")

(defun gitlab-api-toggle-debug ()
  (interactive)
  (setq gitlab-api-debug (not gitlab-api-debug)))

(defvar gitlab-api-projects-alist nil)

(defun gitlab-api-request (resource &optional method params)
  (request (concat gitlab-api-url-base resource)
           :type (or method "GET")
           :encoding 'utf-8
           :headers (list (cons "Private-Token" gitlab-api-token))
           :params params
           :parser (lambda ()
                     (decode-coding-region (point) (point-max) 'utf-8)
                     (utf8-fix-wrong-latin (point) (point-max))
                     ;;(save-excursion (ascii-to-utf8-forward))
                     (json-read))
           :sync t))

(defun gitlab-api-data (resource &optional method params)
  (request-response-data
   (gitlab-api-request resource method params)))

(defun gitlab-api-data-all-pages (resource &optional method params)
  (map-put params "per_page" gitlab-api-per-page 'string-equal)
  (let ((page 1) data-page data)
    (map-put params "page" (int-to-string page) 'string-equal)
    (setq data-page (gitlab-api-data resource method params)
          data data-page)
    (while (not (seq-empty-p data-page))
      (setq page (1+ page))
      (map-put params "page" (int-to-string page) 'string-equal)
      (setq data-page (gitlab-api-data resource method params)
            data (vconcat data data-page)))
    (if gitlab-api-debug (let ((item-count 0))
                           (mapc (lambda (item)
                                   (message "  Item %i" (cl-incf item-count))
                                   (mapc (lambda (prop)
                                           (message "    %s" prop)) item))
                                 data)))
    (message "%s %s %s: %i items"
             method resource params (length data))
    data))

(defun gitlab-api-template (resource &optional alist method params)
  (setq resource (if alist
                     (gitlab-api--fill-template resource alist)
                   resource))
  (let ((data (gitlab-api-data resource method params)))
    (if gitlab-api-debug (mapc (lambda (prop) (message "    %s" prop)) data))
    (message "%s %s %s: %i properties" method resource params (length data))
    data))

(defun gitlab-api-template-all-pages (resource &optional alist method params)
  (gitlab-api-data-all-pages (if alist
                                 (gitlab-api--fill-template resource alist)
                               resource)
                             method params))

;;;;;;;;;;;;;;
;; Projects ;;
;;;;;;;;;;;;;;
(defun gitlab-api-get-projects ()
  (let ((projets-datas (gitlab-api-data-all-pages "/projects" "GET")))
    (setq gitlab-api-projects-alist (mapcar (lambda (project-data)
                                              (cons (cdr (assoc 'id project-data))
                                                    (cdr (assoc 'name project-data))))
                                            projets-datas))
    projets-datas))

(defun gitlab-api-get-project-name (project-id)
  (unless gitlab-api-projects-alist
    (gitlab-api-get-projects))
  (cdr (assoc project-id gitlab-api-projects-alist)))

(defun gitlab-api-get-projects-property (property &optional test)
  (mapcar (lambda (project) (assoc-default property project test)) (gitlab-api-get-projects)))

;; (defun gitlab-api-get-merge-requests-all ()
;;   (apply 'vconcat (mapcar 'gitlab-api-get-merge-requests (gitlab-api-get-projects-property 'id))))

(defun gitlab-api-project-resource (project-id resource &optional alist method params)
  (gitlab-api-data-all-pages (concat "/projects/" (int-to-string project-id)
                                     (if alist
                                         (gitlab-api--fill-template resource alist)
                                       resource))
                             method params))

(defun gitlab-api-project-resource-all (resource &optional alist method params)
  (apply 'vconcat (mapcar (lambda (project-id)
                            (gitlab-api-project-resource project-id resource alist method params))
                          (gitlab-api-get-projects-property 'id))))

;;;;;;;;;;;;;
;; Helpers ;;
;;;;;;;;;;;;;
(defun gitlab-api--fill-template (template alist)
  (mapc (lambda (key-value)
          (setq template (replace-regexp-in-string
                          (regexp-quote (concat "{" (car key-value) "}"))
                          (cdr key-value) template t 'literal)))
        alist)
  template)

;; (defun gitlab-api--assoc-keys (keys alist)
;;   "Recursively find KEYS in ALIST."
;;   (while (and (listp alist) keys)
;;     (setq alist (cdr (assoc (pop keys) alist))))
;;   (if keys nil alist))

(defun gitlab-api--convert-state (state type)
  (pcase state
    ("opened" (pcase type
                ("mer" "VERI")
                ("iss" "STAR")
                (_ (concat state "/" type))))
    ("closed" (pcase type
                ("mer" "CANC")
                ("iss" "DONE")
                (_ (concat state "/" type))))
    ("locked" "HOLD")
    ("merged" "DONE")
    (_ (concat state "/" type))))

(defun gitlab-api--abbrev-project-name (project-name)
  (pcase project-name
    ("command-lib"        "CMD")
    ("comfy-lib"          "CMF")
    ("managers-lib"       "MGR")
    ("bestof-lib"         "BOF")
    ("gigas-errors-pkg"   "ERR")
    ("gigas_hv_failover"  "HFO")
    ("gigas_kudeiro"      "KUD")
    ("gigas_apiproxy"     "AXY")
    ("API Proxy"          "AXY")
    ("gigas_api_panel"    "APN")
    ("gigas_api"          "API")
    ("gigas_hapi"         "BLY")
    ("MApp"               "MAP")
    ("gigas_mapp"         "MAP")
    ("gigas_mercury"      "MCY")
    ("gigas_executor"     "EX")
    ("Control Panel"      "CPN")
    ("Hostbill"           "HBL")
    (_ project-name)))

(defun gitlab-api--format-field (field value indent)
  (concat indent (format "%-9s  %s" (concat ":" field ":")
                         (if (stringp value)
                             (replace-regexp-in-string "\r?\n"
                                                       (concat
                                                        indent
                                                        (format "%-9s " (concat ":" field "+:")))
                                                       value t)
                           value))))

(defun gitlab-api--convert-to-org (data keys &optional level resource)
  (setq level (or level 1))
  (let ((indent (concat "\n" (make-string (1+ level) ?\ )))
        (type (replace-regexp-in-string
               "^.*/\\([a-zA-Z0-9_-]\\{3\\}\\)[a-zA-Z0-9_-]*/[^/]*$" "\\1"
               (or (assoc-default 'web_url data) resource) t))
        (project (gitlab-api-get-project-name (assoc-default 'project_id data))))
    (let ((entry (format "%s %s %-64s  :%s:%s:"
                         (make-string level ?*)
                         (gitlab-api--convert-state (assoc-default 'state data) type)
                         (assoc-default 'title data)
                         type
                         (gitlab-api--abbrev-project-name project))))
      (setq entry (concat entry indent ":PROPERTIES:"
                          (and resource (concat indent ":RESOURCE:     " resource))
                          (and project (concat indent ":PROJECT:  " project))))
      (dolist (key keys)
        (if (listp key)
            (let ((value (assoc-keys key data)))
              (if value
                  (setq entry (concat
                               entry
                               (gitlab-api--format-field (mapconcat 'symbol-name key ".") value indent)))))
          (let ((value (assoc-default key data)))
            (when value
              (setq entry (concat
                           entry
                           (redmine-api--format-field (if (eq key 'id)
                                                          "id_"
                                                        (symbol-name key))
                                                      value indent)))))))
      (replace-regexp-in-string "\n\\*" "\n.*" (concat entry indent ":END:\n\n") t 'literal))))

(defun gitlab-api-default-sort (a b)
  (let ((a-value (cdr (assoc 'state a)))
        (b-value (cdr (assoc 'state b))))
    (cond
     ((string-equal a-value "opened")
      (if (string-equal b-value "opened")
          (string> (cdr (assoc 'updated_at a))
                   (cdr (assoc 'updated_at b)))
        t))
     ((string-equal b-value "opened") nil)
     ((string-equal a-value "locked")
      (if (string-equal b-value "locked")
          (string> (cdr (assoc 'updated_at a))
                   (cdr (assoc 'updated_at b)))
        t))
     ((string-equal b-value "locked") nil)
     ((string> (cdr (assoc 'updated_at a))
               (cdr (assoc 'updated_at b)))))))

(defmacro gitlab-api-create-filter (condition &rest args)
  (list 'lambda (list 'a)
        (cons (eval condition)
              (let (conds)
                (while args
                  (setq
                   conds
                   (nconc conds
                          (list (cons (eval (pop args))
                                      (let (cond-args arg)
                                        (while (and args (not (functionp (setq arg (eval (car args))))))
                                          (setq
                                           cond-args
                                           (nconc cond-args
                                                  (list (cond
                                                         ((symbolp arg)
                                                          (list 'assoc-default (pop args) 'a))
                                                         ((listp arg)
                                                          (list 'assoc-keys (pop args) 'a))
                                                         (t (pop args)))))))
                                        cond-args))))))
                conds))))

;;;;;;;;;
;; Org ;;
;;;;;;;;;
(defun gitlab-api-org-convert (datas template &optional level sort-func &rest filter-funcs)
  (while filter-funcs
    (setq datas (cl-delete-if-not (pop filter-funcs) datas)))
  (mapconcat (lambda (data)
               (gitlab-api--convert-to-org data
                                           '(id
                                             iid
                                             project_id
                                             title
                                             description
                                             state
                                             created_at
                                             updated_at
                                             target_branch
                                             source_branch
                                             (author id)
                                             (author username)
                                             (assignee id)
                                             (assignee username)
                                             (assignees username)
                                             source_project_id
                                             target_project_id
                                             labels
                                             (milestone id)
                                             (milestone project_id)
                                             (milestone title)
                                             merge_status
                                             web_url)
                                           level
                                           template))
             (sort
              datas
              (or sort-func 'gitlab-api-default-sort))
             ""))

(defun gitlab-api-org-get-search (level params &optional sort-func &rest filter-funcs)
  (interactive "p\nXParams alist: ")
  (if (called-interactively-p 'any)
      (insert (apply 'gitlab-api-org-convert
                     (gitlab-api-data-all-pages "/search" "GET" params)
                     (concat "/projects/{project_id}/" (cdr (assoc "scope" params)) "/{iid}")
                     level sort-func filter-funcs))
    (apply 'gitlab-api-org-convert
           (gitlab-api-data-all-pages "/search" "GET" params)
           (concat "/projects/{project_id}/" (cdr (assoc "scope" params)) "/{iid}")
           level sort-func filter-funcs)))

(defun gitlab-api-org-get-issues (level &optional params sort-func &rest filter-funcs)
  (interactive "p")
  (map-put params "scope" "all" 'string-equal)
  (map-put params "state" "all" 'string-equal)
  (if (called-interactively-p 'any)
      (insert (apply 'gitlab-api-org-convert
                     (gitlab-api-data-all-pages "/issues" "GET" params)
                     "/projects/{project_id}/issues/{iid}"
                     level sort-func filter-funcs))
    (apply 'gitlab-api-org-convert
           (gitlab-api-data-all-pages "/issues" "GET" params)
           "/projects/{project_id}/issues/{iid}"
           level sort-func filter-funcs)))

(defun gitlab-api-org-get-merge-requests (level &optional params sort-func &rest filter-funcs)
  (interactive "p")
  (map-put params "scope" "all" 'string-equal)
  (map-put params "state" "all" 'string-equal)
  (if (called-interactively-p 'any)
      (insert (apply 'gitlab-api-org-convert
                     (gitlab-api-data-all-pages "/merge_requests" "GET" params)
                     "/projects/{project_id}/merge_requests/{iid}"
                     level sort-func filter-funcs))
    (apply 'gitlab-api-org-convert
           (gitlab-api-data-all-pages "/merge_requests" "GET" params)
           "/projects/{project_id}/merge_requests/{iid}"
           level sort-func filter-funcs)))

(defun gitlab-api-org-get-from-redmine-id (level &optional redmine-id property)
  (interactive (list (and current-prefix-arg (prefix-numeric-value current-prefix-arg))
                     (completing-read "Redmine issue id: "
                                      (mapcar
                                       (lambda (str-or-url) (replace-regexp-in-string "^https?://.*/" "" str-or-url))
                                       (remove
                                        nil
                                        (mapcar
                                         (lambda (key) (org-entry-get nil key))
                                         '("ORIGIN" "BUG_IN"))))))
               ;; "p\nMRedmine issue id: "
               )
  (setq level (or level (1+ (org-outline-level)))
        redmine-id (if (and (stringp redmine-id) (not (string-empty-p redmine-id)))
                       redmine-id
                     (replace-regexp-in-string "^https?://.*/" "" (org-entry-get nil (or property "ORIGIN")))))
  (let* ((issues (gitlab-api-data-all-pages
                  "/issues" "GET"
                  `(("scope" . "all")
                    ("state" . "all")
                    ("search" . ,(concat
                                  "\""
                                  redmine-id
                                  "\""))
                    ("in" . "description"))))
         (result (concat
                  (gitlab-api-org-convert issues "/projects/{project_id}/issues/{iid}" level)
                  (mapconcat (lambda (issue)
                               (let ((issue-id (concat "#" (int-to-string (cdr (assoc 'iid issue))))))
                                 (gitlab-api-org-convert
                                  (gitlab-api-data-all-pages
                                   (concat "/projects/" (int-to-string (cdr (assoc 'project_id issue))) "/merge_requests")
                                   "GET"
                                   `(("scope" . "all")
                                     ("state" . "all")
                                     ("search" . ,(concat "\"" issue-id "\"")) ("in" . "description")))
                                  "/projects/{project_id}/merge_requests/{iid}" level nil
                                  (lambda (data)
                                    (string-match-p (concat issue-id "\\([ \t\n]\\|$\\)") (cdr (assoc 'description data)))))))
                             issues
                             ""))))
    (if (called-interactively-p 'any)
        (insert result)
      result)))

(defun gitlab-api-org-update-entry-at-point ()
  (interactive)
  (let ((properties (org-entry-properties nil 'standard)))
    (let ((id-assoc (assoc "ID_" properties)))
      (if id-assoc (setcar id-assoc "ID")))
    (let ((type (replace-regexp-in-string
                 "^.*/\\([a-zA-Z0-9_-]\\{3\\}\\)[a-zA-Z0-9_-]*/[^/]*$" "\\1"
                 (or (assoc-default "web_url" properties)
                     (assoc-default "RESOURCE" properties)) t))
          (resource-datas (gitlab-api-template (cdr (assoc "RESOURCE" properties)) properties "GET"
                                               '(("scope" . "all")
                                                 ("state" . "all")))))
      (mapc (lambda (property)
              (let* ((name (downcase (car property)))
                     (value (cdr property))
                     (resource-data (if (string-match-p "\\." name)
                                        (assoc-keys (mapcar 'intern (split-string name "\\."))
                                                    resource-datas)
                                      (cdr (assoc (intern name) resource-datas)))))
                (when resource-data
                  (let ((data (replace-regexp-in-string
                               "\r" ""
                               (if (stringp resource-data)
                                   resource-data
                                 (format "%s" resource-data)) t t)))
                    (unless (string-equal value (replace-regexp-in-string "\n" "" data t t))
                      (org-entry-put-multiline-property nil name data)
                      (pcase name
                        ("state" (org-entry-put nil "TODO" (gitlab-api--convert-state data type)))
                        ("project_id" (org-entry-put nil "PROJECT" (gitlab-api-get-project-name resource-data)))))))))
            properties))))


(provide 'gitlab-api)
;;; gitlab-api.el ends here
