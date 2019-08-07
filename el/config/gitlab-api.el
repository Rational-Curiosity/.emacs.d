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

(defcustom gitlab-api-token ""
  "Private-Token")

(defcustom gitlab-api-url-base ""
  "Url base, example https://gitlab.domain.com/api/v4")

(defvar gitlab-api-projects-alist nil)

(defun gitlab-api-request (url &optional method params)
  (request url
           :type (or method "GET")
           :encoding 'utf-8
           :headers (list (cons "Private-Token" gitlab-api-token))
           :params params
           :parser (lambda ()
                     (decode-coding-region (point) (point-max) 'utf-8)
                     ;;(save-excursion (ascii-to-utf8-forward))
                     (json-read))
           :sync t))

(defun gitlab-api-data (url &optional method params)
  (request-response-data
   (gitlab-api-request url method params)))

(defun gitlab-api--fill-template (template alist)
  (mapc (lambda (key-value)
          (setq template (replace-regexp-in-string
                          (concat "{" (car key-value) "}")
                          (cdr key-value) template t 'literal)))
        alist)
  template)

(defun gitlab-api-resource (resource &optional alist method params)
  (gitlab-api-data (concat gitlab-api-url-base
                           (if alist
                               (gitlab-api--fill-template resource alist)
                             resource))
                   method params))

;;;;;;;;;;;;;;
;; Projects ;;
;;;;;;;;;;;;;;
(defun gitlab-api-get-projects ()
  (let ((projets-datas (gitlab-api-data (concat gitlab-api-url-base "/projects") "GET")))
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

(defun gitlab-api-project-resource (project-id resource &optional alist method)
  (gitlab-api-data (concat gitlab-api-url-base "/projects/" (int-to-string project-id)
                           (if alist
                               (gitlab-api--fill-template resource alist)
                             resource))
                   method))

(defun gitlab-api-project-resource-all (resource &optional alist method)
  (apply 'vconcat (mapcar (lambda (project-id)
                            (gitlab-api-project-resource project-id resource alist method))
                          (gitlab-api-get-projects-property 'id))))

;;;;;;;;;;;;;
;; Helpers ;;
;;;;;;;;;;;;;
(defun gitlab-api--assoc-keys (keys alist)
  "Recursively find KEYS in ALIST."
  (while (and (listp alist) keys)
    (setq alist (cdr (assoc (pop keys) alist))))
  (if keys nil alist))

(defun gitlab-api--convert-state (state)
  (pcase state
    ("opened" "VERI")
    ("closed" "CANC")
    ("locked" "HOLD")
    ("merged" "DONE")
    (t state)))

(defun gitlab-api--convert-to-org (data keys &optional level resource)
  (setq level (or level 1))
  (let ((indent (concat "\n" (make-string (1+ level) ?\ )))
        (entry (format "%s %s %s"
                       (make-string level ?*)
                       (gitlab-api--convert-state (assoc-default 'state data))
                       (assoc-default 'title data))))
    (setq entry (concat entry indent ":PROPERTIES:"
                        (and resource (concat indent ":RESOURCE:     " resource))))
    (dolist (key keys)
      (if (listp key)
          (let ((value (gitlab-api--assoc-keys key data)))
            (if value
                (setq entry (concat
                             entry
                             indent
                             (format "%-10s %s"
                                     (concat ":" (mapconcat 'symbol-name key ".") ":")
                                     value)))))
        (let ((value (assoc-default key data)))
          (when value
            (setq entry (concat
                         entry
                         indent
                         (format "%-10s %s"
                                 (concat ":" (symbol-name key) ":")
                                 value)))
            (if (eq key 'project_id)
                (setq entry (concat
                             entry
                             indent
                             (concat ":PROJECT:  "
                                     (gitlab-api-get-project-name value)))))))))
    (setq entry (concat entry indent ":END:\n\n"))))

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
                                                          (list 'gitlab-api--assoc-keys (pop args) 'a))
                                                         (t (pop args)))))))
                                        cond-args))))))
                conds))))

(defun gitlab-api-org-get-merge-requests (&optional level sort-func &rest filter-funcs)
  (interactive "P")
  (let ((merge-requests (gitlab-api-project-resource-all "/merge_requests" nil "GET")))
    (while filter-funcs
      (setq merge-requests (cl-delete-if-not (pop filter-funcs) merge-requests)))
    (let ((text (mapconcat (lambda (data)
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
                                                           source_project_id
                                                           target_project_id
                                                           labels
                                                           (milestone id)
                                                           (milestone project_id)
                                                           (milestone title)
                                                           merge_status
                                                           web_url)
                                                         level
                                                         "/projects/{project_id}/merge_requests/{iid}"))
                           (sort
                            merge-requests
                            (or sort-func 'gitlab-api-default-sort))
                           "")))
      (if (called-interactively-p 'any)
          (insert text)
        text))))

(defun gitlab-api-org-get-issues (&optional level sort-func &rest filter-funcs)
  (interactive "P")
  (let ((issues (gitlab-api-project-resource-all "/issues" nil "GET")))
    (while filter-funcs
      (setq issues (cl-delete-if-not (pop filter-funcs) issues)))
    (let ((text (mapconcat (lambda (data)
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
                                                           source_project_id
                                                           target_project_id
                                                           labels
                                                           (milestone id)
                                                           (milestone project_id)
                                                           (milestone title)
                                                           merge_status
                                                           web_url)
                                                         level
                                                         "/projects/{project_id}/issues/{iid}"))
                           (sort
                            issues
                            (or sort-func 'gitlab-api-default-sort))
                           "")))
      (if (called-interactively-p 'any)
          (insert text)
        text))))

(defun gitlab-api-org-update-entry-at-point ()
  (interactive)
  (let* ((properties (org-entry-properties nil 'standard))
         (resource-datas (gitlab-api-resource (cdr (assoc "RESOURCE" properties)) properties "GET")))
    (mapc (lambda (property)
            (let* ((name (downcase (car property)))
                   (value (cdr property))
                   (resource-data (if (string-match-p "\\." name)
                                      (gitlab-api--assoc-keys (mapcar 'intern (split-string name "\\."))
                                                              resource-datas)
                                    (cdr (assoc (intern name) resource-datas)))))
              (when resource-data
                (let ((data (if (stringp resource-data) resource-data (format "%s" resource-data))))
                  (unless (string-equal value data)
                    (org-entry-put nil name data)
                    (pcase name
                      ("state" (org-entry-put nil "TODO" (gitlab-api--convert-state data)))
                      ("project_id" (org-entry-put nil "PROJECT" (gitlab-api-get-project-name resource-data)))))))))
          properties)))


(provide 'gitlab-api)
;;; gitlab-api.el ends here
