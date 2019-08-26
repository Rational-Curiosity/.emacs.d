;;; redmine-api.el --- Redmine interface

;;; Commentary:

;;; Code:
(require 'request)
(require 'map)
(require 'json)

(require 'config-lib)

(defcustom redmine-api-token ""
  "Private-Token")

(defcustom redmine-api-url-base ""
  "Url base, example https://redmine.domain.com/api/v4")

(defcustom redmine-api-per-page 100
  "Pagination")

(defcustom redmine-api-debug nil
  "Debug")

(defun redmine-api-toggle-debug ()
  (interactive)
  (setq redmine-api-debug (not redmine-api-debug)))

(defun redmine-api-request (resource &optional method params)
  (request (concat redmine-api-url-base resource ".json")
           :type (or method "GET")
           :encoding 'utf-8
           :headers (list (cons "X-Redmine-API-Key" redmine-api-token))
           :params params
           :parser (lambda ()
                     (decode-coding-region (point) (point-max) 'utf-8)
                     ;;(save-excursion (ascii-to-utf8-forward))
                     (json-read))
           :sync t))

(defun redmine-api-data (resource &optional method params)
  (request-response-data
   (redmine-api-request resource method params)))

(defun redmine-api-template (resource &optional alist method params)
  (setq resource (if alist
                     (redmine-api--fill-template resource alist)
                   resource))
  (redmine-api-data resource method params))

;;;;;;;;;;;;;
;; Helpers ;;
;;;;;;;;;;;;;
(defun redmine-api--fill-template (template alist)
  (mapc (lambda (key-value)
          (setq template (replace-regexp-in-string
                          (concat "{" (car key-value) "}")
                          (cdr key-value) template t 'literal)))
        alist)
  template)

(defun redmine-api-default-sort (a b)
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

(defun redmine-api--convert-state (state type)
  (pcase state
    ("New"         "TODO")
    ("In progress" "STAR")
    ("On hold"     "HOLD")
    ("QA Failed"   "REOP")
    ("QA Blocked"  "LINK")
    ("QA Queue"    "DONE")
    ("QA Passed"   "FINI")
    ("Closed"      "CANC")
    (_ (concat state "/" type))))

(defun redmine-api--abbrev-project-name (project-name)
  (pcase project-name
    ("Command"             "Cmd")
    ("Comfy"               "CMF")
    ("Managers"            "Mgr")
    ("BestOf"              "Bof")
    ("Programación"        "Prg")
    ("API Proxy"           "Axy")
    ("Billy (Billing API)" "Axy")
    ("Panel de control"    "Cpn")
    ("Front"               "Fnt")
    ("GO"                  "Go")
    ("API Provisión"       "Apv")
    ("GO panel"            "Gpn")
    ("Kudeiro"             "Kud")
    (_ project-name)))

(defun redmine-api--format-field (field value indent)
  (concat indent (format "%-9s  %s" (concat ":" field ":")
                         (if (stringp value)
                             (replace-regexp-in-string "\r?\n"
                                                       (concat
                                                        indent
                                                        (format "%-9s " (concat ":" field "+:")))
                                                       value t)
                           value))))

(defun redmine-api--convert-to-org (data keys &optional level resource)
  (setq level (or level 1))
  (let ((indent (concat "\n" (make-string (1+ level) ?\ )))
        (type (replace-regexp-in-string
               "^.*/\\([a-zA-Z0-9_-]\\{3\\}\\)[a-zA-Z0-9_-]*/[^/]*$" "\\1"
               (or (assoc-default 'web_url data) resource) t))
        (project (assoc-keys '(project name) data)))
    (let ((entry (format "%s %s %-64s  :%s:%s:"
                         (make-string level ?*)
                         (redmine-api--convert-state (assoc-keys '(status name) data) type)
                         (assoc-default 'subject data)
                         type
                         (redmine-api--abbrev-project-name project))))
      (setq entry (concat entry indent ":PROPERTIES:"
                          (and resource (concat indent ":RESOURCE:     " resource))
                          (and resource (concat indent ":ORIGIN:       "
                                                (concat
                                                 redmine-api-url-base
                                                 (replace-regexp-in-string
                                                  "{id}"
                                                  (int-to-string (cdr (assoc 'id data)))
                                                  resource t))))))
      (dolist (key keys)
        (if (listp key)
            (let ((value (assoc-keys key data)))
              (if value
                  (setq entry (concat
                               entry
                               (redmine-api--format-field (mapconcat 'symbol-name key ".") value indent)))))
          (let ((value (assoc-default key data)))
            (when value
              (setq entry (concat
                           entry
                           (redmine-api--format-field (symbol-name key) value indent)))))))
      (concat entry indent ":END:\n\n"))))

;;;;;;;;;
;; Org ;;
;;;;;;;;;
(defun redmine-api-org-convert (datas template &optional level sort-func &rest filter-funcs)
  (while filter-funcs
    (setq datas (cl-delete-if-not (pop filter-funcs) datas)))
  (mapconcat (lambda (data)
               (redmine-api--convert-to-org data
                                            '(id
                                              (project id)
                                              (project name)
                                              subject
                                              description
                                              (status name)
                                              created_on
                                              updated_on
                                              start_date
                                              done_ratio
                                              spent_hours
                                              (fixed_version name)
                                              (author id)
                                              (author name)
                                              (assigned_to id)
                                              (assigned_to name)
                                              (priority id)
                                              (priority name)
                                              (tracker name))
                                           level
                                           template))
             (sort
              datas
              (or sort-func 'redmine-api-default-sort))
             ""))

(defun redmine-api-org-get-issue (level issue-id)
  (interactive (list (and current-prefix-arg (prefix-numeric-value current-prefix-arg))
                     (read-string "Issue Id: ")))
  (setq level (or level (1+ (org-outline-level))))
  (let* ((resource (concat "/issues/" issue-id))
         (issue (cdr (assoc 'issue (redmine-api-data resource "GET"))))
         (result (redmine-api-org-convert (list issue) "/issues/{id}" level)))
    (if redmine-api-debug (mapc (lambda (prop) (message "    %s" prop)) issue))
    (message "%s %s %s: %i properties" "GET" resource nil (length issue))
    (if (called-interactively-p 'any)
        (insert result)
      result)))

(defun redmine-api-org-get-issues (level &optional params)
  (interactive (list (and current-prefix-arg (prefix-numeric-value current-prefix-arg))
                     (let ((expr (read-from-minibuffer "Params alist: ")))
                       (if (string-equal expr "")
                           nil
                         (eval-string expr)))))
  (setq level (or level (1+ (org-outline-level))))
  (let* ((issues (cdr (assoc 'issues (redmine-api-data "/issues" "GET" params))))
         (result (redmine-api-org-convert issues "/issues/{id}" level)))
    (if redmine-api-debug (let ((item-count 0))
                            (mapc (lambda (item)
                                    (message "  Item %i" (cl-incf item-count))
                                    (mapc (lambda (prop)
                                            (message "    %s" prop)) item))
                                  issues)))
    (message "%s %s %s: %i items" "GET" "/issues" params (length issues))
    (if (called-interactively-p 'any)
        (insert result)
      result)))

(defun redmine-api-org-get-immediates (level &optional params)
  (interactive (list (and current-prefix-arg (prefix-numeric-value current-prefix-arg))
                     (let ((expr (read-from-minibuffer "Params alist: ")))
                       (if (string-equal expr "")
                           nil
                         (eval-string expr)))))
  (setq level (or level (1+ (org-outline-level))))
  (map-put params "priority_id" "5" 'string-equal)
  (let* ((issues (cdr (assoc 'issues (redmine-api-data "/issues" "GET" params))))
         (result (redmine-api-org-convert issues "/issues/{id}" level)))
    (if redmine-api-debug (let ((item-count 0))
                            (mapc (lambda (item)
                                    (message "  Item %i" (cl-incf item-count))
                                    (mapc (lambda (prop)
                                            (message "    %s" prop)) item))
                                  issues)))
    (message "%s %s %s: %i items" "GET" "/issues" params (length issues))
    (if (called-interactively-p 'any)
        (insert result)
      result)))

(defun redmine-api-org-get-bugs (level &optional params)
  (interactive (list (and current-prefix-arg (prefix-numeric-value current-prefix-arg))
                     (let ((expr (read-from-minibuffer "Params alist: ")))
                       (if (string-equal expr "")
                           nil
                         (eval-string expr)))))
  (setq level (or level (1+ (org-outline-level))))
  (map-put params "tracker_name" "~bug" 'string-equal)
  (let* ((issues (cdr (assoc 'issues (redmine-api-data "/issues" "GET" params))))
         (result (redmine-api-org-convert issues "/issues/{id}" level)))
    (if redmine-api-debug (let ((item-count 0))
                            (mapc (lambda (item)
                                    (message "  Item %i" (cl-incf item-count))
                                    (mapc (lambda (prop)
                                            (message "    %s" prop)) item))
                                  issues)))
    (message "%s %s %s: %i items" "GET" "/issues" params (length issues))
    (if (called-interactively-p 'any)
        (insert result)
      result)))

(defun redmine-api-org-update-entry-at-point ()
  (interactive)
  (let* ((properties (org-entry-properties nil 'standard))
         (type (replace-regexp-in-string
                "^.*/\\([a-zA-Z0-9_-]\\{3\\}\\)[a-zA-Z0-9_-]*/[^/]*$" "\\1"
                (or (assoc-default "ORIGIN" properties)
                    (assoc-default "RESOURCE" properties)) t))
         (resource (redmine-api--fill-template (cdr (assoc "RESOURCE" properties)) properties))
         (resource-datas (cdr (assoc 'issue (redmine-api-data resource "GET")))))
    (if redmine-api-debug (mapc (lambda (prop) (message "    %s" prop)) resource-datas))
    (message "%s %s %s: %i properties" "GET" resource nil (length resource-datas))
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
                      ("status.name" (org-entry-put nil "TODO" (redmine-api--convert-state data type)))))))))
          properties)))


(provide 'redmine-api)
;;; redmine-api.el ends here
