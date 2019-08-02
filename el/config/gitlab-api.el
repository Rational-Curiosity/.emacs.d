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

(defun gitlab-api-get (url)
  (let ((request-backend 'url-retrieve))
    (request-response-data
     (request url
              :type "GET"
              :encoding 'utf-8
              :headers (list (cons "Private-Token" gitlab-api-token))
              :parser 'json-read
              :sync t
              ))))

(defun gitlab-api-get-projects ()
  (gitlab-api-get (concat gitlab-api-url-base "/projects")))

(defun gitlab-api-get-projects-property (property &optional test)
  (mapcar (lambda (project) (assoc-default property project test)) (gitlab-api-get-projects)))

(defun gitlab-api-get-merge-requests (project-id)
  (gitlab-api-get (concat gitlab-api-url-base "/projects/" (int-to-string project-id) "/merge_requests")))

(defun gitlab-api-get-merge-requests-all ()
  (mapcar 'gitlab-api-get-merge-requests (gitlab-api-get-projects-property 'id)))

(defun gitlab-api--assoc-keys (alist &rest keys)
  "Recursively find KEYS in ALIST."
  (while (and (listp alist) keys)
    (setq alist (cdr (assoc (pop keys) alist))))
  (if keys nil alist))

(defun gitlab-api--convert-state (state)
  (pcase state
    ("merged" "DONE")
    (t state)))

(cl-defun gitlab-api--convert-to-org (data &rest keys
                                           &key
                                           (level 1)
                                           (type nil)
                                           &allow-other-keys)
  (let ((indent (concat "\n" (make-string (1+ level) ?\ )))
        (entry (format "%s %s %s"
                       (make-string level ?*)
                       (gitlab-api--convert-state (assoc-default 'state data))
                       (assoc-default 'title data))))
    (setq entry (concat entry indent ":PROPERTIES:"
                        (and type (concat indent ":TYPE:     " type))))
    (dolist (key keys)
      (setq entry (concat entry indent
                          (if (listp key)
                              (format "%-10s %s"
                                      (concat ":" (mapconcat 'symbol-name key ".") ":")
                                      (gitlab-api--assoc-keys data key))
                            (format "%-10s %s"
                                    (concat ":" (symbol-name key) ":")
                                    (assoc-default key data))))))
    (setq entry (concat entry indent ":END:"))))

(defun gitlab-api--convert-merge-request-to-org ())

(provide 'gitlab-api)
;;; gitlab-api.el ends here
