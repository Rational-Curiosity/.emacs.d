(defun eww-submit ()
  "Submit the current form."
  (interactive)
  (let* ((this-input (get-text-property (point) 'eww-form))
     (form (plist-get this-input :eww-form))
     values next-submit)
    (dolist (elem (sort (eww-inputs form)
             (lambda (o1 o2)
               (< (car o1) (car o2)))))
      (let* ((input (cdr elem))
         (input-start (car elem))
         (name (plist-get input :name)))
    (when name
      (cond
       ((member (plist-get input :type) '("checkbox" "radio"))
        (when (plist-get input :checked)
          (push (cons name (plist-get input :value))
            values)))
       ((equal (plist-get input :type) "submit")
        ;; We want the values from buttons if we hit a button if
        ;; we hit enter on it, or if it's the first button after
        ;; the field we did hit return on.
        (when (or (eq input this-input)
              (and (not (eq input this-input))
               (null next-submit)
               (> input-start (point))))
          (setq next-submit t)
          (push (cons name (plist-get input :value))
            values)))
       (t
        (push (cons name (eww-input-value input))
          values))))))
    (dolist (elem form)
      (when (and (consp elem)
         (eq (car elem) 'hidden))
    (push (cons (plist-get (cdr elem) :name)
                (or (plist-get (cdr elem) :value) ""))
          values)))
    (if (and (stringp (cdr (assq :method form)))
         (equal (downcase (cdr (assq :method form))) "post"))
    (let ((url-request-method "POST")
          (url-request-extra-headers
           '(("Content-Type" . "application/x-www-form-urlencoded")))
          (url-request-data (mm-url-encode-www-form-urlencoded values)))
      (eww-browse-url (shr-expand-url (cdr (assq :action form))
                      eww-current-url)))
      (eww-browse-url
       (concat
    (if (cdr (assq :action form))
        (shr-expand-url (cdr (assq :action form))
                eww-current-url)
      eww-current-url)
    "?"
    (mm-url-encode-www-form-urlencoded values))))))


(provide 'eww-bug)
