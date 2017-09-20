(require 'srefactor)


(defun srefactor--insert-function (func-tag type)
  "Insert function implementations for FUNC-TAG at point, a tag that is a function."
  (if srefactor-use-srecode-p
      ;; Try using SRecode as the mechanism for inserting a tag.
      (let* ((copy (semantic-tag-copy func-tag))
             ;; (parent (semantic-tag-calculate-parent func-tag))
             ;; TODO - below srefactor fcn should be a part of semantic or srecode.
             (parentstring1 (srefactor--tag-parents-string func-tag))
             (parentstring (substring parentstring1 0 (- (length parentstring1) 2)))
             (endofinsert nil))
        ;; Copied this line from original
        (semantic-tag-put-attribute func-tag :typemodifiers nil)
        (semantic-tag-put-attribute func-tag :parent parentstring)
        ;; Insert the tag
        (require 'srecode/semantic)
        ;; TODO - does it need any special dictionary entries?
        (setq endofinsert
              (srecode-semantic-insert-tag
               func-tag
               nil ;; Style
               (lambda (localtag)
                 (srefactor--insert-initial-content-based-on-return-type
                  (if (or (srefactor--tag-function-constructor copy)
                          (srefactor--tag-function-destructor copy))
                      ""
                    (semantic-tag-type copy)))
                 ) ;; Callbck for function body.
               ;; Dictionary entries go here.
               ))
        (goto-char endofinsert)
        (insert "\n\n"))
    ;; official routine
    ;; add 2 newlines before insert the function
    (newline-and-indent)
    (newline-and-indent)
    (let ((func-tag-name (srefactor--tag-name func-tag))
          (parent (srefactor--calculate-parent-tag func-tag)))
      (when (srefactor--tag-function-modifiers func-tag)
        (semantic-tag-put-attribute func-tag :typemodifiers nil))
      (save-excursion
        (when (and (eq major-mode 'c++-mode)
                   parent)
          (insert (srefactor--tag-templates-declaration-string parent)))
        (insert (srefactor--tag-function-string func-tag))
        (when (eq type 'gen-func-proto)
          (insert ";\n")))
      (unless (eq major-mode 'c-mode)
        (search-forward-regexp (regexp-quote func-tag-name) (line-end-position) t)
        (search-backward-regexp (regexp-quote func-tag-name) (line-beginning-position) t)
        (when (srefactor--tag-function-destructor func-tag)
          (forward-char -1))

        ;; insert tag parent if any
        (let ((tag (semantic-obtain-foreign-tag))) ; bug
          (if tag
              (let* ((parents-pos (srefactor--tag-parents-string tag)) ; bug
                     (parents-remove (concat parents-pos (car tag) "::")) ; bug
                     (parents (srefactor--tag-parents-string func-tag))) ; bug
                (insert (replace-regexp-in-string (concat "^" parents-remove) "" parents))) ; bug
            (insert (srefactor--tag-parents-string func-tag)))) ; bug
        (when (srefactor--tag-function-constructor func-tag)
          (let ((variables (srefactor--tag-filter #'semantic-tag-class
                                                  '(variable)
                                                  (semantic-tag-type-members parent))))
            (setq variables
                  (remove-if-not (lambda (v)
                                   (string-match "const" (srefactor--tag-type-string v)))
                                 variables))
            (when variables
              (goto-char (line-end-position))
              (insert ":")
              (mapc (lambda (v)
                      (when (string-match "const" (srefactor--tag-type-string v))
                        (insert (semantic-tag-name v))
                        (insert "()")))
                    variables))))))))

;; [ Arregla variables con comentarios antes
(defun srefactor--c-tag-start-with-comment (tag)
  (+ (semantic-tag-start tag) 1))
;; xor
;; (defun srefactor--c-tag-start-with-comment (tag)
;;   (semantic-tag-start tag))
;; (defun srefactor--get-all-parents (tag)
;;   "Return a list of parent tags of a TAG.
;; The closer to the end of the list, the higher the parents."
;;   (let* ((tag-buffer (semantic-tag-buffer tag))
;;          (parent (with-current-buffer (if tag-buffer
;;                                           tag-buffer
;;                                         (current-buffer))
;;                    (save-excursion
;;                      (goto-char (+ (semantic-tag-start tag) 1))
;;                      (semantic-current-tag-parent)))))
;;     (when parent
;;       (cons parent (srefactor--get-all-parents parent)))))
;; ]

;; [ Arregla que se pongan a minúsculas los nombres de variables
(defun srefactor--insert-getter (tag &optional newline-before newline-after prototype-p)
  "Insert getter for TAG.
Add NEWLINE-BEFORE and NEWLINE-AFTER if t."
  (let ((tag-type (srefactor--tag-type-string tag))
        (tag-buffer (semantic-tag-buffer tag))
        (tag-parent-string "")
        tag-name beg)
    (setq beg (point))
    (unless (eq tag-buffer (current-buffer))
      (setq tag-parent-string (srefactor--tag-parents-string tag)))
    (when newline-before
      (newline newline-before))
    (when (and (or (listp (semantic-tag-type tag))
                   (semantic-tag-get-attribute tag :pointer))
               (not (semantic-tag-get-attribute tag :constant-flag)))
      (insert "const "))
    (insert tag-type)
    (setq tag-name (replace-regexp-in-string srefactor--getter-setter-removal-prefix
                                             ""
                                             (semantic-tag-name tag)))
    (insert (concat " "
                    tag-parent-string
                    srefactor--getter-prefix
                    (if srefactor--getter-setter-capitalize-p
                        (upcase-initials tag-name) ; bug
                      tag-name)))
    (insert "() const")
    (if prototype-p
        (insert ";")
      (insert " {")
      (srefactor--indent-and-newline 1)
      (insert (concat "return"
                      " "
                      (semantic-tag-name tag) ";"))
      (srefactor--indent-and-newline 1)
      (insert "}")
      (indent-according-to-mode)
      (when newline-after
        (newline newline-after)))
    (indent-region beg (point))))

(defun srefactor--insert-setter (tag newline-before newline-after &optional prototype-p)
  "Insert setter for TAG.
Add NEWLINE-BEFORE and NEWLINE-AFTER if t."
  (when newline-before
    (newline newline-before))
  (let ((tag-type (srefactor--tag-type-string tag))
        (tag-type (srefactor--tag-type-string tag))
        (tag-pointer (srefactor--tag-pointer tag))
        (tag-name (semantic-tag-name tag))
        (tag-type-string (srefactor--tag-type-string tag))
        (tag-buffer (semantic-tag-buffer tag))
        tag-parent-string modified-tag-name beg)
    (setq beg (point))
    (unless (eq tag-buffer (current-buffer))
      (setq tag-parent-string (srefactor--tag-parents-string tag)))
    (insert "void")
    (setq modified-tag-name (replace-regexp-in-string srefactor--getter-setter-removal-prefix
                                                      ""
                                                      (semantic-tag-name tag)))
    (insert (concat " "
                    tag-parent-string
                    srefactor--setter-prefix
                    (if srefactor--getter-setter-capitalize-p
                        (upcase-initials modified-tag-name) ; bug
                      modified-tag-name)))
    (insert (concat (insert "(")
                    (unless (semantic-tag-variable-constant-p tag)
                      "const ")
                    tag-type
                    (when (and (listp tag-type)
                               ;; (srefactor--tag-reference tag)
                               (not tag-pointer))
                      "&")
                    " "
                    tag-name
                    ")"))
    (if prototype-p
        (insert ";")
      (insert " {")
      (srefactor--indent-and-newline 1)
      (insert (concat "this->" tag-name " = " tag-name ";"))
      (srefactor--indent-and-newline 1)
      (insert "}")
      (indent-according-to-mode)
      (when newline-after
        (newline newline-after)))
    (indent-region beg (point))))
;; ]
(provide 'srefactor-bug)
