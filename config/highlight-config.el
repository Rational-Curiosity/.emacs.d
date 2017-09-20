;;; highlight-config.el --- Configure highlight

;;; Commentary:

;;; Code:

(require 'hi-lock)

;; Faces

(defface hi-yellow-b
  '((((min-colors 88)) (:weight bold :foreground "yellow1"))
    (t (:weight bold :foreground "yellow")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defun hl-smaller-5 ()
  "Highlight nunbers smaller than 5."
  (interactive)
  (highlight-regexp " [0-4]\\.[0-9]* " 'hi-red-b))

(defun unhl-smaller-5 ()
  "Unhighlight nunbers smaller than 5."
  (interactive)
  (unhighlight-regexp " [0-4]\\.[0-9]* "))

(defun hl-advices ()
  "Highlight advices."
  (interactive)
  (highlight-regexp "\\_<error\\_>" 'hi-red-b)
  (highlight-regexp "\\_<warn\\_>" 'hi-yellow-b)
  (highlight-regexp "\\_<warning\\_>" 'hi-yellow-b)
  (highlight-regexp "\\_<debug\\_>" 'hi-green-b)
  (highlight-regexp "\\_<info\\_>" 'hi-blue-b)
  (highlight-regexp "\\_<information\\_>" 'hi-blue-b)
  (highlight-regexp "\\_<assertion\\_>" 'hi-yellow))

(defun unhl-advices ()
  "Unhighlight advices."
  (interactive)
  (unhighlight-regexp "\\_<error\\_>")
  (unhighlight-regexp "\\_<warn\\_>")
  (unhighlight-regexp "\\_<warning\\_>")
  (unhighlight-regexp "\\_<debug\\_>")
  (unhighlight-regexp "\\_<info\\_>")
  (unhighlight-regexp "\\_<information\\_>")
  (unhighlight-regexp "\\_<assertion\\_>"))

(provide 'highlight-config)
;;; highlight-config.el ends here
