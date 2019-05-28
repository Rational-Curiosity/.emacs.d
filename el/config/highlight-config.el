;;; highlight-config.el --- Configure highlight

;;; Commentary:

;;; Code:

(require 'hi-lock)
(setcar (cdr (assq 'hi-lock-mode minor-mode-alist)) "")

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
  (highlight-regexp "\\_<assertion\\_>" 'hi-yellow)
  (highlight-regexp "\\_<ERROR\\_>" 'hi-red-b)
  (highlight-regexp "\\_<WARN\\_>" 'hi-yellow-b)
  (highlight-regexp "\\_<WARNING\\_>" 'hi-yellow-b)
  (highlight-regexp "\\_<DEBUG\\_>" 'hi-green-b)
  (highlight-regexp "\\_<INFO\\_>" 'hi-blue-b)
  (highlight-regexp "\\_<INFORMATION\\_>" 'hi-blue-b)
  (highlight-regexp "\\_<ASSERTION\\_>" 'hi-yellow))

(defun unhl-advices ()
  "Unhighlight advices."
  (interactive)
  (unhighlight-regexp "\\_<error\\_>")
  (unhighlight-regexp "\\_<warn\\_>")
  (unhighlight-regexp "\\_<warning\\_>")
  (unhighlight-regexp "\\_<debug\\_>")
  (unhighlight-regexp "\\_<info\\_>")
  (unhighlight-regexp "\\_<information\\_>")
  (unhighlight-regexp "\\_<assertion\\_>")
  (unhighlight-regexp "\\_<ERROR\\_>")
  (unhighlight-regexp "\\_<WARN\\_>")
  (unhighlight-regexp "\\_<WARNING\\_>")
  (unhighlight-regexp "\\_<DEBUG\\_>")
  (unhighlight-regexp "\\_<INFO\\_>")
  (unhighlight-regexp "\\_<INFORMATION\\_>")
  (unhighlight-regexp "\\_<ASSERTION\\_>"))

(defun hl-apirest ()
  (interactive)
  (highlight-regexp "\\_<GET\\_>" 'hi-green-b)
  (highlight-regexp "\\_<POST\\_>" 'hi-green-b)
  (highlight-regexp "\\_<PUT\\_>" 'hi-green-b)
  (highlight-regexp "\\_<PATCH\\_>" 'hi-green-b)
  (highlight-regexp "\\_<DELETE\\_>" 'hi-green-b))

(defun unhl-apirest ()
  (interactive)
  (unhighlight-regexp "\\_<GET\\_>")
  (unhighlight-regexp "\\_<POST\\_>")
  (unhighlight-regexp "\\_<PUT\\_>")
  (unhighlight-regexp "\\_<PATCH\\_>")
  (unhighlight-regexp "\\_<DELETE\\_>"))

(defun hl-today ()
  "Highlight today date."
  (interactive)
  (highlight-regexp (format-time-string "%Y-%m-%d") 'hi-blue))

(defun unhl-today ()
  "Highlight today date."
  (interactive)
  (unhighlight-regexp (format-time-string "%Y-%m-%d")))

(defun hl-log ()
  (interactive)
  (hl-advices)
  (hl-today)
  (hl-apirest))

(defun unhl-log ()
  (interactive)
  (unhl-advices)
  (unhl-today)
  (unhl-apirest))


(provide 'highlight-config)
;;; highlight-config.el ends here
