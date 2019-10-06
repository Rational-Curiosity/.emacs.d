;;; highlight-config.el --- Configure highlight

;;; Commentary:

;;; Code:

(require 'hi-lock)
(setcar (cdr (assq 'hi-lock-mode minor-mode-alist)) "")

(setq hi-lock-highlight-range 2000000)
;;;;;;;;;;;
;; Faces ;;
;;;;;;;;;;;
(defface hi-yellow-b
  '((((min-colors 88)) (:weight bold :foreground "yellow1"))
    (t (:weight bold :foreground "yellow")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(require 'hl-line)
(set-face-attribute 'hl-line nil
                    :foreground 'unspecified
                    :background 'unspecified
                    :overline 'unspecified
                    :underline t
                    :box 'unspecified
                    :inherit 'unspecified)
;; (global-hl-line-mode 1)

;;;;;;;;;;;;;;;
;; Functions ;;
;;;;;;;;;;;;;;;
(require 'pulse)
(setq pulse-flag t)
(defun pulse-momentary-highlight-current-line (delay)
  (interactive (list 1.2))
  (let ((pulse-delay (/ delay pulse-iterations)))
    (pulse-momentary-highlight-one-line (point))))

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

(defvar hl-datetime-today-last nil)
(make-variable-buffer-local 'hl-datetime-today-last)

(defun hl-datetime ()
  "Highlight today date."
  (interactive)
  (highlight-regexp "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]" 'hi-yellow-b)
  (let ((date (format-time-string "%Y-%m-%d")))
    (if hl-datetime-today-last
        (unless (string-equal date hl-datetime-today-last)
          (unhighlight-regexp hl-datetime-today-last)
          (setq hl-datetime-today-last date))
      (setq hl-datetime-today-last date)))
  (highlight-regexp hl-datetime-today-last 'hi-green-b)
  (highlight-regexp "[0-9][0-9]:[0-9][0-9]:[0-9][0-9]" 'hi-yellow-b))

(defun unhl-datetime ()
  "Highlight today date."
  (interactive)
  (if hl-datetime-today-last
      (unhighlight-regexp hl-datetime-today-last))
  (unhighlight-regexp "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]")
  (unhighlight-regexp "[0-9][0-9]:[0-9][0-9]:[0-9][0-9]"))

(defun hl-log ()
  (interactive)
  (hl-advices)
  (hl-datetime)
  (hl-apirest))

(defun unhl-log ()
  (interactive)
  (unhl-advices)
  (unhl-datetime)
  (unhl-apirest))


(provide 'highlight-config)
;;; highlight-config.el ends here
