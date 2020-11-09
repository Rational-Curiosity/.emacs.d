;;; highlight-config.el --- Configure highlight

;;; Commentary:

;;; Code:
(require 'highlight)
(require 'hi-lock)
(setq minor-mode-alist (assq-delete-all 'hi-lock-mode minor-mode-alist))
(setcdr hi-lock-map nil)

(setq hi-lock-highlight-range 2000000)
;;;;;;;;;;;
;; Faces ;;
;;;;;;;;;;;
(defface hi-yellow-b
  '((((min-colors 88)) (:weight bold :foreground "yellow1"))
    (t (:weight bold :foreground "yellow")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-yellow-l
  '((((min-colors 88)) (:weight light :foreground "yellow1"))
    (t (:weight light :foreground "yellow")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-magenta-b
  '((((min-colors 88)) (:weight bold :foreground "magenta1"))
    (t (:weight bold :foreground "magenta")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-magenta-l
  '((((min-colors 88)) (:weight light :foreground "magenta1"))
    (t (:weight light :foreground "magenta")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-blue-l
  '((((min-colors 88)) (:weight light :foreground "blue1"))
    (t (:weight light :foreground "blue")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-green-l
  '((((min-colors 88)) (:weight light :foreground "green1"))
    (t (:weight light :foreground "green")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(defface hi-red-l
  '((((min-colors 88)) (:weight light :foreground "red1"))
    (t (:weight light :foreground "red")))
  "Face for hi-lock mode."
  :group 'hi-lock-faces)

(require 'hl-line)
;; [ conflict with objed with (setq objed-use-hl nil)
;; (require 'hl-line+)
;; ]
(set-face-attribute 'hl-line nil
                    :foreground 'unspecified
                    :background "#000000"
                    :overline 'unspecified
                    :underline 'unspecified
                    :box 'unspecified
                    :inherit 'unspecified)
(global-hl-line-mode 1)

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
  (highlight-regexp "\\_<debug\\_>" 'hi-green-l)
  (highlight-regexp "\\_<info\\_>" 'hi-blue-l)
  (highlight-regexp "\\_<information\\_>" 'hi-blue-l)
  (highlight-regexp "\\_<assertion\\_>" 'hi-yellow)
  (highlight-regexp "\\_<ERROR\\_>" 'hi-red-b)
  (highlight-regexp "\\_<WARN\\_>" 'hi-yellow-b)
  (highlight-regexp "\\_<WARNING\\_>" 'hi-yellow-b)
  (highlight-regexp "\\_<DEBUG\\_>" 'hi-green-l)
  (highlight-regexp "\\_<INFO\\_>" 'hi-blue-l)
  (highlight-regexp "\\_<INFORMATION\\_>" 'hi-blue-l)
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

(defun hl-ip ()
  (interactive)
  (highlight-regexp "[0-9]\\{1,3\\}\\.[0-9]\\{1,3\\}\\.[0-9]\\{1,3\\}\\.[0-9]\\{1,3\\}" 'hi-magenta-l))

(defun unhl-ip ()
  (interactive)
  (unhighlight-regexp "[0-9]\\{1,3\\}\\.[0-9]\\{1,3\\}\\.[0-9]\\{1,3\\}\\.[0-9]\\{1,3\\}"))

;; datetime
(defvar hl-datetime-today-last nil)
(make-variable-buffer-local 'hl-datetime-today-last)

(defun hl-datetime ()
  "Highlight today date."
  (interactive)
  (highlight-regexp "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]" 'hi-yellow-l)
  (let ((date (format-time-string "%Y-%m-%d")))
    (if hl-datetime-today-last
        (unless (string-equal date hl-datetime-today-last)
          (unhighlight-regexp hl-datetime-today-last)
          (setq hl-datetime-today-last date))
      (setq hl-datetime-today-last date)))
  (highlight-regexp hl-datetime-today-last 'hi-yellow-b)
  (highlight-regexp "[0-9][0-9]:[0-9][0-9]:[0-9][0-9]" 'hi-yellow-l))

(defun unhl-datetime ()
  "Highlight today date."
  (interactive)
  (if hl-datetime-today-last
      (unhighlight-regexp hl-datetime-today-last))
  (unhighlight-regexp "[0-9][0-9][0-9][0-9]-[0-9][0-9]-[0-9][0-9]")
  (unhighlight-regexp "[0-9][0-9]:[0-9][0-9]:[0-9][0-9]"))

(defun hl-log ()
  (interactive)
  (hl-ip)
  (hl-advices)
  (hl-datetime)
  (hl-apirest))

(defun unhl-log ()
  (interactive)
  (unhl-ip)
  (unhl-advices)
  (unhl-datetime)
  (unhl-apirest))


(provide 'highlight-config)
;;; highlight-config.el ends here
