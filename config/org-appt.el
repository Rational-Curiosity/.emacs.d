;;; org-appt.el --- Configure android mode

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'org
;;   (require 'org-appt))
;; or:
;; (with-eval-after-load 'org
;;   )
;; never:
;; (require 'org-appt)

;; Do not include in this file:
;; (require 'android-mode)

;;; Code:
(message "Importing org-appt")

(require 'appt)
(fset 'appt-start-process
      (if (require 'async nil t)
          #'async-start-process
        (lambda (name program finish &rest args)
          (apply #'start-process name finish program args))))

(setq appt-disp-window-function 'org-appt-disp-function
      ;; appt-delete-window-function (lambda () t)
      appt-display-interval 3
      appt-message-warning-time (* 3 appt-display-interval)
      appt-display-duration 12
      appt-display-diary nil
      diary-file "~/.emacs.d/cache/diary"
      ;; delete old appointments
      ;; appt-time-msg-list nil
      )
;; turn appointment checking on
(appt-activate t)
;; Add org agenda appointments
;; t means refresh
(defun org-appt-update (&rest args)
  (org-agenda-to-appt t))
(advice-add 'org-deadline :after #'org-appt-update)
(advice-add 'org-schedule :after #'org-appt-update)
(advice-add 'org-agenda-schedule :after #'org-appt-update)

(defvar appt-disp-function
  (cond
   ((executable-find "xmessage")
    (lambda (min time msg)
      (appt-start-process "xmessage disp" "xmessage" nil
                          (concat min " minutes to " msg " on " time))))
   ((executable-find "termux-notification")
    (lambda (min time msg)
      (let ((message (concat min " minutos para " msg)))
        (appt-start-process "termux disp" "termux-notification" nil
                            "-c" message
                            "-t" "Org Agenda")
        (appt-start-process "termux disp" "termux-tts-speak" nil
                            "-l" "es" message)
        (appt-start-process "termux disp" "termux-vibrate" nil
                            "-d" "1000"))))
   (t
    (lambda (min time msg)
      (message "%s minutes to %s on %s" min msg time))))
  "Function that display message.")

(defun org-appt-disp-function (min-to-app new-time appt-msg)
  "Display appointment due in MIN-TO-APP (a string) minutes.
NEW-TIME is a string giving the current date.
Displays the appointment message APPT-MSG in a separate buffer.
The arguments may also be lists, where each element relates to a
separate appointment."
  (appt-disp-window min-to-app new-time appt-msg)
  (if appt-disp-function
      (if (atom min-to-app)
          (funcall appt-disp-function min-to-app new-time appt-msg)
        (cl-mapc (lambda (min msg) (funcall appt-disp-function min new-time msg))
                 min-to-app appt-msg))))


(provide 'org-appt)
;;; org-appt.el ends here
