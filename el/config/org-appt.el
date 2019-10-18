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
;; (require 'org-appt)

;;; Code:
(message "Importing org-appt")

(require 'appt)
(fset 'org-appt-start-process
      (if (require 'async nil t)
          #'async-start-process
        (lambda (name program finish &rest args)
          (apply #'start-process name finish program args))))

(setq appt-disp-window-function 'org-appt-disp-function
      ;; appt-delete-window-function (lambda () t)
      appt-display-interval 5
      appt-message-warning-time (* 2 appt-display-interval)
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
  (interactive (list nil))
  (let (msg)
    (let ((inhibit-message t)
          (message-log-max nil))
      (set 'msg (org-agenda-to-appt t)))
    (let* ((nowtime (current-time))
           (now-ms (nth 2 nowtime)))
      (message (concat msg " at "
                       (format-time-string "%Y-%m-%d %T" nowtime)
                       (format ".%06d" now-ms))))))
(advice-add 'org-deadline :after #'org-appt-update)
(advice-add 'org-schedule :after #'org-appt-update)
(advice-add 'org-agenda-schedule :after #'org-appt-update)
(run-at-time "00:00" (* 24 3600) 'org-appt-update)

(defvar org-appt-disp-functions nil)

;; text notification
(if (not (executable-find "termux-notification"))
    (cl-block 'find
      (cond
       ((executable-find "gxmessage")
        (defun org-appt-function-xmessage (msg)
          (org-appt-start-process "gxmessage disp" "gxmessage" nil msg)))
       ((executable-find "xmessage")
        (defun org-appt-function-xmessage (msg)
          (org-appt-start-process "xmessage disp" "xmessage" nil msg)))
       (t (cl-return-from 'find)))
      (set 'org-appt-disp-functions (cons 'org-appt-function-xmessage org-appt-disp-functions)))
  (defun org-appt-function-termux-notification (msg)
    (org-appt-start-process "termux notification disp" "termux-notification" nil
                            "-c" msg
                            "-t" "Org Agenda"))
  (set 'org-appt-disp-functions (cons 'org-appt-function-termux-notification org-appt-disp-functions)))

;; voice notification
(if (not (executable-find "termux-tts-speak"))
    (cl-block 'find
      (cond
       ((executable-find "sfestival")
        (defun org-appt-function-sfestival (msg)
          (org-appt-start-process "sfestival disp" "sfestival" nil msg)))
       (t (cl-return-from 'find)))
      (set 'org-appt-disp-functions (cons 'org-appt-function-sfestival org-appt-disp-functions)))
  (defun org-appt-function-termux-tts-speak (msg)
    (org-appt-start-process "termux tts disp" "termux-tts-speak" nil
                            "-l" "es" msg))
  (set 'org-appt-disp-functions (cons 'org-appt-function-termux-tts-speak org-appt-disp-functions)))

;; vibrate notification
(when (executable-find "termux-vibrate")
  (defun org-appt-function-termux-vibrate (msg)
    (org-appt-start-process "termux vibrate disp" "termux-vibrate" nil
                            "-d" "1000"))
  (set 'org-appt-disp-functions (cons 'org-appt-function-termux-vibrate org-appt-disp-functions)))

(unless org-appt-disp-functions
  (set 'org-appt-disp-functions (cons 'message org-appt-disp-functions)))


(defun org-appt-format-and-calls (min-to-app new-time appt-msg)
  (let ((msg (concat min-to-app " minutos para " appt-msg)))
    (dolist (fun org-appt-disp-functions)
      (funcall fun msg))))

(defun org-appt-disp-function (min-to-app new-time appt-msg)
  "Display appointment due in MIN-TO-APP (a string) minutes.
NEW-TIME is a string giving the current date.
Displays the appointment message APPT-MSG in a separate buffer.
The arguments may also be lists, where each element relates to a
separate appointment."
  (appt-disp-window min-to-app new-time appt-msg)
  (if org-appt-disp-functions
      (if (atom min-to-app)
          (org-appt-format-and-calls min-to-app new-time appt-msg)
        (cl-mapc (lambda (min msg) (org-appt-format-and-calls min new-time msg))
                 min-to-app appt-msg))))


(provide 'org-appt)
;;; org-appt.el ends here
