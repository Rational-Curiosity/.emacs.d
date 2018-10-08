(defun format-time-string-advice (orig-fun &rest args)
  (let ((time-string (apply orig-fun args)))
    (dolist (map '(("\\\341" . "á")
                   ("\\\351" . "é")))
      (set 'time-string (replace-regexp-in-string (car map) (cdr map) time-string t t)))
    time-string))
(advice-add 'format-time-string :around #'format-time-string-advice)


(provide 'emacs-win-bug)