;;; random-functions.el --- Interactive random functions

;;; Commentary:

;; Usage:
;; (require 'random-functions)

;;; Code:

(defun random-goto-line ()
  (interactive)
  (goto-char (point-min))
  (forward-line (random (count-lines (point-min) (point-max)))))

(defun random-goto-char ()
  (interactive)
  (goto-char (random (- (point-max) (point-min)))))


(provide 'random-functions)
;;; random-functions.el ends here
