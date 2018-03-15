;;; misc-config.el --- Miscelaneous configurations and features

;;; Commentary:

;; Usage:
;; (require 'misc-config)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;
;; Al portapapeles ;;
;;;;;;;;;;;;;;;;;;;;;
(defun copy-buffer-file-name ()
  (interactive)
  (kill-new (abbreviate-file-name buffer-file-name)))

(defun copy-buffer-file-name-nondirectory ()
  (interactive)
  (kill-new (file-name-nondirectory buffer-file-name)))

(defun copy-buffer-file-name-directory ()
  (interactive)
  (kill-new (file-name-directory buffer-file-name)))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Backup & Auto save ;;
;;;;;;;;;;;;;;;;;;;;;;;;
(setq backup-directory-alist '(("" . "~/.emacs.d/backup/"))
      auto-save-file-name-transforms
      '((".*" "~/.emacs.d/backup/" t)))
;; create the autosave dir if necessary, since emacs won't.
;; with backup directory is not necessary
(make-directory "~/.emacs.d/backup/" t)

;;;;;;;;;;
;; Math ;;
;;;;;;;;;;

(defun round-on-region (start end arg)
  "Rounds the numbers of the region."
  (interactive "r\nP")
  (save-restriction
    (narrow-to-region start end)
    (goto-char 1)
    (let ((case-fold-search nil))
      (while (search-forward-regexp "\\([0-9]+\\.[0-9]+\\)" nil t)
        (replace-match
         (format
          (concat "%0." (if arg (number-to-string arg) "0") "f")
          (string-to-number (match-string 1))))))))

(require 'operate-on-number)

;;;;;;;;;;
;; Keys ;;
;;;;;;;;;;
(defhydra hydra-operate (:foreign-keys nil)
  "OPERATE"
  ("+"  apply-operation-to-number-at-point)
  ("-"  apply-operation-to-number-at-point)
  ("*"  apply-operation-to-number-at-point)
  ("/"  apply-operation-to-number-at-point)
  ("\\" apply-operation-to-number-at-point)
  ("^"  apply-operation-to-number-at-point)
  ("<"  apply-operation-to-number-at-point)
  (">"  apply-operation-to-number-at-point)
  ("#"  apply-operation-to-number-at-point)
  ("%"  apply-operation-to-number-at-point)
  ("'"  operate-on-number-at-point))

(bind-keys ("C-c o m" . hydra-operate/body)
           ("C-c o o" . operate-on-number-at-point-or-region))

;;;;;;;;;;;;;;;;
;; Mode utils ;;
;;;;;;;;;;;;;;;;
(defun reload-current-major-mode ()
  "Reloads the current major mode."
  (interactive)
  (let ((mode major-mode))
    (message "%s is going to be unloaded" mode)
    (unload-feature mode t)
    (message "%s unloaded" mode)
    (funcall-interactively mode)
    (message "%s loaded" mode)))


(provide 'misc-config)
;;; misc-config.el ends here
