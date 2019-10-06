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
;; create the autosave dir if necessary, since emacs won't.
;; with backup directory is not necessary
(let ((backup-directory (expand-file-name "~/.emacs.d/backup/")))
  (make-directory backup-directory t)

  (setq backup-directory-alist
        `((".*" . ,backup-directory))
        ;; auto-save-file-name-transforms
        ;; '((".*" "~/.emacs.d/backup/" t))
        ;; Graba cada 10 caracteres introducidos a #<file-name>#
        auto-save-interval 10
        ;; o cada 10 segundos
        auto-save-timeout 10
        ;; create local .#<file-name> to avoid collisions
        create-lockfiles nil))

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

(global-set-key (kbd "C-c o m") #'hydra-operate/body)
(global-set-key (kbd "C-c o o") #'operate-on-number-at-point-or-region)

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
