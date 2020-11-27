;;; menu-config.el --- menu Configurations

;;; Commentary:

;; Usage:
;; (require 'menu-config)

;;; Code:

(require 'recentf)
(require 'tramp)

(add-hook 'after-init-hook 'recentf-cleanup)

;; (with-eval-after-load 'machine-config
;;   (cl-letf (((symbol-function 'sit-for)
;;              (lambda (secs))))
;;     (let ((tramp-message-show-message nil))
;;       (recentf-mode 1))))


(defun icomplete-recentf-find-file (arg)
  "Show a list of recent files."
  (interactive "P")
  (--> recentf-list
       (mapcar #'substring-no-properties it)
       (mapcar #'abbreviate-file-name it)
       (cl-remove-duplicates it :test #'string-equal)
       (let ((minibuffer-completing-file-name t))
         (completing-read "Recent Files: " it nil t))
       (if arg (find-file-other-window it) (find-file it))))

(defun recentf-remove-sudo-tramp-prefix (path)
  "Remove sudo from path.  Argument PATH is path."
  (if (tramp-tramp-file-p path)
      (let ((tx (tramp-dissect-file-name path)))
        (pcase (tramp-file-name-method tx)
          ("sudo" (tramp-file-name-localname tx))
          ("docker" (if (featurep 'docker) path
                      (tramp-file-name-localname path)))
          (_ path)))
    path))

(defun local-file-exists-p (filename)
  (file-exists-p (recentf-remove-sudo-tramp-prefix filename)))

(defun recentf-file-truename (filename)
  (let* ((local-file-name (recentf-remove-sudo-tramp-prefix filename))
         (local-file-truename (file-truename local-file-name)))
    (concat (substring filename 0 (- (length local-file-name))) local-file-truename)))

(setq recentf-max-saved-items 500
      recentf-max-menu-items 30
      recentf-exclude '("\\.emacs\\.d/elpa/.*\\.el\\'" "\\.el\\.gz\\'")
      recentf-filename-handlers '(recentf-file-truename
                                  abbreviate-file-name)
      recentf-keep '(local-file-exists-p)
      tool-bar-max-label-size 12
      recentf-auto-cleanup 'never
      tool-bar-style 'image)

(global-set-key "\C-x\ \C-r" 'icomplete-recentf-find-file)
(recentf-mode 1)

(savehist-mode 1)


(provide 'menu-config)
;;; menu-config.el ends here
