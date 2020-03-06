;;; menu-config.el --- menu Configurations

;;; Commentary:

;; Usage:
;; (require 'menu-config)

;;; Code:

(require 'recentf)

(defun recentf-remove-sudo-tramp-prefix-remove-sudo (x)
  "Remove sudo from path.  Argument X is path."
  (if (tramp-tramp-file-p x)
      (let ((tx (tramp-dissect-file-name x)))
        (if (string-equal "sudo" (tramp-file-name-method tx))
            (tramp-file-name-localname tx) x))
    x))

(setq recentf-max-saved-items 500
      recentf-max-menu-items 30
      recentf-exclude '("\\.emacs\\.d/elpa/.*\\.el\\'" "\\.el\\.gz\\'")
      recentf-filename-handlers '(recentf-remove-sudo-tramp-prefix-remove-sudo
                                  file-truename
                                  abbreviate-file-name)
      ;; recentf-keep '()
      tool-bar-max-label-size 12
      tool-bar-style 'image)
;; There are machine dependent configurations
;; (with-eval-after-load 'machine-config
;;   (cl-letf (((symbol-function 'sit-for)
;;              (lambda (secs))))
;;     (let ((tramp-message-show-message nil))
;;       (recentf-mode 1))))


(provide 'menu-config)
;;; menu-config.el ends here
