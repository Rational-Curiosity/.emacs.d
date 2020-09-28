;;; ellocate-config.el --- Configure and improve ellocate

;;; Commentary:

;; Usage:
;; (require 'ellocate-config)

;;; Code:
(require 'ellocate)
(require 'dash)

(let ((ellocate-dir (expand-file-name "ellocate/" user-emacs-directory)))
  (make-directory ellocate-dir t)
  (setq ellocate-scan-dirs
        (delq nil
              (mapcar
               (lambda (dir-file)
                 (if (file-directory-p (car dir-file))
                     (progn
                       (setcdr dir-file
                               (list
                                (concat ellocate-dir (car (cdr dir-file)))))
                       dir-file)))
               `((,user-emacs-directory "home--emacs-db")
                 ("~/Prog/"             "home-prog-db")
                 ("~/var/"              "home-var-db"))))))

(global-set-key (kbd "C-x F") #'ellocate)


(provide 'ellocate-config)
;;; ellocate-config.el ends here
