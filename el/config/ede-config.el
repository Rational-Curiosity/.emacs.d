;;; ede-config.el --- Configure ede

;;; Commentary:

;; Usage:
;; (require 'ede-config)

;;; Code:

(message "Importing ede-config")
;; ede is difficult for management
;; (require 'cedet)
;; (require 'ede/source)
;; (require 'ede/base)
;; (require 'ede/auto)
;; (require 'ede/proj)
;; (require 'ede/proj-archive)
;; (require 'ede/proj-aux)
;; (require 'ede/proj-comp)
;; (require 'ede/proj-elisp)
;; (require 'ede/proj-info)
;; (require 'ede/proj-misc)
;; (require 'ede/proj-obj)
;; (require 'ede/proj-prog)
;; (require 'ede/proj-scheme)
;; (require 'ede/proj-shared)

;; [ unstable
;; ;; advice 'projectile' functions to work with 'ede'
;; (defun ede-add-to-projectile-project-root (orig-fun &rest args)
;;   (condition-case nil
;;       (file-name-directory (oref (ede-current-project) file))
;;     (error (apply orig-fun args))))
;; (advice-add 'projectile-project-root :around #'ede-add-to-projectile-project-root)

;; (defun ede-add-to-projectile-project-name (orig-fun &rest args)
;;   (condition-case nil
;;       (oref (ede-current-project) name)
;;     (error (apply orig-fun args))))
;; (advice-add 'projectile-project-name :around #'ede-add-to-projectile-project-name)
;; ]

;;(global-ede-mode 1)
;; Unknown error
;; (ede-enable-generic-projects)


(provide 'ede-config)
;;; ede-config.el ends here
