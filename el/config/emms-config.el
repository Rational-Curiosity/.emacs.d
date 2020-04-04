;;; emms-config.el --- Configure and improve emms

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'emms
;;   (require 'emms-config))
;; never:
;; (require 'emms-config)

;; Do not include in this file:
;; (require 'emms)

;;; Code:

(message "Importing emms-config")

(require 'emms-setup)
(emms-all)
(emms-default-players)

(global-set-key (kbd "C-c e P") 'emms-pause)
(global-set-key (kbd "C-c e s") 'emms-stop)
(global-set-key (kbd "C-c e p") 'emms-previous)
(global-set-key (kbd "C-c e n") 'emms-next)

(global-set-key (kbd "<XF86AudioPlay>") 'emms-pause)
(global-set-key (kbd "<XF86AudioStop>") 'emms-stop)
(global-set-key (kbd "<XF86AudioPrev>") 'emms-previous)
(global-set-key (kbd "<XF86AudioNext>") 'emms-next)


(provide 'emms-config)
;;; emms-config.el ends here
