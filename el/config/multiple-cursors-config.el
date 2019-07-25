;;; multiple-cursors-config.el --- Configure multiple-cursors

;;; Commentary:

;; Usage:
;; (require 'multiple-cursors-config)

;;; Code:

(require 'multiple-cursors)

;; Add a cursor to each line in active region
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

;; Add a cursor based on keywords in the buffer
;; (global-set-key (kbd "C->") 'mc/mark-next-like-this)
;; (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)

(global-set-key (kbd "C-<") #'mc/mark-previous-like-this)
(global-set-key (kbd "C->") #'mc/mark-next-like-this)
(global-set-key (kbd "C-c C-<") #'mc/mark-all-like-this-dwim)
(global-set-key (kbd "C-S-<mouse-1>") #'mc/add-cursor-on-click)


;; [ Add a cursor win mouse
;; another option
;; (global-unset-key (kbd "M-<down-mouse-1>"))
;; (global-set-key (kbd "M-<mouse-1>") 'mc/add-cursor-on-click)
;; ]

(provide 'multiple-cursors-config)
;;; multiple-cursors-config.el ends here
