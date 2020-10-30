;;; ace-window-config.el --- Translation tools

;;; Commentary:

;; Usage:
;; after (require 'mode-line-config)
;; (require 'ace-window-config)

;;; Code:
(require 'ace-window)
(set-face-attribute 'aw-mode-line-face nil
                    :bold t
                    :foreground "red")

(push " *which-key*" aw-ignored-buffers)
(push '(?O aw-refresh "Refresh mode-line") aw-dispatch-alist)
(setq aw-scope 'global
      aw-keys
      (let ((keys
             '(?a ?b ?c ?d ?e ?f ?g ?h ;; ?i
                  ?j ?k ?l ?m ?n ?o ?p ?q ;; ?r
                  ?s ;; ?t
                  ?u ?v ?w ?x ?y ?z)))
        (dolist (dispatch aw-dispatch-alist)
          (setq keys (delete (car dispatch) keys)))
        keys)
      aw-dispatch-always t
      aw-minibuffer-flag t
      aw-background t)

(defun aw-refresh ()
  (interactive)
  (aw-update)
  (force-mode-line-update t))

(ace-window-display-mode)

(global-set-key (kbd "M-o") 'ace-window)
(global-set-key (kbd "M-O") 'aw-refresh)


(provide 'ace-window-config)
;;; ace-window-config.el ends here
