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

(setq aw-scope 'global
      aw-keys
      (let ((keys
             '(?a ?b ?c ?d ?e ?f ?g ?h ?i ?j ?k ?l ?m ?n ?o ?p ?q ?r ?s ?t ?u ?v ?w ?x ?y ?z)))
        (dolist (dispatch aw-dispatch-alist)
          (setq keys (delete (car dispatch) keys)))
        keys)
      aw-dispatch-always t
      aw-minibuffer-flag t
      aw-background t)

(ace-window-display-mode)

(global-set-key (kbd "M-o") 'ace-window)


(provide 'ace-window-config)
;;; ace-window-config.el ends here
