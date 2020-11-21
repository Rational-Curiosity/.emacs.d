;;; ace-window-config.el --- Translation tools

;;; Commentary:

;; Usage:
;; after (require 'mode-line-config)
;; (require 'ace-window-config)

;;; Code:
(require 'ace-window)
(set-face-attribute 'aw-mode-line-face nil
                    :weight 'bold
                    :foreground "mint cream")
(set-face-attribute 'aw-leading-char-face nil
                    :background "green"
                    :height 160)

(push " *which-key*" aw-ignored-buffers)
(setq   aw-scope 'global
        aw-dispatch-alist
        '((?R aw-refresh "Refresh mode-line")
          (?X aw-delete-window "Delete Window")
          (?S aw-swap-window "Swap Windows")
          (?M aw-move-window "Move Window")
          (?C aw-copy-window "Copy Window")
          (?J aw-switch-buffer-in-window "Select Buffer")
          (?N aw-flip-window)
          (?U aw-switch-buffer-other-window "Switch Buffer Other Window")
          (?E aw-execute-command-other-window "Execute Command Other Window")
          (?F aw-split-window-fair "Split Fair Window")
          (?V aw-split-window-vert "Split Vert Window")
          (?B aw-split-window-horz "Split Horz Window")
          (?O delete-other-windows "Delete Other Windows")
          (?T aw-transpose-frame "Transpose Frame")
          ;; ?i ?r ?t are used by hyperbole.el
          (?? aw-show-dispatch-help))
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
