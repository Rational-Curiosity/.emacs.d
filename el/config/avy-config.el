;;; avy-config.el --- Configure avy

;;; Commentary:

;; Usage:
;; (require 'avy-config)

;;; Code:

;; goto
(require 'avy)
(require 'link-hint)

(setq avy-keys
      (let ((keys
             '(?q ?w ?e ?r ?t ?y ?u ?i ?o ?p
                  ?a ?s ?d ?f ?g ?h ?j ?k ?l
                  ?z ?x ?c ?v ?b ?n ?m)))
        (dolist (dispatch avy-dispatch-alist)
          (setq keys (delete (car dispatch) keys)))
        keys)
      avy-single-candidate-jump t)

(defmacro avy-prefix-all-windows (avy-fun)
  `(defun ,(intern (concat (symbol-name avy-fun)
                           "-prefix-all-windows"))
       (arg)
     (interactive "p")
     (let ((current-prefix-arg nil)
           (avy-all-windows (cl-case arg
                             (1 nil)
                             ((2 4) t)
                             ((3 5 16) 'all-frames)
                             (otherwise avy-all-windows))))
       (call-interactively (quote ,avy-fun)))))

;;;;;;;;;;
;; Keys ;;
;;;;;;;;;;
(global-set-key (kbd "M-g c") (avy-prefix-all-windows
                               avy-goto-char))
(global-set-key (kbd "M-g C") (avy-prefix-all-windows
                               avy-goto-char-2))
(global-set-key (kbd "M-g s") (avy-prefix-all-windows
                               avy-goto-char-timer))
(global-set-key (kbd "M-g l") (avy-prefix-all-windows
                               avy-goto-line))
(global-set-key (kbd "M-g w") (avy-prefix-all-windows
                               avy-goto-word-1))
(global-set-key (kbd "M-g W") (avy-prefix-all-windows
                               avy-goto-word-0))
(global-set-key (kbd "M-z")   (avy-prefix-all-windows
                               avy-goto-word-or-subword-1))
(global-set-key (kbd "M-Z")   (avy-prefix-all-windows
                               avy-resume))
(global-set-key (kbd "M-g k") (avy-prefix-all-windows
                               link-hint-open-link))
(global-set-key (kbd "M-g K") (avy-prefix-all-windows
                               link-hint-copy-link))
(global-set-key (kbd "M-g SPC") 'avy-pop-mark)


(provide 'avy-config)
;;; avy-config.el ends here
