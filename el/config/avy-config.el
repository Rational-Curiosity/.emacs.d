;;; avy-config.el --- Configure avy

;;; Commentary:

;; Usage:
;; (require 'avy-config)

;;; Code:

;; goto
(require 'avy)
(require 'link-hint)

(setq avy-keys '(?q ?w ?e ?r ?t ?y ?u ?i ?o ?p
                    ?a ?s ?d ?f ?g ?h ?j ?k ?l
                    ?z ?x ?c ?v ?b ?n ?m))
;;;;;;;;;;
;; Keys ;;
;;;;;;;;;;
(global-set-key (kbd "M-g c") #'avy-goto-char)
(global-set-key (kbd "M-g C") #'avy-goto-char-2)
(global-set-key (kbd "M-g s") #'avy-goto-char-timer)
(global-set-key (kbd "M-g l") #'avy-goto-line)
(global-set-key (kbd "M-g w") #'avy-goto-word-1)
(global-set-key (kbd "M-g W") #'avy-goto-word-0)
(global-set-key (kbd "M-z")   #'avy-goto-char-timer)
(global-set-key (kbd "M-g k") #'link-hint-open-link)
(global-set-key (kbd "M-g K") #'link-hint-copy-link)


(provide 'avy-config)
;;; avy-config.el ends here
