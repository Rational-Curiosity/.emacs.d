;;; avy-config.el --- Configure avy

;;; Commentary:

;; Usage:
;; (require 'avy-config)

;;; Code:

;; goto
(require 'avy)

(setq avy-keys '(?q ?w ?e ?r ?t ?y ?u ?i ?o ?p
                    ?a ?s ?d ?f ?g ?h ?j ?k ?l
                    ?z ?x ?c ?v ?b ?n ?m))
;;;;;;;;;;
;; Keys ;;
;;;;;;;;;;
(bind-keys
 ("M-g c"               . avy-goto-char)
 ("M-g C"               . avy-goto-char-2)
 ("M-g s"               . avy-goto-char-timer)
 ("M-g l"               . avy-goto-line)
 ("M-g w"               . avy-goto-word-1)
 ("M-g W"               . avy-goto-word-0)
 ("M-z"                 . avy-goto-char-timer))


(provide 'avy-config)
;;; avy-config.el ends here
