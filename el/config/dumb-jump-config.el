;;; dumb-jump-config.el --- Configure dumb-jump-mode

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'dumb-jump
;;   (require 'dumb-jump-config))

;;; Code:

(setq dumb-jump-selector 'ivy)

(defhydra dumb-jump-hydra (:color blue :columns 3)
    "Dumb Jump"
    ("j" dumb-jump-go "Go")
    ("o" dumb-jump-go-other-window "Other win")
    ("e" dumb-jump-go-prefer-external "Go ext")
    ("x" dumb-jump-go-prefer-external-other-window "Go ext other win")
    ("i" dumb-jump-go-prompt "Prompt")
    ("l" dumb-jump-quick-look "Quick look")
    ("b" dumb-jump-back "Back"))

;;;;;;;;;;
;; Keys ;;
;;;;;;;;;;
(bind-keys
 ("M-g o" . dumb-jump-go-other-window)
 ("M-g j" . dumb-jump-go)
 ("M-g i" . dumb-jump-go-prompt)
 ("M-g x" . dumb-jump-go-prefer-external)
 ("M-g z" . dumb-jump-go-prefer-external-other-window))

(provide 'dumb-jump-config)
;;; dumb-jump-config.el ends here
