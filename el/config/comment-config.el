;;; comment-config.el --- Configure rebox2

;;; Commentary:

;; Usage:
;; (require 'comment-config)

;;; Code:

(require 'rebox2)

(rebox-register-template 227 728
                         '("//,----------"
                           "//| box123456"
                           "//`----------"))

(setq rebox-style-loop '(41 27 23 21 11))

;;;;;;;;;
;; C++ ;;
;;;;;;;;;
(rebox-register-template 247 748
                         '("// ---------"
                           "// box123456"
                           "// ---------"))

(defun my-c++-setup ()
  "Override comment c and c++ defaults."
  (setq comment-start "/* "
        comment-end " */"
        rebox-min-fill-column 90)
  (unless (memq 46 rebox-style-loop)
    (make-local-variable 'rebox-style-loop)
    (setcdr (last rebox-style-loop) '(46 47))))
(add-hook 'c++-mode-hook #'my-c++-setup)

;;(global-set-key [(shift meta q)] 'rebox-dwim)
;;(global-set-key [(meta q)] 'rebox-cycle)

(defhydra hydra-rebox (:foreign-keys run)
  "BOX"
  ("C-+" (lambda () (interactive) (cl-incf rebox-min-fill-column 10) (rebox-fill) (message "Fill column: %i" rebox-min-fill-column)) "+10")
  ("C--" (lambda () (interactive) (cl-decf rebox-min-fill-column 10) (rebox-fill) (message "Fill column: %i" rebox-min-fill-column)) "-10")
  ("C-*" (lambda () (interactive) (cl-incf rebox-min-fill-column 1) (rebox-fill) (message "Fill column: %i" rebox-min-fill-column)) "+1")
  ("C-/" (lambda () (interactive) (cl-decf rebox-min-fill-column 1) (rebox-fill) (message "Fill column: %i" rebox-min-fill-column)) "-1")
  ("C-<left>" (lambda () (interactive) (rebox-cycle '(-1))))
  ("C-<right>" rebox-cycle "cycle")
  ("C-d" rebox-dwim "dwim")
  ("C-f" rebox-fill "fill")
  ("M-q" nil "quit"))

(global-set-key (kbd "C-c ; m") 'hydra-rebox/body)
(global-set-key (kbd "C-c ; l") 'comment-line)


(provide 'comment-config)
;;; comment-config.el ends here
