;;; javascript-config.el --- Configure and improve javascript

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'javascript-mode
;;   (require 'javascript-config))
;; or:
;; (with-eval-after-load 'javascript-config
;;   )
;; never:
;; (require 'javascript-config)

;; Do not include in this file:
;; (require 'javascript-mode)

;;; Code:

(message "Importing javascript-config")

;; Better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

(when (load "js2-refactor" t)
  (js2r-add-keybindings-with-prefix "C-c C-r")
  (define-key js2-mode-map (kbd "C-k") #'js2r-kill)

  ;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
  ;; unbind it.
  (define-key js-mode-map (kbd "M-.") nil)
)

(when (load "xref-js2" t)
  (add-hook 'js2-mode-hook (lambda ()
                             (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))

(when (load "company-tern" t)
  (add-to-list 'company-backends 'company-tern)
                           
  ;; Disable completion keybindings, as we use xref-js2 instead
  (define-key tern-mode-keymap (kbd "M-.") nil)
  (define-key tern-mode-keymap (kbd "M-,") nil))


(provide 'javascript-config)
;;; javascript-config.el ends here
