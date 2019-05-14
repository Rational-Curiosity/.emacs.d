;;; term-config.el --- Configure and improve term

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'term-mode
;;   (require 'term-config))
;; or:
;; (with-eval-after-load 'term-config
;;   )
;; never:
;; (require 'term-config)

;; Do not include in this file:
;; (require 'term-mode)

;;; Code:

(setq xterm-color-preserve-properties t)
(require 'xterm-color)

(if (daemonp)
    (setenv "EDITOR" "emacsclient -c -n")
  (setenv "EDITOR" "emacs"))
;; (setenv "PAGER" "cat")


(provide 'term-config)
;;; term-config.el ends here
