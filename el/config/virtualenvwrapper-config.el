;;; virtualenvwrapper-config.el --- Configure and improve virtualenvwrapper

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'virtualenvwrapper
;;   (require 'virtualenvwrapper-config))

;; Do not include in this file:
;; (require 'virtualenvwrapper)

;;; Code:


(message "Importing virtualenvwrapper-config")

(venv-initialize-interactive-shells) ;; if you want interactive shell support
(venv-initialize-eshell) ;; if you want eshell support
;; note that setting `venv-location` is not necessary if you
;; use the default location (`~/.virtualenvs`), or if the
;; the environment variable `WORKON_HOME` points to the right place
(setq venv-location "~/.virtualenvs")


(provide 'virtualenvwrapper-config)
;;; virtualenvwrapper-config.el ends here
