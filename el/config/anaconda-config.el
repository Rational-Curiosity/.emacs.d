;;; anaconda-config.el --- Configure anaconda

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'anaconda-mode
;;   (require 'anaconda-config))
;; or:
;; (with-eval-after-load 'python
;;   (require 'anaconda-config))
;; never:
;; (require 'anaconda-config)

;; Do not include in this file:
;; (require 'anaconda-mode)


;; Install python dependencies
;;;; Python3
;; sudo pip3 install flake8
;; sudo pip3 install jedi
;; sudo pip3 install pylint
;; sudo pip3 install mypy
;; sudo pip3 install rope_py3k
;; sudo pip3 install importmagic
;; sudo pip3 install autopep8
;; sudo pip3 install yapf
;;;; Python
;; sudo apt install flake8
;; sudo pip install flake8 --upgrade
;; sudo apt install python-jedi
;; sudo pip install jedi --upgrade
;; sudo apt install python-flake8
;; sudo pip install rope
;; sudo pip install importmagic
;; sudo pip install autopep8
;; sudo pip install yapf

;;; Code:

(require 'python-config)
(message "Importing anaconda-config")

(defun set-anaconda-mode-lighter ()
  (if (string-match-p "[^0-9]3[.0-9]*$" python-shell-interpreter)
      (setq anaconda-mode-lighter "A3")
    (setq anaconda-mode-lighter "A2")))
(set-anaconda-mode-lighter)

(advice-add 'set-python-interpreter-args :after #'set-anaconda-mode-lighter)

(define-key anaconda-mode-map (kbd "M-r") nil)


(provide 'anaconda-config)
;;; anaconda-config.el ends here
