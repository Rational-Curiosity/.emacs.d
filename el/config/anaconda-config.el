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


;;;; Install python dependencies
;;;;;; Python3
;; sudo pip3 install flake8
;; sudo pip3 install jedi
;; sudo pip3 install pylint
;; sudo pip3 install mypy
;; sudo pip3 install rope_py3k
;; sudo pip3 install importmagic
;; sudo pip3 install autopep8
;; sudo pip3 install yapf
;;;;;; Python
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
(require 'config-lib)
(message "Importing anaconda-config")

(defun set-anaconda-mode-lighter ()
  (if (string-match-p "[^0-9]3[.0-9]*$" python-shell-interpreter)
      (setq anaconda-mode-lighter "A3")
    (setq anaconda-mode-lighter "A2")))
(set-anaconda-mode-lighter)

(advice-add 'set-python-interpreter-args :after #'set-anaconda-mode-lighter)

;; Limit python processes
(let ((elap (* 10 60)))
  (processes-run-with-timer-cond-body elap elap '("python" "python3") processes
                                      (< 10 processes)
    (anaconda-mode-stop)
    (message "anaconda-config: %i pythons found (>10) stopped at %s"
             processes (format-time-string "%Y-%m-%d %H:%M:%S.%N"))))

(defun anaconda-mode-show-xrefs-advice (result display-action error-message)
  (unless result
    (pcase error-message
      ("No definitions found" (call-interactively 'xref-find-definitions))
      ("No references found" (call-interactively 'xref-find-references)))))
(advice-add 'anaconda-mode-show-xrefs :after 'anaconda-mode-show-xrefs-advice)


;;;;;;;;;;
;; Keys ;;
;;;;;;;;;;
(defhydra hydra-anaconda (:foreign-keys run :hint nil)
  "
^Find^             ^Other window^  ^Other frame
^^^^^^^^----------------------------------------
definitions: _._   _d_             _D_
references:  _,_   _r_             _R_
assignments: _=_   _a_             _A_
show doc:    _s_
"
  ("." anaconda-mode-find-definitions)
  ("d" anaconda-mode-find-definitions-other-window)
  ("D" anaconda-mode-find-definitions-other-frame)
  ("," anaconda-mode-find-references)
  ("r" anaconda-mode-find-references-other-window)
  ("R" anaconda-mode-find-references-other-frame)
  ("=" anaconda-mode-find-assignments)
  ("a" anaconda-mode-find-assignments-other-window)
  ("A" anaconda-mode-find-assignments-other-frame)
  ("s" anaconda-mode-show-doc)
  ("M-q" nil "quit"))

(define-key anaconda-mode-map (kbd "M-r") nil)
(define-key anaconda-mode-map (kbd "C-:") 'hydra-anaconda/body)


(provide 'anaconda-config)
;;; anaconda-config.el ends here
