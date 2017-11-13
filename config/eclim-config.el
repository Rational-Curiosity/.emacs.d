;;; eclim-config.el --- Configure eclim for java

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'eclim
;;   (require 'eclim-config))

;;; Code:
(message "Importing eclim-config")

(setq eclimd-autostart t
      emacs-eclim-ignore-case t
      eclim-eclipse-dirs '("/cygdrive/c/WrkFld/Portable/eclipse")
      eclim-executable "/cygdrive/c/WrkFld/Portable/eclipse/eclim.bat"
      eclimd-default-workspace "/cygdrive/c/WrkFld/ChkOut_henriquez/reports")

(defun my-java-mode-hook ()
    (eclim-mode t))

(add-hook 'java-mode-hook 'my-java-mode-hook)

(defun eclimd--executable-path-advice (orig-fun &rest args)
  (concat (apply orig-fun args) ".bat"))
(advice-add 'eclimd--executable-path :around #'eclimd--executable-path-advice)


(provide 'eclim-config)
;;; eclim-config.el ends here
