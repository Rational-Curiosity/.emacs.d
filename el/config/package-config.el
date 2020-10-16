;;; package-config.el --- package configurations and improvements

;;; Commentary:

;;; Code:
(require 'package)
(require 'config-lib)

;; avoid automatic startup
(setq package-enable-at-startup nil)
;; [ <repos> configure repositories
;; (add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
;; (add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))

(let* ((protocol (if (and (memq system-type '(windows-nt ms-dos))
                          (not (gnutls-available-p)))
                     "http"
                   "https"))
       (repos '(("org"          . "://orgmode.org/elpa/")
                ("melpa"        . "://melpa.org/packages/")
                ("melpa-stable" . "://stable.melpa.org/packages/")
                ;; package name conflict: `project'
                ;; ("marmalade"    . "://marmalade-repo.org/packages/")
                ("emacswiki"    . "://mirrors.tuna.tsinghua.edu.cn/elpa/emacswiki/"))))
  (mapc (lambda (p)
          (add-to-list
           'package-archives
           (cons (car p) (concat protocol (cdr p))) t))
        repos))

(package-initialize)
;; sort package list
(defun package--save-selected-packages-advice (orig-fun value)
  (funcall orig-fun (sort value 'string-lessp)))
(advice-add 'package--save-selected-packages :around #'package--save-selected-packages-advice)

(with-eval-after-load 'custom
  ;; Instalamos los paquetes que falten
  (let ((list-of-boolean (mapcar #'package-installed-p package-selected-packages)))
    (if (cl-every #'identity list-of-boolean)
        (message "Nothing to install")
      (progn
        (package-refresh-contents)
        (let ((list-of-uninstalled '()))
          (cl-mapc #'(lambda (a b)
                       (unless a
                         (set 'list-of-uninstalled (cons b list-of-uninstalled))))
                   list-of-boolean package-selected-packages)
          (mapc #'package-install list-of-uninstalled)))))
  ;; Desinstalamos los que sobran
  ;;(mapc #'package-delete (set-difference package-activated-list package-selected-packages))
  (unless (daemonp)
    (package-autoremove)))

(defun package-emacswiki-update ()
  (interactive)
  ;; bookmark+
  (url-copy-file "https://www.emacswiki.org/emacs/download/bookmark%2b.el"
                 "~/.emacs.d/el/packages/bookmark+/bookmark+.el" t)
  (url-copy-file "https://www.emacswiki.org/emacs/download/bookmark%2b-mac.el"
                 "~/.emacs.d/el/packages/bookmark+/bookmark+-mac.el" t)
  (url-copy-file "https://www.emacswiki.org/emacs/download/bookmark%2b-bmu.el"
                 "~/.emacs.d/el/packages/bookmark+/bookmark+-bmu.el" t)
  (url-copy-file "https://www.emacswiki.org/emacs/download/bookmark%2b-1.el"
                 "~/.emacs.d/el/packages/bookmark+/bookmark+-1.el" t)
  (url-copy-file "https://www.emacswiki.org/emacs/download/bookmark%2b-key.el"
                 "~/.emacs.d/el/packages/bookmark+/bookmark+-key.el" t)
  (url-copy-file "https://www.emacswiki.org/emacs/download/bookmark%2b-lit.el"
                 "~/.emacs.d/el/packages/bookmark+/bookmark+-lit.el" t)
  (url-copy-file "https://www.emacswiki.org/emacs/download/bookmark%2b-doc.el"
                 "~/.emacs.d/el/packages/bookmark+/bookmark+-doc.el" t)
  (url-copy-file "https://www.emacswiki.org/emacs/download/bookmark%2b-chg.el"
                 "~/.emacs.d/el/packages/bookmark+/bookmark+-chg.el" t)
  (byte-recompile-directory "~/.emacs.d/el/packages/bookmark+" 0 t)
  ;; thingatpt+
  (url-copy-file "https://www.emacswiki.org/emacs/download/thingatpt%2b.el"
                 "~/.emacs.d/el/packages/thingatpt+/thingatpt+.el" t)
  (byte-recompile-directory "~/.emacs.d/el/packages/thingatpt+" 0 t))


(provide 'package-config)
;;; package-config.el ends here
