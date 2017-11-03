;;; package-config.el --- Configure company

;;; Commentary:

;;; Code:
(require 'package)
(require 'config-lib)

;; avoid automatic startup
(setq package-enable-at-startup nil)
;; [ <repos> configure repositories
;; (add-to-list 'package-archives '("ELPA"         . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("org"          . "http://orgmode.org/elpa/"))

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (urls '("://melpa.org/packages/"
               "://stable.melpa.org/packages/"
               "://marmalade-repo.org/packages/"))
       (names '("melpa" "melpa-stable" "marmalade"))
       (urls (mapcar (lambda (s) (concat (if no-ssl "http" "https") s)) urls)))
  (cl-mapcar (lambda (n u) (add-to-list 'package-archives (cons n u) t))
             names urls))

;; (when (< emacs-major-version 24)
;;   ;; For important compatibility libraries like cl-lib
;;   (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
;; ] <repos>
;; remove duplicates not necesary with add-to-list
;; (delete-dups  package-archives)
;; manual startup
(package-initialize)

;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))

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
  (eval-and-when-daemon nil
    (package-autoremove)))

;; Se evalua al compilar pero no cuando corre el programa compilado
;; (eval-when-compile
;;   (require 'use-package))


(provide 'package-config)
;;; package-config.el ends here
