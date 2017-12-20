;;; ispell-config.el --- Configure ispell

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'ispell
;;   (require 'ispell-config))
;; or:
;; (with-eval-after-load 'ispell-config
;;   )
;; never:
;; (require 'ispell-config)

;; Do not include in this file:
;; (require 'ispell)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ispell configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;
;; Select checker ;;
;;;;;;;;;;;;;;;;;;;;
(message "Importing ispell-config")

(let ((executable (file-name-nondirectory ispell-program-name)))
  (cond
   ((string-equal executable "aspell")
    (setq ispell-really-aspell t
          ispell-personal-dictionary (file-truename "~/.emacs.d/cache/aspell.spanish.pws"))
    (add-to-list 'ispell-extra-args "--sug-mode=ultra"))
   ((string-equal executable "hunspell")
    (setenv "DICPATH" (file-truename "~/.emacs.d/cache/hunspell/"))
    (add-to-list 'ispell-local-dictionary-alist `("es_ES"
                                                  "[a-zA-ZáéíóúÁÉÍÓÚÑüÜ]"
                                                  "[^a-zA-ZáéíóúÁÉÍÓÚÑüÜ]"
                                                  "[']"
                                                  t
                                                  ("-d" "es_ES")
                                                  nil
                                                  utf-8))
    (setq ispell-really-hunspell t
          ispell-personal-dictionary (file-truename "~/.emacs.d/cache/hunspell.spanish.pws")
          ispell-current-dictionary "es_ES"
          ispell-local-dictionary "es_ES"
          ispell-dictionary "es_ES")
    ;; Internal use
    ;; (add-to-list 'ispell-hunspell-dict-paths-alist `("spanish" ,(file-truename "~/.emacs.d/cache/hunspell/es_ES.aff")))
    )))


(provide 'ispell-config)
;;; ispell-config.el ends here
