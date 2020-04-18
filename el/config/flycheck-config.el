;;; flycheck-config.el --- Configure flycheck and improve it

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'flycheck
;;   (require 'flycheck-config))
;; or:
;; (with-eval-after-load 'flycheck-config
;;   )
;; never:
;; (require 'flycheck-config)

;; Do not include in this file:
;; (require 'flycheck)

;;; Code:

(message "Importing flycheck-config")
;;;;;;;;;;;
;; Julia ;;
;;;;;;;;;;;
(with-eval-after-load 'julia-mode
  (require 'flycheck-julia)
  (flycheck-julia-setup))
;;;;;;;;;;;;;;
;; Flycheck ;;
;;;;;;;;;;;;;;
;; Enable flycheck globaly
;;(add-hook 'after-init-hook #'global-flycheck-mode)
;; Enable flycheck localy
;;(add-hook 'prog-mode-hook 'flycheck-mode)
(add-hook 'c++-mode-hook
          (lambda ()
            (setq flycheck-gcc-language-standard "c++11"
                  flycheck-clang-language-standard "c++11")))

(setq flycheck-check-syntax-automatically nil ;; value without lsp-mode: '(save mode-enabled)
      ;; sudo apt install php-codesniffer
      flycheck-phpcs-standard "PSR2")

;; (require 'semantic)
;; (setq flycheck-clang-system-path (list))
;; (require 'semantic/bovine/gcc)
;; (let ((dirs (semantic-gcc-get-include-paths "c++")))
;;     (dolist (dir dirs)
;;       (add-to-list 'flycheck-clang-system-path dir)))

;; ;; Disable clang check, gcc check works better
;; (setq-default flycheck-disabled-checkers
;;               (append flycheck-disabled-checkers
;;                       '(c/c++-clang)))
(with-eval-after-load 'c-c++-config
  (dolist (path c-c++-include-paths)
    (add-to-list 'flycheck-gcc-include-path path)
    (add-to-list 'flycheck-clang-include-path path)))

;; hide 'In included' messages
(defconst flycheck-fold-include-levels-include
  (symbol-function 'flycheck-fold-include-levels))

(defun flycheck-fold-include-levels-exclude (errors sentinel-message)
  "Exclude ERRORS with SENTINEL-MESSAGE from included files."
  (unless (or (stringp sentinel-message) (functionp sentinel-message))
    (error "Sentinel must be string or function: %S" sentinel-message))
  (let ((sentinel (if (functionp sentinel-message)
                      sentinel-message
                    (lambda (err)
                      (string-match-p sentinel-message
                                      (flycheck-error-message err))))))
    (setq errors (cl-remove-if sentinel errors)))
  errors)
(defconst flycheck-fold-include-levels-exclude
  (symbol-function 'flycheck-fold-include-levels-exclude))

(defun flycheck-toggle-includes ()
  "Toggle errors in included files."
  (interactive)
  (if (eq (symbol-function 'flycheck-fold-include-levels)
          (indirect-function flycheck-fold-include-levels-include))
      (fset 'flycheck-fold-include-levels flycheck-fold-include-levels-exclude)
    (fset 'flycheck-fold-include-levels flycheck-fold-include-levels-include))
  (flycheck-buffer))

;; warning options
(defun flycheck-toggle-warnings ()
  "Toggle warnings."
  (interactive)
  (if (member "extra" flycheck-clang-warnings)
      (delete "extra" flycheck-clang-warnings)
    (add-to-list 'flycheck-clang-warnings "extra"))
  (if (member "extra" flycheck-gcc-warnings)
      (delete "extra" flycheck-gcc-warnings)
    (add-to-list 'flycheck-gcc-warnings "extra"))
  (flycheck-buffer))

(when (member "extra" flycheck-clang-warnings)
  (delete "extra" flycheck-clang-warnings))
(when (member "extra" flycheck-gcc-warnings)
  (delete "extra" flycheck-gcc-warnings))

;; Mode-line
(when (bug-check-function-bytecode
       'flycheck-mode-line-status-text
       "iYYFAAiJw7eCTgDEgk8AxYJPAMaCTwDHgk8AyAkhyQGeQcoCnkEBhC4AiYM+AMvMA4Y1AM0DhjoAzSOCPwDEtoKyAYJPAM6CTwDPgk8A0LIB0QoCUYc=")
  (defun flycheck-mode-line-status-text (&optional status)
    "Get a text describing STATUS for use in the mode line.

STATUS defaults to `flycheck-last-status-change' if omitted or
nil."
    (pcase (or status flycheck-last-status-change)
      ('not-checked '(:propertize "{}" face mode-line-inactive))
      ('no-checker '(:propertize "{∅}" face mode-line-notready))
      ('running '(:propertize "{↻}" face mode-line-correct))
      ('errored '(:propertize "{✘}" face mode-line-error))
      ('finished `((:propertize "{")
                   ,@(let-alist (flycheck-count-errors flycheck-current-errors)
                       (let (accumulate)
                         (if .warning (push `(:propertize ,
                                              (format "⚠%d" .warning)
                                              face flycheck-error-list-warning)
                                            accumulate))
                         (if .error (push `(:propertize
                                            ,(format "⚐%d" .error)
                                            face flycheck-error-list-error)
                                          accumulate))
                         (or accumulate '((:propertize
                                           "✓ "
                                           face flycheck-error-list-info)))))
                   (:propertize "}")))
      ('interrupted '(:propertize "{.}" face mode-line-error))
      ('suspicious '(:propertize "{?}" face mode-line-warning)))))

;;;;;;;;;;;;;;
;; Posframe ;;
;;;;;;;;;;;;;;
(when (and (display-graphic-p) (load "flycheck-posframe" t))
  (add-hook 'flycheck-mode-hook #'flycheck-posframe-mode))

;; Keys
(define-key flycheck-mode-map (kbd "C-c ! t w") 'flycheck-toggle-warnings)
(define-key flycheck-mode-map (kbd "C-c ! t i") 'flycheck-toggle-includes)
(define-key flycheck-mode-map (kbd "M-g n") #'flycheck-next-error)
(define-key flycheck-mode-map (kbd "M-g M-n") #'flycheck-next-error)
(define-key flycheck-mode-map (kbd "M-g p") #'flycheck-previous-error)
(define-key flycheck-mode-map (kbd "M-g M-p") #'flycheck-previous-error)


(provide 'flycheck-config)
;;; flycheck-config.el ends here
