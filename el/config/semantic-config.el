;;; semantic-config.el --- Configure and improve semantic

;;; Commentary:

;; Usage:
;; (require 'semantic-config)

;;; Code:

(message "Importing semantic-config")
;; [ Included by default
;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
;; (add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
;; ]
(require 'semantic)
;;(setq-default semantic-symref-tool "grep")
(setq semantic-default-submodes '(semantic-tag-folding-mode
                                  semantic-mru-bookmark-mode
                                  semantic-stickyfunc-mode
                                  semantic-idle-scheduler-mode
                                  semanticdb-minor-mode)
      semantic-stickyfunc-sticky-classes '(function type)
      ;; semantic-symref-tool "grep"
      ;; semantic-decoration-styles
      ;; '(("semantic-decoration-on-includes" . t)
      ;;   ("semantic-decoration-on-protected-members" . t)
      ;;   ("semantic-decoration-on-private-members" . t)
      ;;   ("semantic-tag-boundary" . t))
      semantic-idle-scheduler-idle-time 3)

;; Disabled, completions by company
;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode)
;; First line show current function
;;(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
;; Most Recently Used tags
;;(add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
;; Highlight symbol under cursor in page
;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-local-symbol-highlight-mode)
;;(add-to-list 'semantic-default-submodes 'global-semantic-tag-folding-mode)
;; (add-to-list 'semantic-default-submodes 'global-semantic-decoration-mode)
;; Smart autocomplete
(require 'semantic/ia)
;; SpeedBar
(require 'semantic/sb)
;; (require 'semantic/wisent)
;; (require 'semantic/symref)
;; Include semantic-symref-results-mode-map and related
(require 'semantic/symref/list)
;; [ Autocomplete with gcc headers
;; inside company-extensions-config.el
;; (require 'semantic/bovine/gcc)
;; ]
;; Not found
;;(require 'semantic/bovine/clang)
;; After 'require' and after update 'semantic-default-submodes' list
;;(semantic-mode 1)

;; Autocompletado usando los .h incluidos de librerías personales
;; [ Extremadamente lento para proyectos grandes
;; (semantic-add-system-include "~/Prog/c/lib" 'c++-mode)
;; ]
;; Java autocomplete
;;(require 'semantic/db-javap)
;;(require 'semantic-bug)
(require 'semantic-parse-dir)

;;;;;;;;;;;;;;;
;; Functions ;;
;;;;;;;;;;;;;;;
(defun semantic-complete-jump-at-point (point)
  "Find definition/declaration of symbol at POINT.
Improve default ia jump at point."
  (interactive "d")
  (let* ((ctxt (semantic-analyze-current-context point))
         (pf (and ctxt (reverse (oref ctxt prefix))))
         (first-tag (car pf)))
    (if (semantic-tag-p first-tag)
        (semantic-ia--fast-jump-helper first-tag)
      (progn
        (semantic-error-if-unparsed)
        (let* ((tag (semantic-complete-read-tag-project "Jump to symbol: " first-tag first-tag)))
          (when (semantic-tag-p tag)
            (push-mark nil t)
            (semantic-go-to-tag tag)
            (switch-to-buffer (current-buffer))
            (semantic-momentary-highlight-tag tag)
            (message "%S: %s "
                     (semantic-tag-class tag)
                     (semantic-tag-name  tag))))))))

;;;;;;;;;;
;; Keys ;;
;;;;;;;;;;
;; (bind-keys :map semantic-mode-map
;;            ([(control return)] . semantic-ia-complete-symbol)
;;            ("C-c , a" . semantic-complete-analyze-inline)
;;            ("C-c , s" . semantic-ia-show-summary)
;;            ("C-c , d" . semantic-ia-show-doc)
;;            ("C-c , c" . semantic-ia-describe-class)
;;            ("M-," . semantic-analyze-proto-impl-toggle) ; "C-c , p"
;;            ("M-." . ido-semantic-complete-jump) ; "C-c , j"
;;            ("M--" . semantic-analyze-possible-completions)
;;            ("M-Ç" . semantic-symref)
;;            ("M-ç" . semantic-symref-symbol)
;;            ;; senator
;;            ("C-c , +" . senator-fold-tag)
;;            ("C-c , -" . senator-unfold-tag)
;;            ("C-c , ." . senator-fold-tag-toggle))
(define-key semantic-symref-results-mode-map "e" #'semantic-symref-list-expand-all)
(define-key semantic-symref-results-mode-map "c" #'semantic-symref-list-contract-all)

;;(semantic-mode 1)

(require 'srecode)
;;(global-srecode-minor-mode 1)


(provide 'semantic-config)
;;; semantic-config.el ends here
