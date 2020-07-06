;;; hideshow-config.el --- Configure and improve hideshow

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'hideshow
;;   (require 'hideshow-config))
;; never:
;; (require 'hideshow-config)

;; Do not include in this file:
;; (require 'hideshow-mode)

;;; Code:

(message "Importing hideshow-config")

(setcar (cdr (assq 'hs-minor-mode minor-mode-alist)) nil)

(with-eval-after-load 'tex-mode
  (add-hook 'latex-mode-hook #'hs-minor-mode)
  (define-key latex-mode-map (kbd "C-+") #'hs-toggle-hiding)
  (add-hook 'tex-mode-hook #'hs-minor-mode)
  (define-key tex-mode-map (kbd "C-+") #'hs-toggle-hiding))
(with-eval-after-load 'latex
  (add-hook 'LaTeX-mode-hook #'hs-minor-mode)
  (define-key LaTeX-mode-map (kbd "C-+") #'hs-toggle-hiding))
(with-eval-after-load 'tex
  (add-hook 'TeX-mode-hook #'hs-minor-mode)
  (define-key TeX-mode-map (kbd "C-+") #'hs-toggle-hiding))


(provide 'hideshow-config)
;;; hideshow-config.el ends here
