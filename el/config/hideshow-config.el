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

(defface hideshow-overlay-face
  '((t (:foreground "purple" :box t)))
  "HideShow overlay face"
  :group 'hideshow)

(setq minor-mode-alist (assq-delete-all 'hs-minor-mode minor-mode-alist)
      hs-set-up-overlay
      (lambda (ov)
        (when (eq 'code (overlay-get ov 'hs))
          (overlay-put ov 'display
                       (propertize
                        (format "… %d lines"
                                (count-lines (overlay-start ov)
                                             (overlay-end ov)))
                        'face 'hideshow-overlay-face)))))

(with-eval-after-load 'tex-mode
  (add-hook 'latex-mode-hook #'hs-minor-mode)
  (add-hook 'tex-mode-hook #'hs-minor-mode))
(with-eval-after-load 'latex
  (add-hook 'LaTeX-mode-hook #'hs-minor-mode))
(with-eval-after-load 'tex
  (add-hook 'TeX-mode-hook #'hs-minor-mode))

;; (define-key hs-minor-mode-map (kbd "<C-tab>") #'hs-toggle-hiding)


(provide 'hideshow-config)
;;; hideshow-config.el ends here
