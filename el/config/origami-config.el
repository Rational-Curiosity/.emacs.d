;;; origami-config.el --- Configure and improve origami

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'origami-mode
;;   (require 'origami-config))
;; never:
;; (require 'origami-config)

;; Do not include in this file:
;; (require 'origami-mode)

;;; Code:

(message "Importing origami-config")

(set-face-attribute 'origami-fold-replacement-face nil
                    :inherit 'unspecified
                    :underline 'unspecified
                    :weight 'bold
                    :foreground "yellow1"
                    :background "DimGray")

(setq origami-fold-replacement "···")


(define-key origami-mode-map (kbd "<C-tab>") 'origami-recursively-toggle-node)
(define-key origami-mode-map (kbd "C-c <tab> n") 'origami-forward-fold-same-level)
(define-key origami-mode-map (kbd "C-c <tab> N") 'origami-forward-toggle-node)
(define-key origami-mode-map (kbd "C-c <tab> p") 'origami-backward-fold-same-level)
(define-key origami-mode-map (kbd "C-c <tab> a") 'origami-close-all-nodes)
(define-key origami-mode-map (kbd "C-c <tab> A") 'origami-open-all-nodes)
(define-key origami-mode-map (kbd "C-c <tab> s") 'origami-show-only-node)


(provide 'origami-config)
;;; origami-config.el ends here

