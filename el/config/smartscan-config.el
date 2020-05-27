;;; smartscan-config.el --- Configure smartscan

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'smartscan
;;   (require 'smartscan-config))

;;; Code:
(require 'config-lib)
(advice-add 'smartscan-symbol-goto :around #'message-silent-advice)
(setq smartscan-symbol-selector "symbol")

(defun smartscan-symbol-go-forward (arg)
  "Jumps forward to the next symbol at point"
  (interactive "P")
  (smartscan-symbol-goto (if arg
                             smartscan-last-symbol-name
                           (smartscan-symbol-at-pt 'end)) 'forward))

;;;###autoload
(defun smartscan-symbol-go-backward (arg)
  "Jumps backward to the previous symbol at point"
  (interactive "P")
  (smartscan-symbol-goto (if arg
                             smartscan-last-symbol-name
                           (smartscan-symbol-at-pt 'beginning)) 'backward))

(define-key smartscan-map (kbd "M-n") nil)
(define-key smartscan-map (kbd "C-c C-n") 'smartscan-symbol-go-forward)
(define-key smartscan-map (kbd "M-p") nil)
(define-key smartscan-map (kbd "C-c C-p") 'smartscan-symbol-go-backward)
(define-key smartscan-map (kbd "M-'") nil)
(define-key smartscan-map (kbd "C-c C-r") 'smartscan-symbol-replace)

(global-set-key (kbd "<f5>") 'smartscan-mode)


(provide 'smartscan-config)
;;; smartscan-config.el ends here
