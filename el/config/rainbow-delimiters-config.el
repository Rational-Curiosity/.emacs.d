;;; rainbow-delimiters-config.el --- Configure rainbow delimiters

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'rainbow-delimiters
;;   (require 'rainbow-delimiters-config))
;; or:
;; (with-eval-after-load 'rainbow-delimiters-config
;;   )
;; never:
;; (require 'rainbow-delimiters-config)

;; Do not include in this file:
;; (require 'rainbow-delimiters)

;;; Code:

(message "Importing rainbow-delimiters-config")
;; [
;; (require 'cl-lib)
;; (require 'color)
;; (cl-loop
;;  for index from 1 to rainbow-delimiters-max-face-count
;;  do
;;  (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
;;    (cl-callf color-saturate-name (face-foreground face) 30)))
;; <xor>
(set-face-attribute 'rainbow-delimiters-depth-1-face nil
                    :foreground "#999999")
(set-face-attribute 'rainbow-delimiters-depth-2-face nil
                    :foreground "#8891ff")
(set-face-attribute 'rainbow-delimiters-depth-3-face nil
                    :foreground "#88fbff")
(set-face-attribute 'rainbow-delimiters-depth-4-face nil
                    :foreground "#f4ff88")
(set-face-attribute 'rainbow-delimiters-depth-5-face nil
                    :foreground "#ff88d6")
(set-face-attribute 'rainbow-delimiters-depth-6-face nil
                    :foreground "#8cff88")
(set-face-attribute 'rainbow-delimiters-depth-7-face nil
                    :foreground "#c088ff")
(set-face-attribute 'rainbow-delimiters-depth-8-face nil
                    :foreground "#ffd488")
(set-face-attribute 'rainbow-delimiters-depth-9-face nil
                    :foreground "#b388ff")

;; ]


(set-face-attribute 'rainbow-delimiters-unmatched-face nil
                    :foreground 'unspecified
                    :inherit 'error
                    :strike-through t)

(provide 'rainbow-delimiters-config)
;;; rainbow-delimiters-config ends here
