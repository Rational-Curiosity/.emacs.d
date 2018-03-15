(require 'sublimity)
(require 'sublimity-scroll)
(require 'sublimity-map)
(require 'sublimity-attractive)

(sublimity-mode 1)


(setq sublimity-scroll-weight 10
      sublimity-scroll-drift-length 5)

(setq sublimity-map-size 20
      sublimity-map-fraction 0.3
      sublimity-map-text-scale -7)

(add-hook 'sublimity-map-setup-hook
          (lambda ()
            (setq buffer-face-mode-face '(:family "Monospace"))
            (buffer-face-mode)))

(sublimity-map-set-delay nil)




(defun prueba ()
  "No tocar pelotillas."
  (insert "tomorow")
  (if t
      (message "godo")))



(provide 'sublimity-config)