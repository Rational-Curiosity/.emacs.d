;;; plantuml-config.el --- Configure plantuml

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'plantuml-mode
;;   (require 'python-config))
;; or:
;; (with-eval-after-load 'plantuml-config
;;   )
;; never:
;; (require 'plantuml-config)

;; Do not include in this file:
;; (require 'plantuml-mode)

;;; Code:

(message "Importing plantuml-mode")
(setq plantuml-jar-path "~/bin/plantuml.jar")


(provide 'plantuml-config)
;;; plantuml-config.el ends here
