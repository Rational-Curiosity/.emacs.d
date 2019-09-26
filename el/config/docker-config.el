;;; docker-config.el --- Configure and improve docker

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'docker
;;   (require 'docker-config))

;; Do not include in this file:
;; (require 'docker)

;;; Code:

(require 'magit-popup)
(message "Importing docker-config")

;; (with-eval-after-load 'docker-container
(setq docker-container-logs-arguments '("-f" "-t" "--tail=150")
      docker-container-logs-popup
      (list :variable 'docker-container-logs-arguments
            :man-page "docker-logs"
            :switches '((?f "Follow" "-f") (?t "Timestamps" "-t"))
            :options  '((?T "Tail" "--tail="))
            :actions  '((?L "Logs" docker-container-logs-selection))
            :default-arguments '("-f" "-t" "--tail=150")
            :setup-function #'docker-utils-setup-popup))
(magit-define-popup docker-container-logs-popup
  "Popup for showing containers logs."
  'docker-container
  :man-page "docker-logs"
  :switches '((?f "Follow" "-f") (?t "Timestamps" "-t"))
  :options  '((?T "Tail" "--tail="))
  :actions  '((?L "Logs" docker-container-logs-selection))
  :default-arguments '("-f" "-t" "--tail=150")
  :setup-function #'docker-utils-setup-popup)
;; )

;;;;;;;;;;
;; Keys ;;
;;;;;;;;;;
;; (dolist (map-symbol '(docker-container-mode-map
;;                       docker-image-mode-map
;;                       docker-machine-mode-map
;;                       docker-network-mode-map
;;                       docker-volume-mode-map))
;;   (let ((map (eval map-symbol)))
;;     ;; (with-eval-after-load (intern (replace-regexp-in-string "-mode-map" "" (symbol-name map-symbol) t 'literal))
;;     (modal-add-first-parent map)
;;     ;; )
;;     ))


(provide 'docker-config)
;;; docker-config.el ends here
