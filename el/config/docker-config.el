;;; docker-config.el --- Configure and improve docker

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'magit-popup
;;   (require 'docker-config))

;; Do not include in this file:
;; (require 'docker)
;; or
;; (require 'magit-popup)

;;; Code:

(message "Importing docker-config")

(magit-define-popup docker-container-logs-popup
  "Popup for showing containers logs."
  'docker-container
  :man-page "docker-logs"
  :switches '((?f "Follow" "-f") (?t "Timestamps" "-t"))
  :options  '((?T "Tail" "--tail "))
  :actions  '((?L "Logs" docker-container-logs-selection))
  :default-arguments '("-f" "-t")
  :setup-function #'docker-utils-setup-popup)


(provide 'docker-config)
;;; docker-config.el ends here
