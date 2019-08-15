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

(with-eval-after-load 'docker-container
  (setq docker-container-logs-popup
        (list :variable docker-container-logs-arguments
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
    :setup-function #'docker-utils-setup-popup))

;;;;;;;;;;
;; Keys ;;
;;;;;;;;;;
(dolist (map-symbol '(docker-container-mode-map
                      docker-image-mode-map
                      docker-machine-mode-map
                      docker-network-mode-map
                      docker-volume-mode-map))
  (let ((map (eval map-symbol)))
    (with-eval-after-load (intern (replace-regexp-in-string "-mode-map" "" (symbol-name map-symbol) t 'literal))
      (define-key map (kbd "M-g c") #'avy-goto-char)
      (define-key map (kbd "M-g C") #'avy-goto-char-2)
      (define-key map (kbd "M-g s") #'avy-goto-char-timer)
      (define-key map (kbd "M-g l") #'avy-goto-line)
      (define-key map (kbd "M-g w") #'avy-goto-word-1)
      (define-key map (kbd "M-g W") #'avy-goto-word-0)
      (define-key map (kbd "z")     #'avy-goto-char-timer)
      (define-key map (kbd "M-g k") #'link-hint-open-link)
      (define-key map (kbd "M-g K") #'link-hint-copy-link))))


(provide 'docker-config)
;;; docker-config.el ends here
