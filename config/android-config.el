;;; android-config.el --- Configure android mode

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'android-mode
;;   (require 'android-config))
;; or:
;; (with-eval-after-load 'android-mode
;;   )
;; never:
;; (require 'android-config)

;; Do not include in this file:
;; (require 'android-mode)

;;; Code:

(message "Importing android-config")
;(add-to-list 'load-path "~/.emacs.d/elpa/android-mode-0.4.0")
(set 'android-mode-sdk-dir "~/Prog/java/android/sdk")
;;(set 'android-mode-builder 'gradle)
(set 'android-mode-root-file-plist '(ant "AndroidManifest.xml"
                                         maven  "AndroidManifest.xml"
                                         gradle "gradlew"))


(provide 'android-config)
;;; android-config.el ends here
