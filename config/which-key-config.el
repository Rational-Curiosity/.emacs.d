;;; which-key-config.el --- Configure which key

;;; Commentary:

;; Usage:
;; (require 'which-key-config)

;;; Code:

(require 'which-key)
(which-key-mode)

(which-key-setup-side-window-right-bottom)

;; Set the time delay (in seconds) for the which-key popup to appear.
(setq which-key-idle-delay 1.0)

;; Set the maximum length (in characters) for key descriptions (commands or
;; prefixes). Descriptions that are longer are truncated and have ".." added.
(setq which-key-max-description-length 21)

;; Use additonal padding between columns of keys. This variable specifies the
;; number of spaces to add to the left of each column.
(setq which-key-add-column-padding 0)

;; Set the separator used between keys and descriptions. Change this setting to
;; an ASCII character if your font does not show the default arrow. The second
;; setting here allows for extra padding for Unicode characters. which-key uses
;; characters as a means of width measurement, so wide Unicode characters can
;; throw off the calculation.
(setq which-key-separator " → " )
(setq which-key-unicode-correction 3)

;; Set the prefix string that will be inserted in front of prefix commands
;; (i.e., commands that represent a sub-map).
(setq which-key-prefix-prefix "+" )

;; Set the special keys. These are automatically truncated to one character and
;; have which-key-special-key-face applied. Disabled by default. An example
;; setting is
(setq which-key-special-keys '("SPC" "TAB" "RET" "ESC" "DEL"))
;; (setq which-key-special-keys nil)

(provide 'which-key-config)
;;; which-key-config.el ends here
