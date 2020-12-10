;;; which-key-config.el --- Configure which key

;;; Commentary:

;; Usage:
;; (require 'which-key-config)

;;; Code:

(require 'which-key)

(setq which-key-lighter ""
      ;; Allow C-h to trigger which-key before it is done automatically
      which-key-show-early-on-C-h t
      ;; Set the time delay (in seconds) for the which-key popup to appear.
      which-key-idle-delay 1.0

      ;; Set the maximum length (in characters) for key descriptions (commands or
      ;; prefixes). Descriptions that are longer are truncated and have ".." added.
      which-key-max-description-length 30

      ;; Use additonal padding between columns of keys. This variable specifies the
      ;; number of spaces to add to the left of each column.
      which-key-add-column-padding 0

      ;; Set the separator used between keys and descriptions. Change this setting to
      ;; an ASCII character if your font does not show the default arrow. The second
      ;; setting here allows for extra padding for Unicode characters. which-key uses
      ;; characters as a means of width measurement, so wide Unicode characters can
      ;; throw off the calculation.
      which-key-separator " → "
      which-key-unicode-correction 3

      ;; Set the prefix string that will be inserted in front of prefix commands
      ;; (i.e., commands that represent a sub-map).
      which-key-prefix-prefix "+"

      ;; Set to t to show the count of keys shown vs. total keys in the mode line.
      which-key-show-remaining-keys nil
      ;; which-key-special-keys nil
      ;; Location
      which-key-popup-type 'side-window
      which-key-side-window-location '(right bottom)
      which-key-show-prefix 'top)

(if (display-graphic-p)
    (progn
      (push '(("ESC" . nil) . ("⎋" . nil)) which-key-replacement-alist)
      (push '(("TAB" . nil) . ("↹" . nil)) which-key-replacement-alist)
      (push '(("RET" . nil) . ("↵" . nil)) which-key-replacement-alist)
      (push '(("DEL" . nil) . ("⇤" . nil)) which-key-replacement-alist)
      (push '(("SPC" . nil) . ("␣" . nil)) which-key-replacement-alist)
      (setq which-key-special-keys '("⎋" "↹" "↵" "⇤" "␣"))
      (set-face-attribute 'which-key-special-key-face nil
                      :bold t
                      :inverse-video 'unspecified
                      :inherit 'unspecified
                      :foreground "#78e56d")
      (when (require 'which-key-posframe nil t)
        (setq which-key-posframe-poshandler 'posframe-poshandler-frame-center)
        (which-key-posframe-mode)))
  ;; Set the special keys. These are automatically truncated to one character and
  ;; have which-key-special-key-face applied. Disabled by default. An example
  ;; setting is
  (setq which-key-special-keys '("SPC" "TAB" "RET" "ESC" "DEL"))
  (set-face-attribute 'which-key-special-key-face nil
                      :foreground "#78e56d"))

(global-set-key (kbd "C-h C-h") 'which-key-show-top-level)
(which-key-mode)


(provide 'which-key-config)
;;; which-key-config.el ends here
