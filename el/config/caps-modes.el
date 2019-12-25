;;; caps-modes.el --- Configure and improve python

;;; Commentary:

;; Usage:
;; (require 'caps-modes)

;;; Code:

;;;;;;;;;;;;;;;
;; Show caps ;;
;;;;;;;;;;;;;;;
(require 'dash)
(require 's)

(defun x-led-mask ()
  "Get the current status of the LED mask from X."
  (with-temp-buffer
    (call-process "xset" nil t nil "q")
    (let ((led-mask-string
           (->> (buffer-string)
                s-lines
                (--first (s-contains? "LED mask" it))
                s-split-words
                -last-item)))
      (string-to-number led-mask-string 16))))

(defun caps-lock-on (led-mask)
  "Return non-nil if LED-MASK means caps lock is on."
  (eq (logand led-mask 1) 1))

(define-minor-mode caps-lock-show-mode
  "Display whether caps lock is on."
  :global t
  :lighter (:propertize "⇪" font-lock-face
                        (:foreground "violet" :weight bold))
  (if caps-lock-show-mode
      (set-cursor-color "violet")
    (set-cursor-color "red")))

;;;;;;;;;;;;;;;;
;; Force caps ;;
;;;;;;;;;;;;;;;;
(defun caps-find-bind (key)
  ;; (message "active maps: %s" (mapcar 'keymap-symbol (current-active-maps t)))
  (cl-some (lambda (keymap)
             ;; (message "looking keymap: `%s'" (or (keymap-symbol keymap) keymap))
             (unless (eq keymap modal-mode-map)
               ;; (message "keymap accepted")
               (let ((binding (lookup-key keymap key)))
                 (if (commandp binding)
                     ;; (progn
                     ;;   (message "bind `%s' found in keymap: `%s'" binding (keymap-symbol keymap))
                     binding
                     ;;   )
                   ))))
           (current-active-maps)))

(defun caps-lock--upcase ()
  ;; (message "last-command-event: %s" last-command-event)
  (when (and (characterp last-command-event)
             (< last-command-event 123)
             (< 96 last-command-event))
    (setq last-command-event (upcase last-command-event))
    (unless isearch-mode
      (let ((binding (caps-find-bind (vector last-command-event))))
        (if binding
            (setq real-this-command binding
                  this-original-command binding
                  this-command binding))))))

(defvar caps--post-command-countdown nil)

(define-minor-mode caps-lock-mode
  "Make self-inserting keys invert the capitalization."
  :global t
  :lighter (:propertize "⇪" font-lock-face
                        (:foreground "red" :weight bold))
  (if caps-lock-mode
      (progn
        (when caps--post-command-countdown
          (remove-hook 'post-command-hook 'caps--enable-mode-and-remove-from-hook)
          (setq caps--post-command-countdown nil))
        (add-hook 'pre-command-hook 'caps-lock--upcase))
    (when caps--post-command-countdown
      (remove-hook 'post-command-hook 'caps--disable-mode-and-remove-from-hook)
      (setq caps--post-command-countdown nil))
    (remove-hook 'pre-command-hook 'caps-lock--upcase)))

(defun caps--enable-mode-and-remove-from-hook ()
  (if (< 0 caps--post-command-countdown)
      (cl-decf caps--post-command-countdown)
    (caps-lock-mode 1)))

(defun caps--disable-mode-and-remove-from-hook ()
  (if (< 0 caps--post-command-countdown)
      (cl-decf caps--post-command-countdown)
    (caps-lock-mode 0)))

(defun caps-lock-mode-post-command (times)
  (interactive "p")
  (if caps--post-command-countdown
      (setq caps--post-command-countdown (+ caps--post-command-countdown times 1))
    (when (and (numberp times)
               (< 0 times))
      (if caps-lock-mode
          (progn
            (caps-lock-mode 0)
            (add-hook 'post-command-hook 'caps--enable-mode-and-remove-from-hook))
        (caps-lock-mode 1)
        (add-hook 'post-command-hook 'caps--disable-mode-and-remove-from-hook))
      (setq caps--post-command-countdown times))))


(provide 'caps-modes)
;;; caps-modes.el ends here
