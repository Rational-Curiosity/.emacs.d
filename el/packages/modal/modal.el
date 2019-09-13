;;; modal.el --- Easily introduce native modal editing of your own design
;;
;; thanks to: https://github.com/mrkkrp/modalka
;;
;;; Commentary:

;; This is a building kit to help switch to modal editing in Emacs.  The
;; main goal of the package is making modal editing in Emacs as natural and
;; native as possible.

;; List of map options
;; # man xkeyboard-config
;; Keycode meanings
;; # xmodmap -pke

;; * Swap LeftControl and LeftWin
;;   # setxkbmap -option ctrl:swap_lwin_lctl
;; * RightControl as AltGr
;;   # setxkbmap -option lv3:switch
;; * AltGr as control
;;   # xmodmap -e 'keycode 108 = Alt_R'
;;   # xmodmap -e 'add control = Alt_R'
;;   <xor>
;;   # xmodmap -e 'keycode 108 = Control_R'
;;   # xmodmap -e 'add control = Control_R'

;; Hyper_R is free because `xmodmap' don't show it running
;; # xmodmap

;;; Code:
(require 'cl-lib)
(require 'quail)

(defgroup modal nil
  "Introduce native modal editing of your own design"
  :group  'editing
  :tag    "Modal"
  :prefix "modal-"
  :link   '(url-link :tag "GitHub" "https://github.com/mrkkrp/modal"))

(defcustom modal-cursor-type t
  "Cursor type to use in `modal-mode'.

See description of `cursor-type' for mode information, this
variable should follow the same conventions."
  :tag "Cursor Type"
  :type '(choice
          (const :tag "use the cursor specified for the frame" t)
          (const :tag "don't display a cursor" nil)
          (const :tag "display a filled box cursor" box)
          (const :tag "display a hollow box cursor" hollow)
          (const :tag "display a vertical bar cursor with default width" bar)
          (cons  :tag "display a vertical bar cursor with given width"
                 (const bar) (integer :tag "width of cursor"))
          (const :tag "display a horizontal bar cursor with default height" hbar)
          (cons  :tag "display a horizontal bar cursor with given height"
                 (const hbar (integer :tag "height of cursor")))))

(defcustom modal-insert-cursor-type t
  "Cursor type to use out `modal-mode'.

See description of `cursor-type' for mode information, this
variable should follow the same conventions."
  :tag "Insertion Mode Cursor Type"
  :type '(choice
          (const :tag "use the cursor specified for the frame" t)
          (const :tag "don't display a cursor" nil)
          (const :tag "display a filled box cursor" box)
          (const :tag "display a hollow box cursor" hollow)
          (const :tag "display a vertical bar cursor with default width" bar)
          (cons  :tag "display a vertical bar cursor with given width"
                 (const bar) (integer :tag "width of cursor"))
          (const :tag "display a horizontal bar cursor with default height" hbar)
          (cons  :tag "display a horizontal bar cursor with given height"
                 (const hbar (integer :tag "height of cursor")))))

(defcustom modal-idle-secs 1
  "Modal mode enabled during `modal-idle-secs' seconds")

;;;###autoload
(defcustom modal-excluded-modes nil
  "List of major modes for which `modal-mode' should not be activated.

This variable is considered when Modal is enabled globally via
`modal-global-mode'."
  :tag  "Excluded Modes"
  :type '(repeat :tag "Major modes to exclude" symbol))

;;;###autoload
(defcustom modal-insertp-functions nil
  "List of functions to avoid on read only mode.

This variable is considered on read only mode
`modal-global-mode'."
  :tag  "Insertp Functions"
  :type '(repeat :tag "Insertp functions to avoid on read only mode" symbol))

(defvar modal-mode-map (make-sparse-keymap)
  "This is Modal mode map, used to translate your keys.")

(defun modal-find-bind (key)
  (cl-some (lambda (keymap)
             (unless (eq keymap modal-mode-map)
               (let ((binding (lookup-key keymap key)))
                 (if (commandp binding)
                     binding))))
           (current-active-maps)))

(defun modal-find-bind-check-read-only (actual-key target-key)
  (let ((binding (modal-find-bind target-key)))
    (if (and buffer-read-only
             (memq binding modal-insertp-functions))
        (setq binding (modal-find-bind actual-key)))
    binding))

;;;###autoload
(defun modal-define-key (actual-key target-key)
  "Register translation from ACTUAL-KEY to TARGET-KEY."
  (if (arrayp target-key)
      (let ((docstring (format "Modal mode translates \"%s\" into \"%s\"."
                               (key-description actual-key)
                               (key-description target-key))))
        (define-key
          modal-mode-map
          actual-key
          (eval
           `(lambda ()
              ,docstring
              (interactive)
              (let ((binding (modal-find-bind-check-read-only ,actual-key ,target-key)))
                (when binding
                  (setq real-this-command binding
                        this-original-command binding
                        this-command binding)
                  (call-interactively binding)))))))
    (define-key modal-mode-map actual-key target-key)))

;;;###autoload
(defun modal-remove-key (key)
  "Unregister translation from KEY."
  (define-key modal-mode-map key nil))

(defvar modal--original-buffer nil)

;;;###autoload
(defun modal-add-first-parent (keymap)
  (let ((parent (keymap-parent keymap)))
    (if parent
        (set-keymap-parent keymap (make-composed-keymap
                                   modal-mode-map
                                   parent))
      (set-keymap-parent keymap modal-mode-map))))

;;;###autoload
(defun modal-add-last-parent (keymap)
  (let ((parent (keymap-parent keymap)))
    (if parent
        (set-keymap-parent keymap (make-composed-keymap
                                   parent
                                   modal-mode-map))
      (set-keymap-parent keymap modal-mode-map))))

;;;###autoload
(define-minor-mode modal-mode
  "Toggle the `modal-mode' minor mode.

With a prefix argument ARG, enable `modal-mode' if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or NIL, and toggle it if ARG is
`toggle'.

This minor mode setups translation of key bindings according to
configuration created previously with `modal-define-key' and
`modal-define-keys'."
  :lighter "◇"
  :keymap modal-mode-map
  (if modal-mode
      (progn
        ;; (dolist (buffer (buffer-list))
        ;;   (with-current-buffer buffer
        (setq-local cursor-type modal-cursor-type)
        ;; (set-face-attribute 'hl-line nil
        ;;                     :background "#3B3B5E")
        ;; ))
        (define-key isearch-mode-map (kbd "Q") #'isearch-quote-char)
        (define-key isearch-mode-map (kbd "G") #'isearch-abort)
        (define-key isearch-mode-map (kbd "S") #'isearch-repeat-forward)
        (define-key isearch-mode-map (kbd "R") #'isearch-repeat-backward)
        (define-key universal-argument-map "U" #'universal-argument-more))
    ;; (dolist (buffer (buffer-list))
    ;;   (with-current-buffer buffer
    (setq-local cursor-type modal-insert-cursor-type)
    ;; (set-face-attribute 'hl-line nil
    ;;                     :background "#3B3B3B")
    ;; ))
    (define-key isearch-mode-map (kbd "Q") #'isearch-printing-char)
    (define-key isearch-mode-map (kbd "G") #'isearch-printing-char)
    (define-key isearch-mode-map (kbd "S") #'isearch-printing-char)
    (define-key isearch-mode-map (kbd "R") #'isearch-printing-char)
    (define-key universal-argument-map "U" nil)))

(defun modal--maybe-activate ()
  "Activate `modal-mode' if current buffer is not minibuffer or blacklisted.

This is used by `modal-global-mode'."
  (unless (and (not (and modal--original-buffer
                         (equal modal--original-buffer
                                (current-buffer))))
               ;; (or (minibufferp)
               (member major-mode modal-excluded-modes)
               ;;)
               )
    (modal-mode 1)))

;;;###autoload
(define-globalized-minor-mode modal-global-mode
  modal-mode
  modal--maybe-activate)

;; Minibuffer
(add-hook 'minibuffer-setup-hook 'modal-mode)

(defun modal-global-mode-idle (secs)
  (interactive "P")
  (let ((new-state (if modal-mode 0 1))
        (modal--original-buffer (current-buffer)))
    (modal-global-mode new-state)
    (run-with-idle-timer (if (and (integerp secs) (< 0 secs))
                             secs
                           modal-idle-secs)
                         nil #'modal-global-mode (- 1 new-state))))

(defun modal-global-mode-force ()
  (interactive)
  (unless modal-mode
    (let ((modal--original-buffer (current-buffer)))
      (modal-global-mode 1))))

(defun modal-global-mode-toggle (secs)
  (interactive "P")
  (if secs
      (modal-global-mode-idle secs)
    (if modal-mode
        (modal-global-mode 0)
      (modal-global-mode-force))))

(defun modal-global-mode-force-with-unbind ()
  (interactive)
  (global-set-key [(escape)] nil)
  (modal-global-mode-force))

(defun modal-global-mode-disable-with-bind ()
  (interactive)
  (global-set-key [(escape)] 'modal-global-mode-force-with-unbind)
  (modal-global-mode 0))

;; advices
(defun modal--input-function-advice (fnc key)
  "Call FNC with KEY as argument only when `modal-mode' is disabled.

Otherwise use `list'."
  (funcall (if modal-mode #'list fnc) key))
(advice-add 'quail-input-method :around #'modal--input-function-advice)

;; which-key-mode
(with-eval-after-load 'which-key
  (cl-delete '((nil . "\\`\\?\\?\\'") . (nil . "lambda")) which-key-replacement-alist)
  (push '((nil . "\\`\\?\\?\\'") .
          (lambda (key-binding)
            (let* ((key (car key-binding))
                   (key-array (kbd key)))
              (condition-case nil
                  (let ((keys (split-string (documentation (key-binding key-array) t) "\"")))
                    (if (and (= (length keys) 5)
                             (string-equal (car keys) "Modal mode translates "))
                        (let ((binding (modal-find-bind-check-read-only
                                        key-array
                                        (kbd (nth 3 keys)))))
                          (and binding (cons key (copy-sequence
                                                  (symbol-name binding)))))
                      (cons key "lambda")))
                (error (cons key "lambda")))))) which-key-replacement-alist))

;; (fset 'modal--symbol-name (symbol-function 'symbol-name))

;; (defun modal--which-key-advice (orig-fun &rest args)
;;   (cl-letf (((symbol-function 'symbol-name)
;;              (lambda (symbol)
;;                (let ((name (modal--symbol-name symbol)))
;;                  (message "--- %s" name)
;;                  (if (string-equal name "**modal-mode-translation**")
;;                      (message (modal--symbol-name (funcall symbol t)))
;;                    name)))))
;;     (apply orig-fun args)))

;; (with-eval-after-load 'which-key
;;   (advice-add 'which-key--get-keymap-bindings :around 'modal--which-key-advice))
;;;;;;;;;;
;; keys ;;
;;;;;;;;;;
;; Make compatible with other modules
(define-key special-mode-map [?\S-\ ] nil)     ;; simple.el
(with-eval-after-load 'rmail
  (define-key rmail-mode-map [?\S-\ ] nil))       ;; rmail.el
(with-eval-after-load 'cus-edit
  (define-key custom-mode-map [?\S-\ ] nil)       ;; cus-edit.el
  (define-key custom-mode-link-map [?\S-\ ] nil)) ;; cus-edit.el
(mapc (lambda (keymap)
        (define-key keymap [?\S-\ ] nil))
      (keymaps-with-binding [?\S-\ ]))
;; ;; Modal keys
(global-set-key (kbd "µ") #'modal-global-mode-toggle)


(provide 'modal)
;;; modal.el ends here
