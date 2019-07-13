;;; modal.el --- Easily introduce native modal editing of your own design
;;
;; thanks to: https://github.com/mrkkrp/modalka
;;
;;; Commentary:

;; This is a building kit to help switch to modal editing in Emacs.  The
;; main goal of the package is making modal editing in Emacs as natural and
;; native as possible.

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

;;;###autoload
(defcustom modal-excluded-modes nil
  "List of major modes for which `modal-mode' should not be activated.

This variable is considered when Modal is enabled globally via
`modal-global-mode'."
  :tag  "Excluded Modes"
  :type '(repeat :tag "Major modes to exclude" symbol))

(defvar modal-mode-map (make-sparse-keymap)
  "This is Modal mode map, used to translate your keys.")

;;;###autoload
(defun modal-define-key (actual-key target-key name)
  "Register translation from ACTUAL-KEY to TARGET-KEY with NAME."
  (let ((docstring (format "`%s' is called through an alias which translates %s into %s."
                           (key-binding     target-key)
                           (key-description actual-key)
                           (key-description target-key))))
    (eval
     `(define-key
        modal-mode-map
        ,actual-key
        (defalias (make-symbol ,name)
          (lambda ()
            (interactive)
            (let ((binding (key-binding ,target-key)))
              (unless (or (memq binding '(nil undefined))
                          (keymapp binding))
                (set 'this-command binding)
                (call-interactively binding))))
          ,docstring)))))

;;;###autoload
(defun modal-define-kbd (actual-kbd target-kbd &optional name)
  "Register translation from ACTUAL-KBD to TARGET-KBD with optional NAME.

Arguments are accepted in in the format used for saving keyboard
macros (see `edmacro-mode')."
  (modal-define-key (kbd actual-kbd) (kbd target-kbd) (or name target-kbd)))

;;;###autoload
(defun modal-remove-key (key)
  "Unregister translation from KEY."
  (define-key modal-mode-map key nil))

;;;###autoload
(defun modal-remove-kbd (kbd)
  "Unregister translation from KBD.

Arguments are accepted in in the format used for saving keyboard
macros (see `edmacro-mode')."
  (modal-remove-key (kbd kbd)))

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
  :lighter "↑"
  :keymap modal-mode-map
  (if modal-mode
      (progn
        ;; (dolist (buffer (buffer-list))
        ;;   (with-current-buffer buffer
        (setq-local cursor-type modal-cursor-type) ;; ))
        (define-key universal-argument-map "u" #'universal-argument-more)
        (global-set-key (kbd "<escape>") nil))
    ;; (dolist (buffer (buffer-list))
    ;;   (with-current-buffer buffer
    (setq-local cursor-type modal-insert-cursor-type) ;; ))
    (define-key universal-argument-map "u" nil)
    (global-set-key (kbd "<escape>") #'modal-global-mode)))

(defun modal--maybe-activate ()
  "Activate `modal-mode' if current buffer is not minibuffer or blacklisted.

This is used by `modal-global-mode'."
  (unless (or (minibufferp)
              (member major-mode modal-excluded-modes))
    (modal-mode 1)))

;;;###autoload
(define-globalized-minor-mode modal-global-mode
  modal-mode
  modal--maybe-activate)

;; advices
(defun modal--input-function-advice (fnc key)
  "Call FNC with KEY as argument only when `modal-mode' is disabled.

Otherwise use `list'."
  (funcall (if modal-mode #'list fnc) key))
(advice-add 'quail-input-method :around #'modal--input-function-advice)

;; keys
(define-key modal-mode-map (kbd "i") #'modal-global-mode)
(modal-define-kbd "u" "C-u" "universal-argument")


(provide 'modal)
;;; modal.el ends here
