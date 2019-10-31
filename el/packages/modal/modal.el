;;; modal.el --- Easily introduce native modal editing of your own design
;;
;; thanks to: https://github.com/mrkkrp/modalka
;;
;;; Commentary:

;; functions (modal-define-key <key> <function>) only with global <key>

;; if <key0> in (modal-define-key <key0> <new-keyA>) is the
;; prefix in other (modal-define-key <key0 key1> <new-keyB>)
;; sort like this:
;; (modal-define-key <key0 key1> <new-keyB>)
;; (modal-define-key <key0> <new-keyA>)

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

(defvar modal--translations (make-hash-table :test 'equal)
  "Especial translations.")

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

(defun modal--bind-and-remove-from-hook (key command)
  (eval `(defun modal--self-remove-from-hook ()
           (define-key modal-mode-map ,key ,command)
           (remove-hook 'pre-command-hook 'modal--self-remove-from-hook)))
  (add-hook 'pre-command-hook 'modal--self-remove-from-hook))

;;;###autoload
(defun modal-define-key (actual-key target-key)
  "Register translation from ACTUAL-KEY to TARGET-KEY."
  (if (arrayp target-key)
      (let* ((sub-keymap (lookup-key modal-mode-map actual-key))
             (target-key-description (key-description target-key))
             (docstring (format "Modal mode translates \"%s\" into \"%s\"."
                                (key-description actual-key)
                                target-key-description)))
        ;; (if (equal actual-key target-key)
        ;;     (error "Dangerous and absurd: %s" docstring))
        (puthash target-key-description actual-key modal--translations)
        (define-key
          modal-mode-map
          actual-key
          (eval
           `(lambda ()
              ,docstring
              (interactive)
              (let ((binding (modal-find-bind-check-read-only ,actual-key ,target-key)))
                (if binding
                    (progn
                      (setq real-this-command binding
                            this-original-command binding
                            this-command binding)
                      (call-interactively binding))
                  ,@(cond
                     ((keymapp sub-keymap)
                      `((define-key modal-mode-map ,actual-key ',sub-keymap)
                        (modal--bind-and-remove-from-hook ,actual-key real-this-command)
                        (setq prefix-arg current-prefix-arg
                              unread-command-events (mapcar
                                                     (lambda (e) (cons t e))
                                                     (listify-key-sequence ,actual-key)))))
                     ((or (null sub-keymap)
                          (numberp sub-keymap))
                      `((setq prefix-arg current-prefix-arg
                              unread-command-events (mapcar
                                                     (lambda (e) (cons t e))
                                                     (listify-key-sequence ,target-key)))))
                     (t
                      (display-warning
                       'modal
                       (format-message
                        "Inconsistency on %s (`lookup-key' `modal-mode-map' (`kbd' \"%s\")) returns %s"
                        docstring target-key-description sub-keymap))
                        '()))))))))
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
  (let ((c-g-key (gethash "C-g" modal--translations))
        (c-u-key (gethash "C-u" modal--translations)))
    (if modal-mode
        (progn
          (setq-local cursor-type modal-cursor-type)
          (when c-g-key
            (define-key function-key-map c-g-key "\C-g")    ;; read-key
            (define-key query-replace-map c-g-key 'quit) ;; read-event
            ;; minibuffer
            (define-key minibuffer-local-map c-g-key #'abort-recursive-edit)
            )
          (when c-u-key
            (define-key universal-argument-map c-u-key #'universal-argument-more)))
      (setq-local cursor-type modal-insert-cursor-type)
      (when c-g-key
        (define-key function-key-map c-g-key nil)
        (define-key query-replace-map c-g-key nil)
        ;; minibuffer
        (define-key minibuffer-local-map c-g-key nil)
        )
      (when c-u-key
        (define-key universal-argument-map c-u-key nil)))))

(defun modal--maybe-activate ()
  "Activate `modal-mode' if current buffer is not blacklisted.

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

(defun modal-global-mode-force ()
  (interactive)
  (unless modal-mode
    (let ((modal--original-buffer (current-buffer)))
      (modal-global-mode 1))))

(defvar modal--post-command-countdown nil)

(defun modal--enable-mode-and-remove-from-hook ()
  (if (< 0 modal--post-command-countdown)
      (cl-decf modal--post-command-countdown)
    (modal-global-mode 1)
    (remove-hook 'post-command-hook 'modal--enable-mode-and-remove-from-hook)
    (setq modal--post-command-countdown nil)))

(defun modal--disable-mode-and-remove-from-hook ()
  (if (< 0 modal--post-command-countdown)
      (cl-decf modal--post-command-countdown)
    (modal-global-mode 0)
    (remove-hook 'post-command-hook 'modal--disable-mode-and-remove-from-hook)
    (setq modal--post-command-countdown nil)))

(defun modal-global-mode-post-command (times)
  (interactive "p")
  (if modal--post-command-countdown
      (setq modal--post-command-countdown (+ modal--post-command-countdown times 1))
    (when (and (numberp times)
               (< 0 times))
      (setq modal--post-command-countdown times)
      (if modal-mode
          (progn
            (modal-global-mode 0)
            (add-hook 'post-command-hook 'modal--enable-mode-and-remove-from-hook))
        (modal-global-mode 1)
        (add-hook 'post-command-hook 'modal--disable-mode-and-remove-from-hook)))))

(defun modal-global-mode-idle (secs)
  (interactive "P")
  (let ((new-state (if modal-mode 0 1))
        (modal--original-buffer (current-buffer)))
    (modal-global-mode new-state)
    (run-with-idle-timer (if (and (integerp secs) (< 0 secs))
                             secs
                           modal-idle-secs)
                         nil #'modal-global-mode (- 1 new-state))))

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

(defun modal--quoted-insert-advice (orig-fun arg)
  (let ((key (gethash "C-g" modal--translations)))
    (if (and modal-mode
             key)
        (progn
          (define-key function-key-map key nil)
          (unwind-protect
              (funcall orig-fun arg)
            (define-key function-key-map key "\C-g")))
      (funcall orig-fun arg))))
(advice-add 'quoted-insert :around #'modal--quoted-insert-advice)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Integration with other packages ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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


(provide 'modal)
;;; modal.el ends here
