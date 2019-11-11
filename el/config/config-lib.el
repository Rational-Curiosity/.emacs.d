;;; config-lib.el --- Library for configurations

;;; Commentary:

;; Usage:
;; (require 'config-lib)

;;; Code:

(defun remove-nth-element (nth list)
  (if (zerop nth) (cdr list)
    (let ((last (nthcdr (1- nth) list)))
      (setcdr last (cddr last))
      list)))

(defun assoc-keys (keys alist &optional test-fun)
  "Recursively find KEYS in ALIST."
  (if keys
      (cond
       ((listp alist)
        (assoc-keys (cdr keys) (cdr (assoc (car keys) alist test-fun)) test-fun))
       ((vectorp alist)
        (mapcar (lambda (al)
                  (assoc-keys (cdr keys) (cdr (assoc (car keys) al test-fun)) test-fun)) alist)))
    alist))

(defun eval-string (string)
  "Evaluate elisp code stored in a string."
  (eval (car (read-from-string string))))

;; keymaps
(defun keymap-symbol (keymap)
  "Return the symbol to which KEYMAP is bound, or nil if no such symbol exists."
  (catch 'gotit
    (mapatoms (lambda (sym)
                (and (boundp sym)
                     (eq (symbol-value sym) keymap)
                     (not (eq sym 'keymap))
                     (throw 'gotit sym))))))

(defun keymaps-with-binding (key)
  (let (keymaps)
    (mapatoms (lambda (ob) (if (boundp ob)
                          (let ((keymap (symbol-value ob)))
                            (if (keymapp keymap)
                                (let ((m (lookup-key keymap key)))
                                  (if (and m (or (symbolp m) (keymapp m)))
                                      (push keymap keymaps)))))))
              obarray)
    keymaps))

(defun locate-key-binding (key)
  "Determine keymaps KEY is defined"
  (interactive "kPress key: ")
  (let ((key-str (key-description key)))
    (mapatoms (lambda (ob) (when (and (boundp ob) (keymapp (symbol-value ob)))
                        (let ((m (lookup-key (symbol-value ob) key)))
                          (when m
                            (message "key: %s, keymap: %S, bind: %s" key-str ob m)))))
              obarray)))

;; Recursibe byte compile
(defun byte-compile-recursively (directory)
  (interactive "DByte compile and recompile directory: ")
  (byte-recompile-directory directory 0 t))

(require 'cl-lib)
;; [ get current function name
;; thanks to: https://emacs.stackexchange.com/a/2312
(defun call-stack ()
  "Return the current call stack frames."
  (let ((frames)
        (frame)
        (index 5))
    (while (setq frame (backtrace-frame index))
      (push frame frames)
      (cl-incf index))
    (cl-delete-if-not 'car frames)))

(defmacro compile-time-function-name ()
  "Get the name of calling function at expansion time."
  (symbol-name
   (cl-cadadr
    (cl-caddr
     (cl-find-if (lambda (frame)
                (ignore-errors (equal (car (cl-caddr frame)) 'defalias)))
              (reverse (call-stack)))))))
;; ]

;; Converts calls to COMPOSE to lambda forms with everything written
;; out and some things written as direct function calls.
;; Example: (compose #'1+ #'2* #'-) => (LAMBDA (X) (1+ (2* (- X))))
(cl-define-compiler-macro compose (&rest functions)
  (cl-labels ((sharp-quoted-p (x)
                           (and (listp x)
                                (eql (cl-first x) 'function)
                                (symbolp (cl-second x)))))
    `(lambda (x) ,(cl-reduce #'(lambda (fun arg)
                         (if (sharp-quoted-p fun)
                             (list (cl-second fun) arg)
                           (list 'funcall fun arg)))
                     functions
                     :initial-value 'x
                     :from-end t))))

;; Eval checking bound before
(defmacro bound-and-eval (func &rest args)
  "Ensures FUNC exist and eval with ARGS."
  (list 'and (list 'fboundp func) (list 'apply func (list 'quote args))))

;; Return function that check bound before
(defun lambda-bound-and-eval (func &rest args)
  "Return lambda that ensures FUNC exist and eval with ARGS."
  (lexical-let ((func func)
                (args args))
    (lambda () (and (fboundp func) (apply func args)))))

;; write message in *Messages* buffer with colors
;; Thanks to: https://emacs.stackexchange.com/a/20178
(defun message-color (format &rest args)
  "Acts like `message' but preserves string properties in the *Messages* buffer."
  (let ((message-log-max nil))
    (apply 'message format args))
  (with-current-buffer (get-buffer "*Messages*")
    (save-excursion
      (goto-char (point-max))
      (let ((inhibit-read-only t))
        (unless (zerop (current-column)) (insert "\n"))
        (insert (apply 'format format args))
        (insert "\n")))))

;; Silent messages
;; Usage:
;; (advice-add '<orig-fun> :around #'message-silent-advice)
(defun message-silent-advice (orig-fun &rest args)
  "Silent and eval ORIG-FUN with ARGS."
  (let ((message-log-max nil)
        (inhibit-message t))
    (apply orig-fun args)))

;; Inhibit messages on echo
;; Usage:
;; (advice-add '<orig-fun> :around #'message-inhibit-advice)
(defun message-inhibit-advice (orig-fun &rest args)
  "Inhibit message and eval ORIG-FUN with ARGS."
  (let ((inhibit-message t))
    (apply orig-fun args)))

;; Truncate messages
;; Usage:
;; (advice-add '<orig-fun> :around #'message-truncate-advice)
(defun message-truncate-advice (orig-fun &rest args)
  "Stablish `message-truncate-lines' and eval ORIG-FUN with ARGS."
  (let ((message-truncate-lines t))
    (apply orig-fun args)))

(require 'server)
(defmacro eval-and-when-daemon (frame &rest body)
  "When starting daemon wait FRAME ready before BODY."
  (declare (indent defun))
  (cons 'if
        (cons
         (list 'daemonp)
         (nconc
          (list (list 'add-hook (quote 'after-make-frame-functions)
                      (cons 'lambda (cons (list (if frame frame 'frame)) body))))
          (list (list 'funcall
                      (cons 'lambda (cons (list (if frame frame 'frame)) body))
                      (list 'selected-frame)))))))

;; Load all libraries in directory
(defun load-all-in-directory (dir)
  "`load' all elisp libraries in directory DIR which are not already loaded."
  (interactive "D")
  (let ((libraries-loaded (mapcar #'file-name-sans-extension
                                  (delq nil (mapcar #'car load-history)))))
    (dolist (file (directory-files dir t ".+\\.elc?$"))
      (let ((library (file-name-sans-extension file)))
        (unless (member library libraries-loaded)
          (load library nil t)
          (push library libraries-loaded))))))

;; Establish safe dir-locals variables
(defun safe-dir-locals (dir list &optional class)
  "Set local variables for directory.
DIR directory.
LIST list of local variables.
CLASS optional class name, DIR default."
  (unless class
    (setq class dir))
  (dir-locals-set-class-variables class list)
  (dir-locals-set-directory-class dir class)
  (dolist (item list)
    (setq safe-local-variable-values (nconc safe-local-variable-values (cdr item))))
  class)


;; regex inside each file in list
;; return first occurence
(defun re-search-in-files (regex files &optional first)
  "Search REGEX match inside the files of FILES list.
If FIRST is not-nil return first file in files with regex match.
Otherwise return a list of files which regex match."
  (let ((matched '()))
    (while (and
            files
            (not (and first matched)))
      (let* ((file (pop files))
             (buffer (get-file-buffer file)))
        (if buffer
            (with-current-buffer buffer
              (save-excursion
                (goto-char (point-min))
                (when (re-search-forward regex nil t)
                  (push file matched))))
          (let ((buffer (find-file-noselect file t t)))
            (with-current-buffer buffer
              (when (re-search-forward regex nil t)
                (push file matched)))
            (kill-buffer buffer)))))
    (if first
        (car matched)
      matched)))

;; bugs
(defun bug-check-function-bytecode (function bytecode-base64 &optional inhibit-log)
  "Check if FUNCTION has BYTECODE-BASE64.  If INHIBIT-LOG is non-nil inhibit log when differs."
  (if (string-equal
       (condition-case nil
           (base64-encode-string (aref (symbol-function function) 1) t)
         (error
          (unless inhibit-log
            (message-color #("WARN missing function bytecode, maybe %s is a built-in function in 'C source code' or not bytecompiled"
                             0 4 (face warning)) function))))
       bytecode-base64)
      t
    (unless inhibit-log
      (message-color #("WARN bug fixed for different version of %s see %s"
                       0 4 (face warning))
                     (if (fboundp 'find-function-library)
                         (find-function-library function)
                       (symbol-name function))
                     load-file-name))
    nil))


(require 'help-fns)
(defun bug-function-bytecode-into-base64 (function)
  "Write the bytecode of FUNCTION (a symbol).
When called from lisp, FUNCTION may also be a function object."
  (interactive
   (let* ((fn (function-called-at-point))
          (enable-recursive-minibuffers t)
          (val (completing-read
                (if fn
                    (format "Bytecode of function (default %s): " fn)
                  "Bytecode of function: ")
                #'help--symbol-completion-table
                (lambda (f) (fboundp f))
                t nil nil
                (and fn (symbol-name fn)))))
     (unless (equal val "")
       (setq fn (intern val)))
     (unless (and fn (symbolp fn))
       (user-error "You didn't specify a function symbol"))
     (unless (fboundp fn)
       (user-error "Symbol's function definition is void: %s" fn))
     (list fn)))
  (insert "\""
          (condition-case nil
              (base64-encode-string (aref (symbol-function function) 1) t)
            (error
             (user-error "Missing function bytecode, maybe %s is a built-in function in 'C source code' or not bytecompiled" function)))
          "\""))

;; escape special characters
;; (let ((print-escape-newlines t))
;;   (prin1-to-string "..."))

;;;;;;;;;;;;
;; Errors ;;
;;;;;;;;;;;;
;; Protect from errors
(defun rollback-on-error-inc ()
  "Increment `rollback-on-error-counter' fake variable."
  (cl-incf rollback-on-error-counter))
(defun rollback-on-error-advice (orig-fun &rest args)
  "Rollback (ORIG-FUN ARGS) evaluation on error.

Example: (advice-add 'mt-interchange-thing-up :around #'rollback-on-error-advice)"
  ;; (undo-boundary)  ; <undo>
  (advice-add 'undo-boundary :before #'rollback-on-error-inc)
  (unwind-protect
      (let ((rollback-on-error-counter 1))
        (condition-case raised-error
            (apply orig-fun args)
          (error (primitive-undo rollback-on-error-counter
                                 buffer-undo-list)
                 (error "%s: %s rolled back (%i)"
                        orig-fun
                        (error-message-string raised-error)
                        rollback-on-error-counter))))
    (advice-remove 'undo-boundary #'rollback-on-error-inc)))


;;;;;;;;;;;;;;;
;; Processes ;;
;;;;;;;;;;;;;;;
(defun process-get-attrs (pid attrs-process)
  (let ((process-attrs (process-attributes pid)))
    (cons `(pid . ,pid) (mapcar (lambda (attr) (assoc attr process-attrs)) attrs-process))))

(defun processes-named (names attrs-processes)
  (cl-remove-if-not (lambda (attrs-process) (member (cdr (assoc 'comm attrs-process)) names)) attrs-processes))

(defun processes-children (pid attrs-processes)
  (cl-remove-if-not (lambda (attrs-process) (let ((ppid (cdr (assoc 'ppid attrs-process))))
                                         (and (integerp ppid) (= pid ppid)))) attrs-processes))

(defun processes-children-all (pid attrs-processes)
  (let ((pids (list pid))
        (children)
        (processes))
    (while pids
      (setq children nil)
      (mapc (lambda (pid) (setq children (nconc children (processes-children pid attrs-processes)))) pids)
      (setq processes (nconc processes children))
      (setq pids (mapcar (lambda (attrs-process) (cdr (assoc 'pid attrs-process))) children)))
    processes))

(defmacro processes-run-with-timer-cond-body (secs repeat process-names
                                                 processes-number-variable
                                                 processes-number-condition
                                                 &rest body)
  (declare (indent 5))
  `(run-with-timer
    ,secs ,repeat
    (lambda ()
      ;; [ Limit python's processes of all emacs
      ;; (let ((attrs-processes (mapcar (lambda (x) (process-get-attrs x '(ppid comm))) (list-system-processes)))
      ;;       (emacs-processes))
      ;;   (mapc (lambda (x) (nconc emacs-processes (processes-children-all (cdr (assoc 'pid x)) attrs-processes))) (processes-named "emacs.exe" attrs-processes))
      ;;   (processes-named "python.exe" emacs-processes))
      ;; ]
      ;; Limit python's processes of every emacs
      (let ((,processes-number-variable (length (processes-named
                                                 ,process-names
                                                 (processes-children-all
                                                  (emacs-pid)
                                                  (mapcar (lambda (x) (process-get-attrs x '(ppid comm)))
                                                          (list-system-processes)))))))
        (when
            ,processes-number-condition
          ,@body)))))


(provide 'config-lib)
;;; config-lib.el ends here
