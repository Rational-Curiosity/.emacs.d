;;; shell-config.el --- Configure and improve shell

;;; Commentary:

;; Usage:
;; (require 'shell-config)

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             ;;
;;   Shell and commands        ;;
;;                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq shell-file-name "bash")
;;;;;;;;;;;;;
;; Filtros ;;
;;;;;;;;;;;;;
;; Make URLs clickable
(add-hook 'shell-mode-hook (lambda () (goto-address-mode 1)))
;; Make file paths clickable
(add-hook 'shell-mode-hook 'compilation-shell-minor-mode)
;; Actualiza 'default-directory' mirando el prompt
(add-hook 'shell-mode-hook #'dirtrack-mode)
;; Colorea los avisos
(add-hook 'shell-mode-hook #'hl-advices)
;;;;;;;;;;;;;;;;;;;;
;; Autocompletado ;;
;;;;;;;;;;;;;;;;;;;;
(require 'bash-completion)
(bash-completion-setup)

;;;;;;;;;;;;;;
;; Opciones ;;
;;;;;;;;;;;;;;
(setq-default dirtrack-list '("\033\\[00;34m\\([^\033]+\\)" 1 nil))
(require 'comint-bug)
(setq comint-scroll-to-bottom-on-input t  ; always insert at the bottom
      comint-scroll-to-bottom-on-output t ; always add output at the bottom
      comint-scroll-show-maximum-output t ; scroll to show max possible output
      comint-completion-autolist t        ; show completion list when ambiguous
      comint-input-ignoredups t           ; no duplicates in command history
      comint-completion-addsuffix t       ; insert space/slash after file completion
      )
(add-hook 'shell-mode-hook #'ansi-color-for-comint-mode-on)
(add-to-list 'comint-output-filter-functions 'ansi-color-process-output)
(set-face-attribute 'comint-highlight-prompt nil
                    :inherit nil)
;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables de entorno ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set 'exec-path (getenv "PATH"))
;; Asigna el valor a variables necesarias
;; al usar la shell de emacs.
;;(setenv "PATH" (getenv "PATH"))
;; Locales
(setenv "LANG" "es_ES.UTF-8")
(setenv "LC_ALL" "")
(setenv "PROMPT_COMMAND" "")

;;;;;;;;;;;;;
;; execute ;;
;;;;;;;;;;;;;
;; (defun execute (dir)
;;   (interactive
;;    (list
;;     (let ((dir (and (bound-and-true-p execute-directory)
;;                     (eval execute-directory))))
;;      (if (or (not dir) current-prefix-arg)
;;          (read-string "Execute directory: " dir)
;;        dir))))
;;   (let ((default-directory dir))
;;     (compilation-start (read-string "Execute command: "
;;                                     (and (bound-and-true-p execute-command)
;;                                          (eval execute-command))))))
(defvar execute-list '(compile execute test check config convert copy move next previous extern generate clean recompile build rebuild))
(defun execute (&optional arg)
  (interactive "P")
  (let* ((pair-list (cl-remove-if-not
                     #'boundp
                     execute-list))
         (pair (eval (intern (completing-read "Execute: " pair-list nil t))))
         (default-directory (if current-prefix-arg
                                (read-string "Execute directory: " (eval (cdr pair)))
                              (eval (cdr pair)))))
    (save-some-buffers arg)
    (compilation-start (read-string "Execute command: " (eval (car pair))))))

(defvar insert-from-function-alist '(("git branches" . vc-git-branches)))
(defun insert-from-function (arg)
  (interactive "P")
  (let* ((functions (mapcar 'car insert-from-function-alist))
         (result (completing-read "Text to insert: "
                                  (funcall
                                   (cdr (assoc
                                         (completing-read "Function: "
                                                          functions nil t)
                                         insert-from-function-alist))))))
    (if arg
        (kill-new result)
      (insert result))))
;;;;;;;;;;;;;;;
;; Funciones ;;
;;;;;;;;;;;;;;;
;; Sustituye:
;; '$p' por el nombre del buffer con su ruta.
;; '$n' por el nombre del buffer.
;; '$b' por el nombre del buffer sin extensión.
(defun shell-execute (command &rest args)
  (interactive
   (list
    (read-shell-command "Shell command: " nil nil
                        (let ((filename
                               (cond
                                (buffer-file-name)
                                ((eq major-mode 'dired-mode)
                                 (dired-get-filename nil t)))))
                          (and filename (file-relative-name filename))))
    current-prefix-arg
    shell-command-default-error-buffer))
  (apply 'shell-command (replace-regexp-in-string
                         "%p" (concat "\"" (or buffer-file-name "") "\"")
                         (replace-regexp-in-string
                          "%n" (concat "\"" (or (buffer-name) "") "\"")
                          (replace-regexp-in-string
                           "%b" (concat "\"" (or (file-name-base) "") "\"")
                           command t t) t t) t t) args))

(bind-keys
 ("C-M-!"   . insert-from-function)
 ("M-!"     . shell-execute)
 ("M-s RET" . shell-execute)
 ("C-!"     . execute))
;; (global-set-key (kbd "M-!") 'shell-execute)
;; (global-set-key (kbd "M-s RET") 'shell-execute)

;; ansi-term con utf-8
;; (defadvice ansi-term (after advise-ansi-term-coding-system)
;;     (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))
;; (ad-activate 'ansi-term)
;; another option
;; (add-hook 'term-exec-hook
;;           (function
;;            (lambda ()
;;              (set-buffer-process-coding-system 'utf-8-unix 'utf-8-unix))))

;; Habilita colores en la terminal
;; (require 'ansi-color)
;; (defadvice display-message-or-buffer (before ansi-color activate)
;;   "Process ANSI color codes in shell output."
;;   (let ((buf (ad-get-arg 0)))
;;     (and (bufferp buf)
;;          (string= (buffer-name buf) "*Compilation Output*")
;;          (with-current-buffer buf
;;            (ansi-color-apply-on-region (point-min) (point-max))))))
;; (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;; (add-to-list 'comint-output-filter-functions 'ansi-color-process-output)


(provide 'shell-config)
;;; shell-config.el ends here
