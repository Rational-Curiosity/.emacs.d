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
(defun execute ()
  (interactive)
  (let* ((pair-list (cl-remove-if-not
                     #'boundp
                     execute-list))
         (pair (eval (intern (completing-read "Execute: " pair-list nil t))))
         (default-directory (if current-prefix-arg
                                (read-string "Execute directory: " (eval (cdr pair)))
                              (eval (cdr pair)))))
    (compilation-start (read-string "Execute command: " (eval (car pair))))))

;;;;;;;;;;;;;;;
;; Funciones ;;
;;;;;;;;;;;;;;;
;; Sustituye:
;; '$p' por el nombre del buffer con su ruta.
;; '$n' por el nombre del buffer.
;; '$b' por el nombre del buffer sin extensión.
(defun shell-execute (command &optional output-buffer error-buffer)
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
  ;; Look for a handler in case default-directory is a remote file name.
  (let ((command (replace-regexp-in-string "\$p" (concat "\"" (or buffer-file-name "") "\"") 
                     (replace-regexp-in-string "\$n" (concat "\"" (or (buffer-name) "") "\"")
                       (replace-regexp-in-string "\$b" (concat "\"" (or (file-name-base) "") "\"")
                         command))))
        (handler
	 (find-file-name-handler (directory-file-name default-directory)
				 'shell-command)))
    (if handler
	(funcall handler 'shell-command command output-buffer error-buffer)
      (if (and output-buffer
	       (not (or (bufferp output-buffer)  (stringp output-buffer))))
	  ;; Output goes in current buffer.
	  (let ((error-file
		 (if error-buffer
		     (make-temp-file
		      (expand-file-name "scor"
					(or small-temporary-file-directory
					    temporary-file-directory)))
		   nil)))
	    (barf-if-buffer-read-only)
	    (push-mark nil t)
	    ;; We do not use -f for csh; we will not support broken use of
	    ;; .cshrcs.  Even the BSD csh manual says to use
	    ;; "if ($?prompt) exit" before things which are not useful
	    ;; non-interactively.  Besides, if someone wants their other
	    ;; aliases for shell commands then they can still have them.
	    (call-process shell-file-name nil
			  (if error-file
			      (list t error-file)
			    t)
			  nil shell-command-switch command)
	    (when (and error-file (file-exists-p error-file))
	      (if (< 0 (nth 7 (file-attributes error-file)))
		  (with-current-buffer (get-buffer-create error-buffer)
		    (let ((pos-from-end (- (point-max) (point))))
		      (or (bobp)
			  (insert "\f\n"))
		      ;; Do no formatting while reading error file,
		      ;; because that can run a shell command, and we
		      ;; don't want that to cause an infinite recursion.
		      (format-insert-file error-file nil)
		      ;; Put point after the inserted errors.
		      (goto-char (- (point-max) pos-from-end)))
		    (display-buffer (current-buffer))))
	      (delete-file error-file))
	    ;; This is like exchange-point-and-mark, but doesn't
	    ;; activate the mark.  It is cleaner to avoid activation,
	    ;; even though the command loop would deactivate the mark
	    ;; because we inserted text.
	    (goto-char (prog1 (mark t)
			 (set-marker (mark-marker) (point)
				     (current-buffer)))))
	;; Output goes in a separate buffer.
	;; Preserve the match data in case called from a program.
	(save-match-data
	  (if (string-match "[ \t]*&[ \t]*\\'" command)
	      ;; Command ending with ampersand means asynchronous.
	      (let ((buffer (get-buffer-create
			     (or output-buffer "*Async Shell Command*")))
		    (directory default-directory)
		    proc)
		;; Remove the ampersand.
		(setq command (substring command 0 (match-beginning 0)))
		;; Ask the user what to do with already running process.
		(setq proc (get-buffer-process buffer))
		(when proc
		  (cond
		   ((eq async-shell-command-buffer 'confirm-kill-process)
		    ;; If will kill a process, query first.
		    (if (yes-or-no-p "A command is running in the default buffer.  Kill it? ")
			(kill-process proc)
		      (error "Shell command in progress")))
		   ((eq async-shell-command-buffer 'confirm-new-buffer)
		    ;; If will create a new buffer, query first.
		    (if (yes-or-no-p "A command is running in the default buffer.  Use a new buffer? ")
			(setq buffer (generate-new-buffer
				      (or output-buffer "*Async Shell Command*")))
		      (error "Shell command in progress")))
		   ((eq async-shell-command-buffer 'new-buffer)
		    ;; It will create a new buffer.
		    (setq buffer (generate-new-buffer
				  (or output-buffer "*Async Shell Command*"))))
		   ((eq async-shell-command-buffer 'confirm-rename-buffer)
		    ;; If will rename the buffer, query first.
		    (if (yes-or-no-p "A command is running in the default buffer.  Rename it? ")
			(progn
			  (with-current-buffer buffer
			    (rename-uniquely))
			  (setq buffer (get-buffer-create
					(or output-buffer "*Async Shell Command*"))))
		      (error "Shell command in progress")))
		   ((eq async-shell-command-buffer 'rename-buffer)
		    ;; It will rename the buffer.
		    (with-current-buffer buffer
		      (rename-uniquely))
		    (setq buffer (get-buffer-create
				  (or output-buffer "*Async Shell Command*"))))))
		(with-current-buffer buffer
		  (setq buffer-read-only nil)
		  ;; Setting buffer-read-only to nil doesn't suffice
		  ;; if some text has a non-nil read-only property,
		  ;; which comint sometimes adds for prompts.
		  (let ((inhibit-read-only t))
		    (erase-buffer))
		  (display-buffer buffer '(nil (allow-no-window . t)))
		  (setq default-directory directory)
		  (setq proc (start-process "Shell" buffer shell-file-name
					    shell-command-switch command))
		  (setq mode-line-process '(":%s"))
		  (require 'shell) (shell-mode)
		  (set-process-sentinel proc 'shell-command-sentinel)
		  ;; Use the comint filter for proper handling of carriage motion
		  ;; (see `comint-inhibit-carriage-motion'),.
		  (set-process-filter proc 'comint-output-filter)
		  ))
	    ;; Otherwise, command is executed synchronously.
	    (shell-command-on-region (point) (point) command
				     output-buffer nil error-buffer)))))))


(bind-keys
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
