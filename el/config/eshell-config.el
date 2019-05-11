(if (daemonp)
    (setenv "EDITOR" "emacsclient -c -n")
  (setenv "EDITOR" "emacs"))
;; (setenv "PAGER" "cat")

;;;;;;;;;;;;;;;;;
;; Emacs Shell ;;
;;;;;;;;;;;;;;;;;
(with-eval-after-load 'em-term
  (add-to-list 'eshell-visual-commands "htop")
  (add-to-list 'eshell-visual-options '("git" "--help" "--paginate"))
  (add-to-list 'eshell-visual-subcommands '("git" "log" "diff" "show")))
(setq eshell-prefer-lisp-functions t
      eshell-destroy-buffer-when-process-dies t
      eshell-cmpl-cycle-completions nil)

(advice-add 'eshell-previous-matching-input-from-input :before (lambda (arg) (goto-char (point-max))))
(advice-add 'eshell-next-matching-input-from-input :before (lambda (arg) (goto-char (point-max))))

(with-eval-after-load 'ag
  (defun ag-advice (orig-fun string &optional directory)
    (funcall orig-fun string (or directory default-directory)))
  (advice-add 'ag :around 'ag-advice))

;;;;;;;;;;;;;
;; Filters ;;
;;;;;;;;;;;;;
;; Make URLs clickable
(add-hook 'eshell-mode-hook (lambda () (goto-address-mode 1)))
;; Colorize advices
(add-hook 'eshell-post-command-hook (lambda () (unhl-advices) (hl-advices)))

;;;;;;;;;;;;
;; Prompt ;;
;;;;;;;;;;;;
(require 'dash)
(require 's)
(require 'vc-git)


(defvar pyvenv-virtual-env-name nil)

(defmacro with-face (STR &rest PROPS)
  "Return STR propertized with PROPS."
  `(propertize ,STR 'face (list ,@PROPS)))

(defmacro esh-section (NAME ICON FORM &rest PROPS)
  "Build eshell section NAME with ICON prepended to evaled FORM with PROPS."
  `(setq ,NAME
         (lambda () (when ,FORM
                 (-> ,ICON
                    (concat esh-section-delim ,FORM)
                    (with-face ,@PROPS))))))

(defun esh-acc (acc x)
  "Accumulator for evaluating and concatenating esh-sections."
  (--if-let (funcall x)
      (if (s-blank? acc)
          it
        (concat acc esh-sep it))
    acc))

;; Below I implement a "prompt number" section
(setq esh-prompt-num 0)
(add-hook 'eshell-mode-hook (lambda ()
                              (make-local-variable 'esh-prompt-num)
                              (setq-default esh-prompt-num 0)))

(defun esh-prompt-func ()
  "Build `eshell-prompt-function'"
  (setq esh-prompt-num (incf esh-prompt-num))
  (concat esh-header
          (-reduce-from 'esh-acc "" eshell-funcs)
          "\n"
          eshell-prompt-string))

(esh-section esh-dir
             (if (display-graphic-p) "📂" "")  ;  (faicon folder)
             (abbreviate-file-name (eshell/pwd))
             '(:foreground "gold" :bold ultra-bold :underline t))

(esh-section esh-git
             "⎇"  ;  (git icon)
             ;; (magit-get-current-branch)
             (car (vc-git-branches))
             '(:foreground "pink"))

(esh-section esh-python
             "\xe928"  ;  (python icon)
             pyvenv-virtual-env-name)

(esh-section esh-clock
             (if (display-graphic-p) "⏳" "τ")  ;  (clock icon)
             (format-time-string "%H:%M" (current-time))
             '(:foreground "forest green"))

(esh-section esh-num
             "☰"  ;  (list icon)
             (number-to-string esh-prompt-num)
             '(:foreground "brown"))

;; Separator between esh-sections
(setq esh-sep "  "  ; or " | "

      ;; Separator between an esh-section icon and form
      esh-section-delim " "

      ;; Eshell prompt header
      esh-header "\n┌─"  ; or "\n┌─"

      ;; Eshell prompt regexp and string. Unless you are varying the prompt by eg.
      ;; your login, these can be the same.
      eshell-prompt-string "└─» "   ; or "└─> "
      eshell-prompt-regexp (concat "^" eshell-prompt-string "\\|^[a-z]*>\\{1,4\\} "))   ; or "└─> "

;; Choose which eshell-funcs to enable
(setq eshell-funcs (list esh-dir esh-git esh-python esh-clock esh-num))

;; Enable the new eshell prompt
(setq eshell-prompt-function 'esh-prompt-func)

;;;;;;;;;;;;;;;;
;; Completion ;;
;;;;;;;;;;;;;;;;
;;**** Git Completion
(when (executable-find "git")
  (defun pcmpl-git-commands ()
    "Return the most common git commands by parsing the git output."
    (with-temp-buffer
      (call-process-shell-command "git" nil (current-buffer) nil "help" "--all")
      (goto-char 0)
      (search-forward "available git commands in")
      (let (commands)
        (while (re-search-forward
                "^[[:blank:]]+\\([[:word:]-.]+\\)[[:blank:]]*\\([[:word:]-.]+\\)?"
                nil t)
          (push (match-string 1) commands)
          (when (match-string 2)
            (push (match-string 2) commands)))
        (sort commands #'string<))))

  (defconst pcmpl-git-commands (pcmpl-git-commands)
    "List of `git' commands.")

  (defvar pcmpl-git-ref-list-cmd "git for-each-ref refs/ --format='%(refname)'"
    "The `git' command to run to get a list of refs.")

  (defun pcmpl-git-get-refs (type)
    "Return a list of `git' refs filtered by TYPE."
    (with-temp-buffer
      (insert (shell-command-to-string pcmpl-git-ref-list-cmd))
      (goto-char (point-min))
      (let (refs)
        (while (re-search-forward (concat "^refs/" type "/\\(.+\\)$") nil t)
          (push (match-string 1) refs))
        (nreverse refs))))

  (defun pcmpl-git-remotes ()
    "Return a list of remote repositories."
    (split-string (shell-command-to-string "git remote")))

  (defun pcomplete/git ()
    "Completion for `git'."
    ;; Completion for the command argument.
    (pcomplete-here* pcmpl-git-commands)
    (cond
     ((pcomplete-match "help" 1)
      (pcomplete-here* pcmpl-git-commands))
     ((pcomplete-match (regexp-opt '("pull" "push")) 1)
      (pcomplete-here (pcmpl-git-remotes)))
     ;; provide branch completion for the command `checkout'.
     ((pcomplete-match "checkout" 1)
      (pcomplete-here* (append (pcmpl-git-get-refs "heads")
                               (pcmpl-git-get-refs "tags"))))
     (t
      (while (pcomplete-here (pcomplete-entries)))))))

;;**** Bzr Completion
(when (executable-find "bzr")

  (defun pcmpl-bzr-commands ()
    "Return the most common bzr commands by parsing the bzr output."
    (with-temp-buffer
      (call-process-shell-command "bzr" nil (current-buffer) nil "help" "commands")
      (goto-char 0)
      (let (commands)
        (while (re-search-forward "^\\([[:word:]-]+\\)[[:blank:]]+" nil t)
          (push (match-string 1) commands))
        (sort commands #'string<))))

  (defconst pcmpl-bzr-commands (pcmpl-bzr-commands)
    "List of `bzr' commands.")

  (defun pcomplete/bzr ()
    "Completion for `bzr'."
    ;; Completion for the command argument.
    (pcomplete-here* pcmpl-bzr-commands)
    (cond
     ((pcomplete-match "help" 1)
      (pcomplete-here* pcmpl-bzr-commands))
     (t
      (while (pcomplete-here (pcomplete-entries)))))))
;;**** Mercurial (hg) Completion
(when (executable-find "hg")

  (defun pcmpl-hg-commands ()
    "Return the most common hg commands by parsing the hg output."
    (with-temp-buffer
      (call-process-shell-command "hg" nil (current-buffer) nil "-v" "help")
      (goto-char 0)
      (search-forward "list of commands:")
      (let (commands
            (bound (save-excursion
                     (re-search-forward "^[[:alpha:]]")
                     (forward-line 0)
                     (point))))
        (while (re-search-forward
                "^[[:blank:]]\\([[:word:]]+\\(?:, [[:word:]]+\\)*\\)" bound t)
          (let ((match (match-string 1)))
            (if (not (string-match "," match))
                (push (match-string 1) commands)
              (dolist (c (split-string match ", ?"))
                (push c commands)))))
        (sort commands #'string<))))

  (defconst pcmpl-hg-commands (pcmpl-hg-commands)
    "List of `hg' commands.")

  (defun pcomplete/hg ()
    "Completion for `hg'."
    ;; Completion for the command argument.
    (pcomplete-here* pcmpl-hg-commands)
    (cond
     ((pcomplete-match "help" 1)
      (pcomplete-here* pcmpl-hg-commands))
     (t
      (while (pcomplete-here (pcomplete-entries)))))))

;;;; sudo completion
(defun pcomplete/sudo ()
  "Completion rules for the `sudo' command."
  (let ((pcomplete-ignore-case t))
    (pcomplete-here (funcall pcomplete-command-completion-function))
    (while (pcomplete-here (pcomplete-entries)))))

;;;; systemctl completion
(defcustom pcomplete-systemctl-commands
  '("disable" "enable" "status" "start" "restart" "stop" "reenable"
    "list-units" "list-unit-files")
  "p-completion candidates for `systemctl' main commands"
  :type '(repeat (string :tag "systemctl command"))
  :group 'pcomplete)

(defvar pcomplete-systemd-units
  (split-string
   (shell-command-to-string
    "(systemctl list-units --all --full --no-legend;systemctl list-unit-files --full --no-legend)|while read -r a b; do echo \" $a\";done;"))
  "p-completion candidates for all `systemd' units")

(defvar pcomplete-systemd-user-units
  (split-string
   (shell-command-to-string
    "(systemctl list-units --user --all --full --no-legend;systemctl list-unit-files --user --full --no-legend)|while read -r a b;do echo \" $a\";done;"))
  "p-completion candidates for all `systemd' user units")

(defun pcomplete/systemctl ()
  "Completion rules for the `systemctl' command."
  (pcomplete-here (append pcomplete-systemctl-commands '("--user")))
  (cond ((pcomplete-test "--user")
         (pcomplete-here pcomplete-systemctl-commands)
         (pcomplete-here pcomplete-systemd-user-units))
        (t (pcomplete-here pcomplete-systemd-units))))

;;;; man completion
(defvar pcomplete-man-user-commands
  (split-string
   (shell-command-to-string
    "apropos -s 1 .|while read -r a b; do echo \" $a\";done;"))
  "p-completion candidates for `man' command")

(defun pcomplete/man ()
  "Completion rules for the `man' command."
  (pcomplete-here pcomplete-man-user-commands))

;;;;;;;;;;
;; Keys ;;
;;;;;;;;;;
(advice-add 'eshell-cmpl-initialize :after (lambda ()
                                             (define-key eshell-mode-map [tab] 'completion-at-point)))


(provide 'eshell-config)
