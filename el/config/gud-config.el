;;; gud-config.el --- Configure gud

;;; Commentary:

;; Usage:
;; (require 'gud-config)

;;; Code:

(message "Importing gud-config")
(with-eval-after-load 'gud
;; ;; (setq gud-gdb-command-name "gdb --annotate=3 --fullname")

;; ;; [ <color> Add color to the current GUD line (by google)
;; ;; (defvar gud-overlay
;; ;;   (let* ((ov (make-overlay (point-min) (point-min))))
;; ;;     (overlay-put ov 'face 'secondary-selection)
;; ;;     ov)
;; ;;   "Overlay variable for GUD highlighting.")

;; ;; (defadvice gud-display-line (after my-gud-highlight act)
;; ;;            "Highlight current line."
;; ;;            (let* ((ov gud-overlay)
;; ;;                   (bf (gud-find-file true-file)))
;; ;;              (save-excursion
;; ;;                  (set-buffer bf)
;; ;;                    (move-overlay ov (line-beginning-position) (line-end-position)
;; ;;                                    (current-buffer)))))

;; ;; (defun gud-kill-buffer ()
;; ;;   (if (eq major-mode 'gud-mode)
;; ;;     (delete-overlay gud-overlay)))

;; ;; (add-hook 'kill-buffer-hook 'gud-kill-buffer)
;; ;; ] <color>


;; =================================
;; PDB configuration
;; =================================
  (setq gud-pdb-command-name "pdb3"))
;; =================================
;; GDB configuration
;; =================================
;;;;;;;;;;
;; Bugs ;;
;;;;;;;;;;
(require 'gdb-bug)
;;;;;;;;;;
;;;;;;
(with-eval-after-load 'gdb-mi

;; Dedicated windows except source window
  (defun gdb-dedicated-windows ()
    (dolist (window (window-list))
      (when (and (eq 0 (string-match "*gud\\|*stack\\|*locals\\|*registers\\|*input/output\\|*breakpoints" (buffer-name (window-buffer window))))
                 (not (buffer-file-name (window-buffer window))))
        (set-window-dedicated-p window t))))
  (advice-add  'gdb-setup-windows :after #'gdb-dedicated-windows)

;; Window options
  (setq gdb-many-windows t
        gdb-use-separate-io-buffer t)

  (add-hook 'gdb-mode-hook 'gud-tooltip-mode)
;;(add-hook 'gdb-mode-hook '(lambda () (require 'gdb-highlight)))


;; [ <history> cycle command history
;; (add-hook 'gud-mode-hook
;;           '(lambda ()
;;             (local-set-key [home]        ; move to beginning of line, after prompt
;;              'comint-bol)
;;             (local-set-key [up]          ; cycle backward through command history
;;              '(lambda () (interactive)
;;                (if (comint-after-pmark-p)
;;                    (comint-previous-input 1)
;;                    (previous-line 1))))
;;             (local-set-key [down]        ; cycle forward through command history
;;              '(lambda () (interactive)
;;                (if (comint-after-pmark-p)
;;                    (comint-next-input 1)
;;                  (forward-line 1))))))
;; ] <history>

;;;;;;;;;;;;;;;;;;
;; New commands ;;
;;;;;;;;;;;;;;;;;;

  (defun gdb-new-commands (command-line)
    (gud-def gud-args "info args" "a" "Show args variables.")
    (gud-def gud-kill "kill" "k" "Kill running process.")
    (gud-def gud-quit "quit" "q" "Quit gdb."))
  (advice-add 'gdb :after #'gdb-new-commands)

;;;;;;;;;;
;; Keys ;;
;;;;;;;;;;

  (defhydra hydra-gud (:foreign-keys run);(gud-minor-mode-map "C-x C-a" :foreign-keys run)
    "GUD"
    ("<" gud-up "up")
    (">" gud-down "down")
    ("C-b" gud-break "break")
    ("C-d" gud-remove "remove")
    ("C-f" gud-finish "finish")
    ("C-j" gud-jump "jump")
    ("C-l" gud-refresh "refresh")
    ("C-n" gud-next "next")
    ("C-p" gud-print "print")
    ("C-c" gud-cont "continue")
    ("C-r" gud-run "run")
    ("C-s" gud-step "step")
    ;;("C-t" gud-tbreak "tbreak")
    ("C-u" gud-until "until")
    ("C-w" gud-watch "watch")
    ("C-a" gud-args "args")
    ("C-t" gud-tooltip-mode "tooltip")
    ("C-k" gud-kill "kill")
    ("C-q" gud-quit "quit" :color blue) ; blue color exec and quit hydra
    ("M-q" nil ""))
  (bind-keys :map gud-minor-mode-map
             ("C-x C-a m" . hydra-gud/body)
             ("C-c C-t" . gud-tooltip-mode)))

(provide 'gud-config)
;;; gud-config.el ends here
