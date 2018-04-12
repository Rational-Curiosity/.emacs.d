;;; flyspell-config.el --- Configure flyspell and improve it

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'flyspell
;;  (require 'flyspell-config))

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Flyspell configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ispell)
(require 'flyspell)
;;;;;;;;;;;
;; Faces ;;
;;;;;;;;;;;
(set-face-attribute 'flyspell-incorrect nil
                    :underline "red1")
(set-face-attribute 'flyspell-duplicate nil
                    :underline "magenta")

;;;;;;;;;;;;;;;
;; Functions ;;
;;;;;;;;;;;;;;;

;; move point to previous error
;; based on code by hatschipuh at
;; http://emacs.stackexchange.com/a/14912/2017
(defun flyspell-goto-previous-error (arg)
  "Go to ARG previous spelling error."
  (interactive "p")
  (while (not (= 0 arg))
    (let ((pos (point))
          (min (point-min)))
      (if (and (eq (current-buffer) flyspell-old-buffer-error)
               (eq pos flyspell-old-pos-error))
          (progn
            (if (= flyspell-old-pos-error min)
                ;; goto beginning of buffer
                (progn
                  (message "Restarting from end of buffer")
                  (goto-char (point-max)))
              (backward-word 1))
            (setq pos (point))))
      ;; seek the next error
      (while (and (> pos min)
                  (let ((ovs (overlays-at pos))
                        (r '()))
                    (while (and (not r) (consp ovs))
                      (if (flyspell-overlay-p (car ovs))
                          (setq r t)
                        (setq ovs (cdr ovs))))
                    (not r)))
        (backward-word 1)
        (setq pos (point)))
      ;; save the current location for next invocation
      (setq arg (1- arg))
      (setq flyspell-old-pos-error pos)
      (setq flyspell-old-buffer-error (current-buffer))
      (goto-char pos)
      (if (= pos min)
          (progn
            (message "No more miss-spelled word!")
            (setq arg 0))))))

;; [ Cycle languages
(require 'ring)
(defvar flyspell-lang-ring nil)
(let ((langs '("english" "spanish")))
  (setq flyspell-lang-ring (make-ring (length langs)))
  (dolist (elem langs) (ring-insert flyspell-lang-ring elem)))

(defun change-dictionary (dictionary)
  "Change dictionary file inserting DICTIONARY."
  (setq ispell-personal-dictionary
        (cond
         ((string-equal ispell-program-name "aspell")
          (concat "~/.emacs.d/cache/aspell." dictionary ".pws"))
         ((string-equal ispell-program-name "hunspell")
          (concat "~/.emacs.d/cache/hunspell." dictionary ".pws"))))
  (ispell-change-dictionary dictionary))

;(ispell-change-dictionary)
(defun cycle-ispell-languages ()
  "Cycle languages in ring."
  (interactive)
  (let ((lang (ring-ref flyspell-lang-ring -1))
        (dict ispell-current-dictionary))
    (ring-insert flyspell-lang-ring lang)
    (if (equal dict lang)
        (let ((next-lang (ring-ref flyspell-lang-ring -1)))
          (ring-insert flyspell-lang-ring next-lang)
          (change-dictionary next-lang))
      (change-dictionary lang))))
;; ]

;; Selecciona una opción incluso cuando picas fuera del popup
;; (defun flyspell-emacs-popup-textual (event poss word)
;;       "A textual flyspell popup menu."
;;       (require 'popup)
;;       (let* ((corrects (if flyspell-sort-corrections
;;                            (sort (car (cdr (cdr poss))) 'string<)
;;                          (car (cdr (cdr poss)))))
;;              (cor-menu (if (consp corrects)
;;                            (mapcar (lambda (correct)
;;                                      (list correct correct))
;;                                    corrects)
;;                          '()))
;;              (affix (car (cdr (cdr (cdr poss)))))
;;              show-affix-info
;;              (base-menu  (let ((save (if (and (consp affix) show-affix-info)
;;                                          (list
;;                                           (list (concat "Save affix: " (car affix))
;;                                                 'save)
;;                                           '("Accept (session)" session)
;;                                           '("Accept (buffer)" buffer))
;;                                        '(("Save word" save)
;;                                          ("Accept (session)" session)
;;                                          ("Accept (buffer)" buffer)))))
;;                            (if (consp cor-menu)
;;                                (append cor-menu (cons "" save))
;;                              save)))
;;              (menu (mapcar
;;                     (lambda (arg) (if (consp arg) (car arg) arg))
;;                     base-menu)))
;;         (cadr (assoc (popup-menu* menu :scroll-bar t) base-menu))))
(defun spanish-dictionary ()
  "Stablish spanish dictionary."
  (interactive)
  (change-dictionary "spanish"))
(defun english-dictionary ()
  "Stablish english dictionary."
  (interactive)
  (change-dictionary "english"))
;;;;;;;;;;
;; Keys ;;
;;;;;;;;;;
(define-key flyspell-mouse-map (kbd "<C-down-mouse-2>") #'flyspell-correct-word)
(define-key flyspell-mouse-map (kbd "<C-mouse-2>") #'undefined)
(define-key flyspell-mouse-map [down-mouse-2] nil)
(define-key flyspell-mouse-map [mouse-2] nil)
;; Change mouse over help text
(let ((item (aref (aref (symbol-function 'make-flyspell-overlay) 2) 12)))
  (if (and (stringp item)
           (string-equal item "mouse-2: correct word at point"))
      (aset (aref (symbol-function 'make-flyspell-overlay) 2) 12 "C-mouse-2: correct word at point")))

;;(fset 'flyspell-emacs-popup 'flyspell-emacs-popup-textual)
(bind-keys
 ("C-c i ." . cycle-ispell-languages)
 ("C-c i s" . spanish-dictionary)
 ("C-c i e" . english-dictionary))
(defhydra hydra-spell (:foreign-keys warn)
  "SPELL"
  ("C-b" flyspell-buffer "buffer")
  ("C-n" flyspell-goto-next-error "next")
  ("C-p" flyspell-goto-previous-error "previous")
  ("C-c" flyspell-correct-word-before-point "correct")
  ("C-a" flyspell-auto-correct-word "auto")
  ("M-q" nil "quit"))
(bind-keys
 ("C-c i m" . hydra-spell/body))
(define-key flyspell-mode-map (kbd "C-;") 'helm-flyspell-correct)

(provide 'flyspell-config)
;;; flyspell-config.el ends here
