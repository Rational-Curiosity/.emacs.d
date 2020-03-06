;;; vimish-fold-config.el --- Configure and improve vimish fold

;;; Commentary:

;; Usage:
;; (require 'vimish-fold-config)

;;; Code:

;; folding
(require 'vimish-fold)
(set-face-attribute 'vimish-fold-overlay nil
                    :foreground 'unspecified
                    :background "#0f0f0f"
                    :underline t
                    :inherit 'unspecified)
;; (face-spec-set 'vimish-fold-overlay '((t (:background "#0f0f0f" :underline t))))

(defvar fold-threshold 1)

(defmacro deffold (func-name params
                             interactive
                             let*-list
                             start-cond
                             &optional while-cond
                             exclude-start include-end)
  (setq while-cond (or while-cond start-cond))
  (let* ((timesp (memq 'times params))
         (vimish-or-goback
          `(condition-case nil
               (save-excursion
                 (vimish-fold beg end))
             (error (goto-char beg)
                    (if (/= 0 (forward-line))
                        (cl-return-from ,func-name)))))
         (vimish-beg-end
          `(let ((end ,(if include-end '(progn (forward-line)
                                               (1- (point)))
                         '(1- (point)))))
             ,(if (memq 'threshold params)
                  `(if (< threshold (count-lines beg end))
                       ,vimish-or-goback)
                vimish-or-goback)))
         (looking-at-end
          `((while ,while-cond
              (when (/= 0 (forward-line))
                ,vimish-beg-end
                (cl-return-from ,func-name)))
            ,vimish-beg-end)))
    `(defun ,func-name ,params
       ,interactive
       (let* ,let*-list
         (save-excursion
           ,(if timesp
                '(if (< times 0)
                     (goto-char (point-min))
                   (beginning-of-line))
              '(goto-char (point-min)))
           (cl-block ,func-name
             ;; Iterations
             (while ,(if timesp
                         '(/= -1 (cl-decf times))
                       't)
               ;; Looking at begin
               (while (not ,start-cond)
                 (if (/= 0 (forward-line))
                     (cl-return-from ,func-name)))
               ,@(if exclude-start
                     `((if (/= 0 (forward-line))
                           (cl-return-from ,func-name))
                       (let ((beg (point)))
                         ;; Looking at end
                         ,@looking-at-end))
                   `((let ((beg (point)))
                       (if (/= 0 (forward-line))
                           (cl-return-from ,func-name))
                       ;; Looking at end
                       ,@looking-at-end))))))))))

(deffold fold-indent (level threshold)
  (interactive (list (read-number "Level: " 1)
                     (read-number "Threshold: " fold-threshold)))
  ((indentation (* level tab-width)))
  (and (= indentation (current-indentation))
       (not (looking-at-p "[[:space:]]*$")))
  (or (<= indentation (current-indentation))
      (looking-at-p "[[:space:]]*$")
      (char-equal ?\\ (char-before (1- (point))))))

(deffold fold-regexp (regexp times threshold)
  (interactive (list (read-regexp "Regexp: ")
                     (read-number "Times: " -1)
                     (read-number "Threshold: " fold-threshold)))
  nil
  (looking-at-p regexp))

(deffold fold-precomp (&optional threshold)
  (interactive "p")
  ((threshold (or threshold 1)))
  (cond
   ((derived-mode-p 'c-mode 'c++-mode)
    (looking-at-p "[[:space:]]*#"))
   ((derived-mode-p 'python-mode 'elpy-mode)
    (looking-at-p "[[:space:]]*\\(from\\|import\\)")))
  (or (looking-at-p "[[:space:]]*$")
      (let ((char (char-before (1- (point)))))
        (and char (char-equal ?\\ char)))
      (cond
       ((derived-mode-p 'c-mode 'c++-mode)
        (looking-at-p "[[:space:]]*#"))
       ((derived-mode-p 'python-mode 'elpy-mode)
        (looking-at-p "[[:space:]]*\\(from\\|import\\)")))))

(deffold fold-comment (&optional threshold)
  (interactive "p")
  ((threshold (or threshold 1)))
  (cond
   ((derived-mode-p 'lisp-interaction-mode 'emacs-lisp)
    (looking-at-p "[[:space:]]*;"))
   ((derived-mode-p 'c-mode 'c++-mode)
    (looking-at-p "[[:space:]]*/[/*]"))
   ((derived-mode-p 'python-mode 'elpy-mode)
    (looking-at-p "[[:space:]]*#")))
  (or (sp-point-in-comment)
      (cond
       ((derived-mode-p 'lisp-interaction-mode 'emacs-lisp)
        (looking-at-p "[[:space:]]*;"))
       ((derived-mode-p 'c-mode 'c++-mode)
        (looking-at-p "[[:space:]]*/[/*]"))
       ((derived-mode-p 'python-mode 'elpy-mode)
        (looking-at-p "[[:space:]]*#")))))

;; [ cc
(deffold fold-cc-class (times threshold)
  (interactive (list (read-number "Times: " -1)
                     (read-number "Threshold: " fold-threshold)))
  (pos-bracket)
  (if (looking-at "[[:space:]]*class [\0-\377[:nonascii:]]*?{")
      (setq pos-bracket (1- (match-end 0))))
  (and (goto-char pos-bracket)
       (sp-forward-sexp)
       nil)
  t t)

(deffold fold-cc-comment (times threshold)
  (interactive (list (read-number "Times: " -1)
                     (read-number "Threshold: " fold-threshold)))
  (while-function)
  (or (if (looking-at-p "[[:space:]]*/\\*")
          (setq while-function (lambda () (not (if (looking-at-p ".*?\\*/")
                                                 (forward-line 1))))))
      (if (looking-at-p "[[:space:]]*//")
          (setq while-function (lambda () (looking-at-p "[[:space:]]*//")))))
  (funcall while-function))
;; ]

;; [ Scripting
(deffold fold-script-comment (times threshold)
  (interactive (list (read-number "Times: " -1)
                     (read-number "Threshold: " fold-threshold)))
  nil
  (looking-at-p "[[:space:]]*#")
  (looking-at-p "[[:space:]]*\\($\\|#\\)"))
;; ]

;; [ Python
(deffold fold-python-class (times threshold)
  (interactive (list (read-number "Times: " -1)
                     (read-number "Threshold: " fold-threshold)))
  (indentation)
  (and (looking-at-p "[[:space:]]*class ")
       (setq indentation (current-indentation)))
  (or (looking-at-p "[[:space:]]*$")
      (char-equal ?\\ (char-before (1- (point))))
      (< indentation (current-indentation)))
  t)

(deffold fold-python-class-header (times threshold)
  (interactive (list (read-number "Times: " -1)
                     (read-number "Threshold: " fold-threshold)))
  (indentation)
  (and (looking-at-p "[[:space:]]*class ")
       (setq indentation (current-indentation)))
  (or (looking-at-p "[[:space:]]*$")
      (char-equal ?\\ (char-before (1- (point))))
      (and (< indentation (current-indentation))
           (not (looking-at-p "[[:space:]]*\\(@\\|\\(async +\\)?def \\)"))))
  t)

(deffold fold-python-function (times threshold)
  (interactive (list (read-number "Times: " -1)
                     (read-number "Threshold: " fold-threshold)))
  (indentation)
  (and (looking-at-p "[[:space:]]*\\(async +\\)?def ")
       (setq indentation (current-indentation)))
  (or (looking-at-p "[[:space:]]*$")
      (char-equal ?\\ (char-before (1- (point))))
      (if (looking-at "[[:space:]]*\"\"\"[\0-\377[:nonascii:]]*?\"\"\"")
          (goto-char (match-end 0)))
      (if (looking-at "[[:space:]]*'''[\0-\377[:nonascii:]]*?'''")
          (goto-char (match-end 0)))
      (< indentation (current-indentation)))
  t)

(deffold fold-python-import (times threshold)
  (interactive (list (read-number "Times: " -1)
                     (read-number "Threshold: " fold-threshold)))
  (indentation)
  (and (looking-at-p "[[:space:]]*\\(import \\|from \\)")
       (setq indentation (current-indentation)))
  (or (looking-at-p "[[:space:]]*\\($\\|import \\|from \\|except \\|finally:\\)")
      (char-equal ?\\ (char-before (1- (point))))
      (if (looking-at "[[:space:]]*\\(try:[[:space:]]*\\|#[^\n]*\\)\n\\([[:space:]]*\\(#[^\n]*\\)?\n\\)*[[:space:]]*\\(import \\|from \\)")
          (goto-char (match-end 0)))
      (< indentation (current-indentation))))

(deffold fold-python-docstring (times threshold)
  (interactive (list (read-number "Times: " -1)
                     (read-number "Threshold: " fold-threshold)))
  (docstring-delimiter)
  (or (if (looking-at-p "[[:space:]]*'''")
          (setq docstring-delimiter "[^\n]*'''"))
      (if (looking-at-p "[[:space:]]*\"\"\"")
          (setq docstring-delimiter "[^\n]*\"\"\"")))
  (not (looking-at-p docstring-delimiter))
  nil t)
;; ]

;; [ php
(deffold fold-php-class-header (times threshold)
  (interactive (list (read-number "Times: " -1)
                     (read-number "Threshold: " fold-threshold)))
  (pos-bracket)
  (if (looking-at "[[:space:]]*class [\0-\377[:nonascii:]]*?{")
      (setq pos-bracket (save-excursion
                          (goto-char (1- (match-end 0)))
                          (sp-forward-sexp)
                          (point))))
  (or (looking-at-p "[[:space:]]*$")
      (char-equal ?\\ (char-before (1- (point))))
      (and (>= pos-bracket (point))
           (not (if (looking-at-p "[[:space:]]*\\(public +\\|protected +\\|private +\\)?\\(static +\\)?function [\0-\377[:nonascii:]]*?{")
                    (forward-line -1)))))
  t t)

(deffold fold-php-function (times threshold)
  (interactive (list (read-number "Times: " -1)
                     (read-number "Threshold: " fold-threshold)))
  (pos-bracket)
  (if (looking-at "[[:space:]]*\\(public +\\|protected +\\|private +\\)?\\(static +\\)?function [\0-\377[:nonascii:]]*?{")
      (setq pos-bracket (1- (match-end 0))))
  (and (goto-char pos-bracket)
       (sp-forward-sexp)
       nil)
  t t)

(defun fold-php-comment (times threshold)
  (interactive (list (read-number "Times: " -1)
                     (read-number "Threshold: " fold-threshold)))
  (fold-cc-comment times threshold)
  (fold-script-comment times threshold))
;; ]

(defun fold-derived-mode (arg blocks)
  (interactive (list current-prefix-arg
                     (completing-read-multiple
                      "Blocks: "
                      '("class" "defun" "require"
                        "comment" "docstring" "header")
                      nil t)))
  (cond
   ;; list parameter `arg'?
   ((pcase arg
      ('(4) ;; C-u prefix
       (vimish-fold-delete-all)
       t)))
   ;; zero `arg'?
   ((and (numberp arg)
         (= arg 0))
    (call-interactively 'fold-indent))
   ((derived-mode-p 'python-mode 'elpy-mode)
    (setq arg (or arg -1))
    (dolist (b blocks)
      (pcase b
        ("class"
         (fold-python-class arg fold-threshold))
        ("defun"
         (fold-python-function arg fold-threshold))
        ("require"
         (fold-python-import arg fold-threshold))
        ("comment"
         (fold-script-comment arg fold-threshold))
        ("docstring"
         (fold-python-docstring arg fold-threshold))
        ("header"
         (fold-python-class-header arg fold-threshold))
        (_ (message "Case: `%s' not implemented" b)))))
   ((derived-mode-p 'php-mode)
    (setq arg (or arg -1))
    (dolist (b blocks)
      (pcase b
        ("class"
         (fold-cc-class arg fold-threshold))
        ("defun"
         (fold-php-function arg fold-threshold))
        ("comment"
         (fold-php-comment arg fold-threshold))
        ("header"
         (fold-php-class-header arg fold-threshold))
        (_ (message "Case: `%s' not implemented" b)))))
   ;; last case, a lot of progmodes derive from c-mode
   ((derived-mode-p 'c-mode 'c++-mode)
    (fold-indent (or arg -1) fold-threshold)
    (fold-precomp)
    (fold-comment))))

(defun fold-dwim (&optional arg)
  (interactive "P")
  (cond
   ;; exists region?
   (mark-active
    (call-interactively #'vimish-fold))
   ;; exists vimish overlay at point?
   ((cl-block nested-dolist
      (dolist (overlay (overlays-at (point)))
        (when (vimish-fold--vimish-overlay-p overlay)
          (cl-return-from nested-dolist t)))
      nil)
    (if arg
        (vimish-fold-delete)
      (vimish-fold-toggle)))
   ;; exists function or class definition at point?
   ((condition-case nil
        (save-excursion
          (let ((pos (point)))
            (end-of-line)
            (let* ((beg (progn (beginning-of-defun) (point)))
                   (end (progn (end-of-defun) (1- (point)))))
              (if (and (<= beg pos)
                       (< pos end)
                       (goto-char beg)
                       (or (looking-at-p "[[:space:]]*class ")
                           (bounds-of-thing-at-point 'defun))
                       (= 0 (forward-line))
                       (setq beg (point))
                       (< beg end))
                  (if arg
                      (vimish-fold-delete)
                    (vimish-fold beg end))))))
      (error (vimish-fold-next-fold)
             (vimish-fold-toggle)
             t)))
   ;; exists vimish overlays in buffer?
   ((cl-block nested-dolist
      (dolist (overlay (overlays-in (point-min) (point-max)))
        (when (vimish-fold--vimish-overlay-p overlay)
          (cl-return-from nested-dolist t)))
      nil)
    (if arg
        (vimish-fold-delete-all)
      (if (cl-every (lambda (overlay)
                      (eq (overlay-get overlay 'type) 'vimish-fold--folded))
                    (vimish-fold--folds-in (point-min) (point-max)))
          (vimish-fold-unfold-all)
        (vimish-fold-refold-all))))
   ;; else
   (t
    (setq prefix-arg arg)
    (call-interactively 'fold-derived-mode))))

(vimish-fold-global-mode 1)

;;;;;;;;;;
;; Keys ;;
;;;;;;;;;;
(define-key vimish-fold-folded-keymap "n" #'vimish-fold-next-fold)
(define-key vimish-fold-folded-keymap "p" #'vimish-fold-previous-fold)
(define-key vimish-fold-folded-keymap "+" #'vimish-fold-unfold)
(define-key vimish-fold-folded-keymap "*" #'vimish-fold-unfold-all)
(define-key vimish-fold-folded-keymap "d" #'vimish-fold-delete)
(define-key vimish-fold-folded-keymap "D" #'vimish-fold-delete-all)

(global-set-key (kbd "C-c @ +") #'vimish-fold-unfold)
(global-set-key (kbd "C-c @ *") #'vimish-fold-unfold-all)
(global-set-key (kbd "C-c @ -") #'vimish-fold-refold)
(global-set-key (kbd "C-c @ _") #'vimish-fold-refold-all)
(global-set-key (kbd "C-c @ .") #'vimish-fold-toggle)
(global-set-key (kbd "C-c @ :") #'vimish-fold-toggle-all)
(global-set-key (kbd "C-c @ d") #'vimish-fold-delete)
(global-set-key (kbd "C-c @ D") #'vimish-fold-delete-all)
(global-set-key (kbd "C-c @ f") #'vimish-fold)
(global-set-key (kbd "C-c @ M-g") #'vimish-fold-avy)
(global-set-key (kbd "C-c @ p") #'vimish-fold-previous-fold)
(global-set-key (kbd "C-c @ n") #'vimish-fold-next-fold)
(global-set-key (kbd "C-c @ w") #'fold-dwim)
(define-key prog-mode-map (kbd "M-+") #'fold-dwin)


(provide 'vimish-fold-config)
;;; vimish-fold-config.el ends here
