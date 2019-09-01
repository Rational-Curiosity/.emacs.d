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


(defmacro deffold (func-name params let*-list start-cond while-cond &optional backward-cond last-cond)
  (cond
   ((and backward-cond last-cond)
    `(defun ,func-name ,params
       (interactive "p")
       (let* (,@let*-list)
         (save-excursion
           (goto-char (point-min))
           ;; Fisrt iteration
           ;; Looking at begin
           (while (and (not ,start-cond)
                       (= 0 (forward-line))))
           (unless (= (point) (point-max))
             (let ((beg (point)))
               ;; Looking at end
               (while (and ,while-cond
                           (= 0 (forward-line))))
               (unless ,last-cond
                 (forward-line -1)
                 (while (and ,backward-cond
                             (= 0 (forward-line -1))))
                 (forward-line))
               (let ((end (point)))
                 (when (< threshold (count-lines beg end))
                   (save-excursion
                     (ignore-errors
                       (vimish-fold beg end)))))))
           ;; Rest iterations
           (while (= 0 (forward-line))
             ;; Looking at begin
             (while (and (not ,start-cond)
                         (= 0 (forward-line))))
             (unless (= (point) (point-max))
               (let ((beg (point)))
                 ;; Looking at end
                 (while (and ,while-cond
                             (= 0 (forward-line))))
                 (unless ,last-cond
                   (forward-line -1)
                   (while (and ,backward-cond
                               (= 0 (forward-line -1))))
                   (forward-line))
                 (let ((end (point)))
                   (when (< threshold (count-lines beg end))
                     (save-excursion
                       (ignore-errors
                         (vimish-fold beg end))))))))))))
   (backward-cond
    `(defun ,func-name ,params
       (interactive "p")
       (let* (,@let*-list)
         (save-excursion
           (goto-char (point-min))
           ;; First iteration
           ;; Looking at begin
           (while (and (not ,start-cond)
                       (= 0 (forward-line))))
           (unless (= (point) (point-max))
             (let ((beg (point)))
               ;; Looking at end
               (while (and ,while-cond
                           (= 0 (forward-line))))
               (forward-line -1)
               (while (and ,backward-cond
                           (= 0 (forward-line -1))))
               (forward-line)
               (let ((end (point)))
                 (when (< threshold (count-lines beg end))
                   (save-excursion
                     (ignore-errors
                       (vimish-fold beg end)))))))
           ;; Rest iterations
           (while (= 0 (forward-line))
             ;; Looking at begin
             (while (and (not ,start-cond)
                         (= 0 (forward-line))))
             (unless (= (point) (point-max))
               (let ((beg (point)))
                 ;; Looking at end
                 (while (and ,while-cond
                             (= 0 (forward-line))))
                 (forward-line -1)
                 (while (and ,backward-cond
                             (= 0 (forward-line -1))))
                 (forward-line)
                 (let ((end (point)))
                   (when (< threshold (count-lines beg end))
                     (save-excursion
                       (ignore-errors
                         (vimish-fold beg end))))))))))))
   (t
    `(defun ,func-name ,params
       (interactive "p")
       (let* (,@let*-list)
         (save-excursion
           (goto-char (point-min))
           ;; First iteration
           (while (and (not ,start-cond)
                       (= 0 (forward-line))))
           (unless (= (point) (point-max))
             (let ((beg (point)))
               ;; Looking at end
               (while (and ,while-cond
                           (= 0 (forward-line))))
               (let ((end (point)))
                 (when (< threshold (count-lines beg end))
                   (save-excursion
                     (ignore-errors
                       (vimish-fold beg end)))))))
           ;; Rest iterations
           (while (= 0 (forward-line))
             ;; Looking at begin
             (while (and (not ,start-cond)
                         (= 0 (forward-line))))
             (unless (= (point) (point-max))
               (let ((beg (point)))
                 ;; Looking at end
                 (while (and ,while-cond
                             (= 0 (forward-line))))
                 (let ((end (point)))
                   (when (< threshold (count-lines beg end))
                     (save-excursion
                       (ignore-errors
                         (vimish-fold beg end))))))))))))))

(deffold fold-regexp (regexp &optional threshold)
  ((regexp (cond
            ((stringp regexp) regexp)
            ((symbolp regexp) (symbol-name regexp))
            (t (read-regexp "Regexp"))))
   (threshold (or threshold 1)))
  (looking-at regexp)
  (looking-at regexp))

(deffold fold-indent (level &optional threshold)
  ((indentation (* level tab-width))
   (threshold (or threshold 5)))
  (and (= indentation (current-indentation))
       (not (looking-at "[[:space:]]*$")))
  (or (<= indentation (current-indentation))
      (looking-at "[[:space:]]*$")
      (char-equal ?\\ (char-before (1- (point)))))
  (looking-at "[[:space:]]*$")
  (looking-at "[[:space:]]*[}\\])]"))

(deffold fold-precomp (&optional threshold)
  ((threshold (or threshold 1)))
  (cond
   ((derived-mode-p 'c-mode 'c++-mode)
    (looking-at "[[:space:]]*#"))
   ((derived-mode-p 'python-mode 'elpy-mode)
    (looking-at "[[:space:]]*\\(from\\|import\\)")))
  (or (looking-at "[[:space:]]*$")
      (let ((char (char-before (1- (point)))))
        (and char (char-equal ?\\ char)))
      (cond
       ((derived-mode-p 'c-mode 'c++-mode)
        (looking-at "[[:space:]]*#"))
       ((derived-mode-p 'python-mode 'elpy-mode)
        (looking-at "[[:space:]]*\\(from\\|import\\)"))))
  (looking-at "[[:space:]]*$"))

(deffold fold-comment (&optional threshold)
  ((threshold (or threshold 1)))
  (cond
   ((derived-mode-p 'lisp-interaction-mode 'emacs-lisp)
    (looking-at "[[:space:]]*;"))
   ((derived-mode-p 'c-mode 'c++-mode)
    (looking-at "[[:space:]]*/[/*]"))
   ((derived-mode-p 'python-mode 'elpy-mode)
    (looking-at "[[:space:]]*#")))
  (or (sp-point-in-comment)
      (cond
       ((derived-mode-p 'lisp-interaction-mode 'emacs-lisp)
        (looking-at "[[:space:]]*;"))
       ((derived-mode-p 'c-mode 'c++-mode)
        (looking-at "[[:space:]]*/[/*]"))
       ((derived-mode-p 'python-mode 'elpy-mode)
        (looking-at "[[:space:]]*#")))))


(defun fold-dwim (level)
  (interactive "p")
  (cond
   (mark-active ;; exists region?
    (call-interactively #'vimish-fold))
   ((cl-block nested-dolist ;; exists vimish overlay at point?
      (dolist (overlay (overlays-at (point)))
        (when (vimish-fold--vimish-overlay-p overlay)
          (cl-return-from nested-dolist t)))
      nil)
    (vimish-fold-toggle))
   ((let ((bounds (bounds-of-thing-at-point 'defun)))
      (if bounds ;; exists function definition at point?
          (let ((beg (car bounds))
                (end (cdr bounds))
                (pos (point)))
            (if (and (<= beg pos)
                     (< pos end))
                (vimish-fold beg end))))))
   ((cl-block nested-dolist ;; exists vimish overlays in buffer?
      (dolist (overlay (overlays-in (point-min) (point-max)))
        (when (vimish-fold--vimish-overlay-p overlay)
          (cl-return-from nested-dolist t)))
      nil)
    (vimish-fold-toggle-all))
   (t  ;; else
    (if current-prefix-arg
        (fold-indent (read-number "Level: " 1) (read-number "Threshold: " 1))
      (fold-indent level))
    (fold-precomp)
    (fold-comment))))

;; (defun fold-indent (level &optional threshold)
;;   (interactive "p")
;;   (let ((indentation (* level tab-width))
;;         (threshold (or 5 threshold)))
;;     (save-excursion
;;       (goto-char (point-min))
;;       (while (not (= (point) (point-max)))
;;         ;; Looking at begin
;;         (while (not (or
;;                      (and (= indentation (current-indentation))
;;                           (not (looking-at "[[:space:]]*$")))
;;                      (= (point) (point-max))))
;;           (forward-line))
;;         (unless (= (point) (point-max))
;;           (let ((beg (point)))
;;             ;; Looking at end
;;             (while (and (or (<= indentation (current-indentation))
;;                             (looking-at "[[:space:]]*$")
;;                             (char-equal ?\\ (char-before (1- (point)))))
;;                         (not (= (point) (point-max))))
;;               (forward-line))
;;             (unless (looking-at "[[:space:]]*[}\\])]")
;;               (forward-line -1)
;;               (while (and (looking-at "[[:space:]]*$")
;;                           (not (= (point) (point-min))))
;;                 (forward-line -1))
;;               (forward-line))
;;             (let ((end (point)))
;;               (when (< threshold (count-lines beg end))
;;                 (save-excursion
;;                   (ignore-errors
;;                     (vimish-fold beg end)))
;;                 (forward-line)))))))))

(vimish-fold-global-mode 1)

;;;;;;;;;;
;; Keys ;;
;;;;;;;;;;
(global-set-key (kbd "C-c v f") #'vimish-fold)
(global-set-key (kbd "C-c v +") #'vimish-fold-unfold)
(global-set-key (kbd "C-c v *") #'vimish-fold-unfold-all)
(global-set-key (kbd "C-c v -") #'vimish-fold-refold)
(global-set-key (kbd "C-c v _") #'vimish-fold-refold-all)
(global-set-key (kbd "C-c v .") #'vimish-fold-toggle)
(global-set-key (kbd "C-c v :") #'vimish-fold-toggle-all)
(global-set-key (kbd "C-c v M-g") #'vimish-fold-avy)
(global-set-key (kbd "C-c v d") #'vimish-fold-delete)
(global-set-key (kbd "C-c v D") #'vimish-fold-delete-all)
(global-set-key (kbd "M-P") #'vimish-fold-previous-fold)
(global-set-key (kbd "M-N") #'vimish-fold-next-fold)
(global-set-key (kbd "M-*") #'fold-dwim)


(provide 'vimish-fold-config)
;;; vimish-fold-config.el ends here
