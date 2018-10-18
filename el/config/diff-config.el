;;; diff-config.el --- Configure diff tools

;;; Commentary:

;;; Code:

(require 'ediff)
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

(custom-set-faces
 '(diff-refine-changed 
   ((((class color) (min-colors 88) (background light))
     :background "#888833")
    (((class color) (min-colors 88) (background dark))
     :background "#555511")
    (t :inverse-video t)))
 '(ediff-odd-diff-A ((t (:background "dark slate gray"))))
 '(ediff-odd-diff-B ((t (:background "dark slate gray"))))
 '(ediff-odd-diff-C ((t (:background "dark slate gray"))))
 '(ediff-even-diff-A ((t (:background "dim gray"))))
 '(ediff-even-diff-B ((t (:background "dim gray"))))
 '(ediff-even-diff-C ((t (:background "dim gray"))))
 '(ediff-fine-diff-A ((t (:background "brown"))))
 '(ediff-fine-diff-B ((t (:background "brown"))))
 '(ediff-fine-diff-C ((t (:background "brown"))))
 '(ediff-current-diff-A ((t (:foreground "White" :background "dark green"))))
 '(ediff-current-diff-B ((t (:foreground "White" :background "dark green"))))
 '(ediff-current-diff-C ((t (:foreground "White" :background "dark green")))))

(setq-default ediff-forward-word-function 'forward-char)

(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally
      ediff-diff-ok-lines-regexp
      "^\\([0-9,]+[acd][0-9,]+
?$\\|[<>] \\|---\\|.*Warning *:\\|.*No +newline\\|.*missing +newline\\|.*No +hay +ningún +carácter +de +nueva +línea +al +final +del +fichero\\|^
?$\\)")

(require 'vdiff)
(defun diff-revert-buffer-with-file (&optional arg)
  "Compare the current modified buffer with the saved version.
ARG - `C-u' differ with prompted file.
    - `C-u' `C-u' force revert."
  (interactive "P")
  (cond
   ((equal arg '(16))
    (revert-buffer))
   ((equal arg '(4))
    (let ((diff-switches "-u")) ;; unified diff
      (diff-buffer-with-file (current-buffer))))
   (t
    (vdiff-current-file))))

(defun vdiff-hydra-or-diff (&optional arg)
  (interactive "P")
  (condition-case nil
      (call-interactively 'vdiff-hydra/body)
    (error
     (cond
      ((equal arg '(64))
       (call-interactively 'vdiff-files3))
      ((equal arg '(16))
       (call-interactively 'vdiff-buffers3))
      ((equal arg '(4))
       (call-interactively 'vdiff-files))
      (t
       (call-interactively 'vdiff-buffers)))
     (call-interactively 'vdiff-hydra/body))))

(bind-keys
 ("C-c d R"     . diff-revert-buffer-with-file)
 ("C-c d m"     . vdiff-hydra-or-diff)
 ("C-c d 3 f"   . vdiff-files3)
 ("C-c d f"     . vdiff-files)
 ("C-c d 3 b"   . vdiff-buffers3)
 ("C-c d b"     . vdiff-buffers))


(provide 'diff-config)
;;; diff-config.el ends here
