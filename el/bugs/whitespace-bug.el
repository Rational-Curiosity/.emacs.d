;; (move-to-column arg t) problems with whitespace-mode
(with-eval-after-load 'whitespace
  (defun avoid-whitespace-mode-advice (orig-fun column &optional force)
    (if (and force
             (< (current-column) column)
             (bound-and-true-p whitespace-mode))
        (prog2
            (call-interactively #'whitespace-mode)
            (funcall orig-fun column force)
          (call-interactively #'whitespace-mode))
      (funcall orig-fun column force)))

  (advice-add 'move-to-column :around #'avoid-whitespace-mode-advice))
