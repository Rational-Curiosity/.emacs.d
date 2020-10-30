(with-eval-after-load 'ace-window
  (when (bug-check-function-bytecode
         'aw-update
         "xInFGBkaxsfIIAsiySIrhw==")
    (defun aw-update ()
      "Update ace-window-path window parameter for all windows.

Ensure all windows are labeled so the user can select a specific
one, even from the set of windows typically ignored when making a
window list."
      (unless prefix-arg        ;; +
        (let (;; (aw-ignore-on) ;; -
              (aw-ignore-current)
              (ignore-window-parameters t))
          (avy-traverse
           (avy-tree (aw-window-list) aw-keys)
           (lambda (path leaf)
             (set-window-parameter
              leaf 'ace-window-path
              (propertize
               (apply #'string (reverse path))
               'face 'aw-mode-line-face)))))))))
