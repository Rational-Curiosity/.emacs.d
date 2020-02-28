(with-eval-after-load 'gud-config
  (add-hook 'gdb-mode-hook (lambda () (gud-basic-call "set print sevenbit-strings off"))))
