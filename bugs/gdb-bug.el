(add-hook 'gdb-mode-hook (lambda () (gud-basic-call "set print sevenbit-strings off")))

(provide 'gdb-bug)

