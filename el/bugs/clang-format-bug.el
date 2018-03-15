(defalias 'clang-format--bufferpos-to-filepos
    (lambda (position &optional _quality _coding-system)
      (1- (position-bytes position))))

(defalias 'clang-format--filepos-to-bufferpos
    (lambda (byte &optional _quality _coding-system)
      (byte-to-position (1+ byte))))

(provide 'clang-format-bug)