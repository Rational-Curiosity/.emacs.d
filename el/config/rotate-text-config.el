;;; rotate-text-config.el --- Configure rotate text

;;; Commentary:

;; Usage:
;; (require 'rotate-text-config)

;; Code:

(require 'rotate-text)

(add-to-list 'rotate-text-symbols
             '("trace" "debug" "info" "warning" "error" "fatal"))
(add-to-list 'rotate-text-symbols
             '("unsigned" "signed"))
(add-to-list 'rotate-text-symbols
             '("void" "bool" "char" "wchar_t" "short" "int"
               "long" "size_t" "float" "double"))
(add-to-list 'rotate-text-symbols
             '("red" "green" "blue" "black" "white" "orange"
               "yellow" "cyan" "violet" "magenta" "brown"
               "salmon" "golden" "pink"))
(add-to-list 'rotate-text-symbols
             '("==" "!=" "<" ">" "<=" ">="))
(add-to-list 'rotate-text-symbols
             '("&&" "||"))
(add-to-list 'rotate-text-symbols
             '("=" "+=" "-=" "*=" "/=" "%=" "<<=" ">>=" "&="
               "^=" "|="))
(add-to-list 'rotate-text-symbols
             '("static_cast" "dynamic_cast" "const_cast" "reinterpret_cast"))
(add-to-list 'rotate-text-symbols
             '("false" "true"))
(add-to-list 'rotate-text-symbols
             '("None" "False" "True"))


(global-set-key (kbd "M-<up>") #'rotate-text)
(global-set-key (kbd "M-<down>") #'rotate-text-backward)


(provide 'rotate-text-config)
;;; rotate-text-config.el ends here
