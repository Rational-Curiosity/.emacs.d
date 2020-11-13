;;; prettify-symbols-config.el --- Configure prettify-symbols

;;; Commentary:

;; Usage:
;; (require 'prettify-symbols-config)

;;; Code:

;;(global-prettify-symbols-mode)

(require 'config-lib)

(eval-and-when-daemon frame
    (setq prettify-symbols-unprettify-at-point 'right-edge)

    (add-hook 'emacs-lisp-mode-hook
              (lambda ()
                (if (display-graphic-p)
                    (progn
                      (cl-delete '("lambda" . 995) prettify-symbols-alist)
                      (push '("lambda" . (?\s (Br . Bl) ?\s (Br . Bl) ?λ (Br . Bl) ?· (Br . Bl) ?\s (Br . Bl) ?\s)) prettify-symbols-alist)
                      (push '("nil" . (?\s (Br . Bl) ?∅ (Br . Bl) ?\s)) prettify-symbols-alist))
                  (push '("nil" . ?∅) prettify-symbols-alist))
                (prettify-symbols-mode)))

    (add-hook 'c-mode-common-hook
              (lambda ()
                (push '("!"     . ?¬) prettify-symbols-alist)
                (push '("="     . ?≔) prettify-symbols-alist)
                (if (display-graphic-p)
                    (progn
                      (push '("true"  . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Bc . Bc) ?✓)) prettify-symbols-alist)
                      (push '("false" . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Bc . Bc) ?✘)) prettify-symbols-alist)
                      (push '("&&"    . (?\s (Br . Bl) ?\s (Bc . Bc) ?∧)) prettify-symbols-alist)
                      (push '("||"    . (?\s (Br . Bl) ?\s (Bc . Bc) ?∨)) prettify-symbols-alist)
                      (push '("->"    . (?\s (Br . Bl) ?\s (Bc . Bc) ?→)) prettify-symbols-alist)
                      (push '("::"    . (?\s (Br . Bl) ?\s (Bc . Bc) ?⊃)) prettify-symbols-alist)
                      (push '("::~"   . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Bc . Br) ?⊃ (Bc . Bl) ?~)) prettify-symbols-alist)
                      (push '(">::"   . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Bl . Bl) ?> (Bc . Bl) ?⊃)) prettify-symbols-alist)
                      (push '("->*"   . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Bc . Br) ?→ (Br . Br) ?*)) prettify-symbols-alist))
                  (push '("true"  . ?✓) prettify-symbols-alist)
                  (push '("false" . ?✘) prettify-symbols-alist)
                  (push '("&&"    . ?∧) prettify-symbols-alist)
                  (push '("||"    . ?∨) prettify-symbols-alist)
                  (push '("->"    . ?→) prettify-symbols-alist)
                  (push '("::"    . ?⊃) prettify-symbols-alist))))

    (add-hook 'c-mode-hook
              (lambda ()
                (if (display-graphic-p)
                    (progn
                      (push '("!="    . (?\s (Br . Bl) ?\s (Bc . Bc) ?≠)) prettify-symbols-alist)
                      (push '("<="    . (?\s (Br . Bl) ?\s (Bc . Bc) ?≤)) prettify-symbols-alist)
                      (push '(">="    . (?\s (Br . Bl) ?\s (Bc . Bc) ?≥)) prettify-symbols-alist)
                      (push '("=="    . (?\s (Br . Bl) ?\s (Bc . Bc) ?≡)) prettify-symbols-alist))
                  (push '("!="    . ?≠) prettify-symbols-alist)
                  (push '("<="    . ?≤) prettify-symbols-alist)
                  (push '(">="    . ?≥) prettify-symbols-alist)
                  (push '("=="    . ?≡) prettify-symbols-alist))
                (prettify-symbols-mode)))

    (add-hook 'c++-mode-hook
              (lambda ()
                (if (display-graphic-p)
                    (progn
                      (push '("!="    . (?\s (Br . Bl) ?\s (Bc . Bc) ?≠)) prettify-symbols-alist)
                      (push '("<="    . (?\s (Br . Bl) ?\s (Bc . Bc) ?≤)) prettify-symbols-alist)
                      (push '(">="    . (?\s (Br . Bl) ?\s (Bc . Bc) ?≥)) prettify-symbols-alist)
                      (push '("=="    . (?\s (Br . Bl) ?\s (Bc . Bc) ?≡)) prettify-symbols-alist))
                  (push '("!="    . ?≠) prettify-symbols-alist)
                  (push '("<="    . ?≤) prettify-symbols-alist)
                  (push '(">="    . ?≥) prettify-symbols-alist)
                  (push '("=="    . ?≡) prettify-symbols-alist))
                (prettify-symbols-mode)))

    (add-hook 'php-mode-hook
              (lambda ()
                (if (display-graphic-p)
                    (progn
                      (push '("!=="   . (?\s (Br . Bl) ?≠ (Br . Bl) ?\s)) prettify-symbols-alist)
                      (push '("<=="   . (?\s (Br . Bl) ?≤ (Br . Bl) ?\s)) prettify-symbols-alist)
                      (push '(">=="   . (?\s (Br . Bl) ?≥ (Br . Bl) ?\s)) prettify-symbols-alist)
                      (push '("==="   . (?\s (Br . Bl) ?≡ (Br . Bl) ?\s)) prettify-symbols-alist)
                      (push '("!="    . (?\s (Br . Bl) ?\s (Bc . Bc) ?≉)) prettify-symbols-alist)
                      (push '("<="    . (?\s (Br . Bl) ?\s (Bc . Bc) ?≲)) prettify-symbols-alist)
                      (push '(">="    . (?\s (Br . Bl) ?\s (Bc . Bc) ?≳)) prettify-symbols-alist)
                      (push '("=="    . (?\s (Br . Bl) ?\s (Bc . Bc) ?≈)) prettify-symbols-alist))
                  (push '("!=="   . ?≠) prettify-symbols-alist)
                  (push '("<=="   . ?≤) prettify-symbols-alist)
                  (push '(">=="   . ?≥) prettify-symbols-alist)
                  (push '("==="   . ?≡) prettify-symbols-alist)
                  (push '("!="    . ?≉) prettify-symbols-alist)
                  (push '("<="    . ?≲) prettify-symbols-alist)
                  (push '(">="    . ?≳) prettify-symbols-alist)
                  (push '("=="    . ?≈) prettify-symbols-alist))
                (prettify-symbols-mode)))

    (add-hook 'js-mode-hook
              (lambda ()
                (if (display-graphic-p)
                    (progn
                      (push '("!=="   . (?\s (Br . Bl) ?≠ (Br . Bl) ?\s)) prettify-symbols-alist)
                      (push '("<=="   . (?\s (Br . Bl) ?≤ (Br . Bl) ?\s)) prettify-symbols-alist)
                      (push '(">=="   . (?\s (Br . Bl) ?≥ (Br . Bl) ?\s)) prettify-symbols-alist)
                      (push '("==="   . (?\s (Br . Bl) ?≡ (Br . Bl) ?\s)) prettify-symbols-alist)
                      (push '("!="    . (?\s (Br . Bl) ?\s (Bc . Bc) ?≉)) prettify-symbols-alist)
                      (push '("<="    . (?\s (Br . Bl) ?\s (Bc . Bc) ?≲)) prettify-symbols-alist)
                      (push '(">="    . (?\s (Br . Bl) ?\s (Bc . Bc) ?≳)) prettify-symbols-alist)
                      (push '("=="    . (?\s (Br . Bl) ?\s (Bc . Bc) ?≈)) prettify-symbols-alist))
                  (push '("!=="   . ?≠) prettify-symbols-alist)
                  (push '("<=="   . ?≤) prettify-symbols-alist)
                  (push '(">=="   . ?≥) prettify-symbols-alist)
                  (push '("==="   . ?≡) prettify-symbols-alist)
                  (push '("!="    . ?≉) prettify-symbols-alist)
                  (push '("<="    . ?≲) prettify-symbols-alist)
                  (push '(">="    . ?≳) prettify-symbols-alist)
                  (push '("=="    . ?≈) prettify-symbols-alist))
                (prettify-symbols-mode)))

    (add-hook 'python-mode-hook
              (lambda ()
                (push '("!"     . ?¬) prettify-symbols-alist)
                (push '("="     . ?≔) prettify-symbols-alist)
                (if (display-graphic-p)
                    (progn
                      (cl-delete '("lambda" . 995) prettify-symbols-alist)
                      (push '("lambda" . (?\s (Br . Bl) ?\s (Br . Bl) ?λ (Br . Bl) ?· (Br . Bl) ?\s (Br . Bl) ?\s)) prettify-symbols-alist)
                      (push '("True"   . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Bc . Bc) ?✓)) prettify-symbols-alist)
                      (push '("False"  . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Bc . Bc) ?✘)) prettify-symbols-alist)
                      (push '("None"   . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Br . Bl) ?\s (Bc . Bc) ?∅)) prettify-symbols-alist)
                      (push '("and"    . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Bc . Bc) ?∧)) prettify-symbols-alist)
                      (push '("or"     . (?\s (Br . Bl) ?\s (Bc . Bc) ?∨)) prettify-symbols-alist)
                      (push '("!="     . (?\s (Br . Bl) ?\s (Bc . Bc) ?≠)) prettify-symbols-alist)
                      (push '("<="     . (?\s (Br . Bl) ?\s (Bc . Bc) ?≤)) prettify-symbols-alist)
                      (push '(">="     . (?\s (Br . Bl) ?\s (Bc . Bc) ?≥)) prettify-symbols-alist)
                      (push '("=="     . (?\s (Br . Bl) ?\s (Bc . Bc) ?≡)) prettify-symbols-alist))
                  (push '("True"  . ?✓) prettify-symbols-alist)
                  (push '("False" . ?✘) prettify-symbols-alist)
                  (push '("None"  . ?∅) prettify-symbols-alist)
                  (push '("and"   . ?∧) prettify-symbols-alist)
                  (push '("or"    . ?∨) prettify-symbols-alist)
                  (push '("!="    . ?≠) prettify-symbols-alist)
                  (push '("<="    . ?≤) prettify-symbols-alist)
                  (push '(">="    . ?≥) prettify-symbols-alist)
                  (push '("=="    . ?≡) prettify-symbols-alist))
                (prettify-symbols-mode)))

    (global-set-key (kbd "M-s M-t p") #'prettify-symbols-mode))


(provide 'prettify-symbols-config)
;;; prettify-symbols-config.el ends here
