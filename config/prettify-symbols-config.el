;;; prettify-symbols-config.el --- Configure prettify-symbols

;;; Commentary:

;; Usage:
;; (require 'prettify-symbols-config)

;;; Code:

;;(global-prettify-symbols-mode)
(with-daemon-after-frame frame
  (when (display-graphic-p frame)
    (setq prettify-symbols-unprettify-at-point 'right-edge)

    (add-hook 'emacs-lisp-mode-hook
              (lambda ()
                (push '("nil" . ?∅) prettify-symbols-alist)
                (prettify-symbols-mode)))

    (add-hook 'c-mode-common-hook
              (lambda ()
                (push '("!"   . ?¬) prettify-symbols-alist)
                (push '("="   . ?≔) prettify-symbols-alist)
                (push '("&&"  . (?\s (Br . Bl) ?\s (Bc . Bc) ?∧)) prettify-symbols-alist)
                (push '("||"  . (?\s (Br . Bl) ?\s (Bc . Bc) ?∨)) prettify-symbols-alist)
                (push '("!="  . (?\s (Br . Bl) ?\s (Bc . Bc) ?≠)) prettify-symbols-alist)
                (push '("<="  . (?\s (Br . Bl) ?\s (Bc . Bc) ?≤)) prettify-symbols-alist)
                (push '(">="  . (?\s (Br . Bl) ?\s (Bc . Bc) ?≥)) prettify-symbols-alist)
                (push '("=="  . (?\s (Br . Bl) ?\s (Bc . Bc) ?≡)) prettify-symbols-alist)
                (push '("->"  . (?\s (Br . Bl) ?\s (Bc . Bc) ?→)) prettify-symbols-alist)
                (push '("::"  . (?\s (Br . Bl) ?\s (Bc . Bc) ?⊃)) prettify-symbols-alist)
                (push '("::~" . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Bc . Br) ?⊃ (Bc . Bl) ?~)) prettify-symbols-alist)
                (push '(">::" . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Bl . Bl) ?> (Bc . Bl) ?⊃)) prettify-symbols-alist)
                (push '("->*" . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Bc . Br) ?→ (Br . Br) ?*)) prettify-symbols-alist)
                (prettify-symbols-mode)))

    (add-hook 'python-mode-hook
              (lambda ()
                (push '("None" . ?∅) prettify-symbols-alist)
                (push '("!"    . ?¬) prettify-symbols-alist)
                (push '("="    . ?≔) prettify-symbols-alist)
                (push '("and"  . (?\s (Br . Bl) ?\s (Br . Bl) ?\s (Bc . Bc) ?∧)) prettify-symbols-alist)
                (push '("or"   . (?\s (Br . Bl) ?\s (Bc . Bc) ?∨)) prettify-symbols-alist)
                (push '("!="   . (?\s (Br . Bl) ?\s (Bc . Bc) ?≠)) prettify-symbols-alist)
                (push '("<="   . (?\s (Br . Bl) ?\s (Bc . Bc) ?≤)) prettify-symbols-alist)
                (push '(">="   . (?\s (Br . Bl) ?\s (Bc . Bc) ?≥)) prettify-symbols-alist)
                (push '("=="   . (?\s (Br . Bl) ?\s (Bc . Bc) ?≡)) prettify-symbols-alist)
                (prettify-symbols-mode)))

    (bind-keys
     ("<f7> t"  . prettify-symbols-mode))))


(provide 'prettify-symbols-config)
;;; prettify-symbols-config.el ends here
