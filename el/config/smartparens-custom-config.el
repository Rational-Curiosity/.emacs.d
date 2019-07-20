;;; smartparens-custom-config.el --- Configure and improve smartparens

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'smartparens
;;   (require 'smartparens-custom-config))
;; or:
;; (with-eval-after-load 'smartparens-custom-config
;;   )
;; never:
;; (require 'smartparens-custom-config)

;; Do not include in this file:
;; (require 'smartparens)

;;; Code:

(message "Importing smartparens-custom-config")
(setcar (cdr (assq 'smartparens-mode minor-mode-alist)) "")

(require 'smartparens-org)
(setq ;;sp-autoinsert-pair nil
 sp-highlight-pair-overlay nil)


(defun my-open-block-c-mode (id action context)
  "Insert a c block of code when ID ACTION CONTEXT."
  (let* ((current-pos (point))
         (next-char (char-after (1+ current-pos)))
         (pre-pre-char (char-after (- current-pos 3))))
    (when (and
           next-char pre-pre-char
           (eq action 'insert)
           (eq context 'code)
           (char-equal 10 next-char)
           (or (char-equal ?\) pre-pre-char)
               (save-excursion
                 (beginning-of-line)
                 (looking-at "[[:space:]]*..$"))))
           (indent-according-to-mode)
           (newline)
           (newline)
           (indent-according-to-mode)
           (forward-line -1)
           (indent-according-to-mode))))
  
(defun my-double-angle-c-mode (id action context)
  "Delete closed angles when ID ACTION CONTEXT."
  (let* ((current-pos (point))
         (next-char (char-after current-pos))
         (next-next-char (char-after (1+ current-pos)))
         (pre-char (char-before (1- current-pos)))
         (pre-pre-char (char-before (- current-pos 2))))
    (when (and
           next-char next-next-char pre-char pre-pre-char
           (eq action 'insert)
           (eq context 'code)
           (char-equal ?> next-char)
           (char-equal ?> next-next-char)
           (char-equal ?< pre-char)
           (not (char-equal ?< pre-pre-char)))
      (delete-char 2))))

(defun my-double-angle-post (id action context)
  "Unwrap angles and insert single angle when ID ACTION CONTEXT."
  (when (and
         (eq action 'insert)
         (eq context 'code))
    (sp-unwrap-sexp)
    (insert "<")
    ;; (when (bound-and-true-p rainbow-delimiters-mode)
    ;;   (rainbow-delimiters-mode-disable)
    ;;   (rainbow-delimiters-mode-enable))
    ))

(defun my-double-angle-p (id action context)
  "Check whether a double angle is a c++ operator when ID ACTION CONTEXT."
  (let* ((current-pos (point))
         (pre-char (char-before (1- current-pos)))
         (post-char (char-after current-pos)))
    (if (and
         (eq context 'code)
         (or (and pre-char (char-equal ?< pre-char))
             (and post-char (char-equal ?< post-char))))
        t
      nil)))

(defun my-pre-text-code-p (id action context)
  "Check whether precesor is text when ID ACTION CONTEXT."
  (if (eq context 'code) ;; 'comment 'string
      (let ((pos (1- (point))))
        (if (< 0 pos)
            (let ((char (char-before pos)))
              (if char
                  (if (memq (get-char-code-property char 'general-category)
                            '(Ll Lu Lo Lt Lm Mn Mc Me Nl))
                      t
                    nil)
                t))
          t))
    t))

(defun my-c-include-line-p (id action context)
  "Check whether current line is an include when ID ACTION CONTEXT."
  (if (eq context 'code)
      (save-excursion
        (beginning-of-line)
        (if (looking-at "# *include")
            t
          nil))
    nil))

(defun remove-c-<-as-paren-syntax-backward ()
  "Remove wrong colored angle."
  (interactive)
  (let ((pos (point)))
    (while (<= 0 pos)
      (when (eq (get-char-property pos 'category) 'c-<-as-paren-syntax)
        (remove-text-properties pos (1+ pos) '(category nil))
        (setq pos 0))
      (cl-decf pos))))

(defun my-org-not-abbrev-p (id action context)
  "Check whether current line isn't an abbrev when ID ACTION CONTEXT."
  (if (eq context 'code)
      (save-excursion
        (beginning-of-line)
        (if (looking-at "[\t ]*<$")
            nil
          t))
    nil))

(sp-pair "<" ">" :actions '(wrap insert autoskip))
;;(sp-local-pair 'c++-mode "<" nil :when '(sp-in-comment-p))
(sp-local-pair 'shell-script-mode "<" nil :post-handlers '(("[d1]" "SPC")))
(sp-local-pair 'lisp-mode "'" nil :actions nil)
(sp-local-pair 'common-lisp-mode "'" nil :actions nil)
(sp-local-pair 'lisp-interaction-mode "'" nil :actions nil)
(sp-local-pair 'emacs-lisp-mode "'" nil :actions nil)
(sp-with-modes '(org-mode)
  (sp-local-pair "<" nil :post-handlers '(("[d1]" "<") ("[d1]" "SPC"))
                 :when '(my-org-not-abbrev-p)))
(sp-with-modes '(c-mode c++-mode)
  (sp-local-pair "{" nil
                 :post-handlers '(:add my-open-block-c-mode))
  (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC") ("* ||\n[i]" "RET")))
  (sp-local-pair "<" nil :post-handlers '(("[d1]" "<") ("[d1]" "SPC"))
                 :when '(my-c-include-line-p my-pre-text-code-p)))

(defhydra hydra-sp-change (:foreign-keys run)
  "SP"
  ("C-t"            sp-transpose-hybrid-sexp "tr")
  ("S-<left>"       sp-backward-slurp-sexp "←(")
  ("S-<right>"      sp-backward-barf-sexp "(→")
  ("C-<left>"       sp-forward-barf-sexp "←)")
  ("C-<right>"      sp-slurp-hybrid-sexp ")→")
  ("C-<backspace>"  sp-backward-unwrap-sexp "(-)←")
  ("C-<delete>"     sp-unwrap-sexp "(-)")
  ("C-s"            sp-swap-enclosing-sexp "swap")
  ("\""  (lambda () (interactive) (sp-rewrap-sexp '("\"" . "\""))))
  ("\\\""  (lambda () (interactive) (sp-rewrap-sexp '("\\\"" . "\\\""))))
  ("'"  (lambda () (interactive) (sp-rewrap-sexp '("'" . "'"))))
  ("`"  (lambda () (interactive) (sp-rewrap-sexp '("`" . "`"))))
  ("("  (lambda () (interactive) (sp-rewrap-sexp '("(" . ")"))))
  ("["  (lambda () (interactive) (sp-rewrap-sexp '("[" . "]"))))
  ("{"  (lambda () (interactive) (sp-rewrap-sexp '("{" . "}"))))
  ("<"  (lambda () (interactive) (sp-rewrap-sexp '("<" . ">"))))
  ("M-q" nil "quit"))

(defun kill-to-end-of-sexp ()
  "Delete forward sexp region with kill."
  (interactive)
  (set-mark (point))
  (sp-end-of-sexp)
  (kill-region (point) (mark)))

(defun kill-to-begin-of-sexp ()
  "Delete backward sexp region with kill."
  (interactive)
  (set-mark (point))
  (sp-beginning-of-sexp)
  (kill-region (point) (mark)))

(defun toggle-sp-angle-pair ()
  "Toggle angle as pair."
  (interactive)
  (if (member "<" (mapcar (lambda (x) (plist-get x :open)) sp-local-pairs))
      (sp-local-pair major-mode "<" nil :actions nil)
    (sp-local-pair major-mode "<" ">" :actions '(wrap insert autoskip)
                   :post-handlers '(("[d1]" "<") ("[d1]" "SPC"))
                   :when '(my-c-include-line-p my-pre-text-code-p))))

;; (defun sp-dwim-of-previous-sexp ()
;;   (interactive)
;;   (let ((to-beg (- (point) (save-excursion (sp-beginning-of-sexp) (point))))
;;         (to-end (- (save-excursion (sp-end-of-sexp) (point)) (point))))
;;     (if (<= to-beg to-end)
;;         (sp-beginning-of-previous-sexp)
;;       (sp-end-of-previous-sexp))))

;; (defun sp-dwim-of-next-sexp ()
;;   (interactive)
;;   (let ((to-beg (- (point) (save-excursion (sp-beginning-of-sexp) (point))))
;;         (to-end (- (save-excursion (sp-end-of-sexp) (point)) (point))))
;;     (if (<= to-beg to-end)
;;         (sp-beginning-of-next-sexp)
;;       (sp-end-of-next-sexp))))

(defun sp-dwim-beginning-of-sexp (&optional arg)
  "Smart beginning of sexp ARG times."
  (interactive "^P")
  (when (= (point) (progn (sp-beginning-of-sexp arg) (point)))
    (sp-beginning-of-previous-sexp arg)))

(defun sp-dwim-end-of-sexp (&optional arg)
  "Smart end of sexp ARG times."
  (interactive "^P")
  (when (= (point) (progn (sp-end-of-sexp arg) (point)))
    (sp-end-of-next-sexp arg)))


(defun sp-local-equal-length (str)
  (let ((pos (point))
        (len (length str))
        (it 0)
        (check))
    (while (and (< 0 len)
                (not (set 'check
                          (string-equal
                           str
                           (buffer-substring-no-properties
                            (- pos len)
                            (min (+ pos it) (point-max)))))))
      (cl-decf len)
      (cl-incf it))
    (if check
        len
      nil)))

(defun sp-unwrap-sexp-lc ()
  (interactive)
  (let ((ends (sort (mapcar 'cdr sp-pair-list) (lambda (x y) (> (length x) (length y)))))
        (check))
    (while (and ends
                (not (set 'check (sp-local-equal-length (pop ends))))))
    (if check (left-char check)))
  (call-interactively 'sp-unwrap-sexp))

(defun sp-rewrap-sexp-lc ()
  (interactive)
  (let ((ends (sort (mapcar 'cdr sp-pair-list) (lambda (x y) (> (length x) (length y)))))
        (check))
    (while (and ends
                (not (set 'check (sp-local-equal-length (pop ends))))))
    (if check (left-char check)))
  (call-interactively 'sp-rewrap-sexp))

;;;;;;;;;;
;; Keys ;;
;;;;;;;;;;
(bind-keys
 ("S-<left>"            . sp-backward-sexp)
 ("S-<right>"           . sp-forward-sexp)
 ("M-a"                 . sp-backward-up-sexp)
 ("S-<down>"            . sp-down-sexp)
 ("C-S-<left>"          . sp-dwim-beginning-of-sexp)
 ("C-S-<right>"         . sp-dwim-end-of-sexp)
 ("M-e"                 . sp-up-sexp)
 ("C-S-<down>"          . sp-down-sexp)
 ("C-S-<delete>"        . kill-to-end-of-sexp)
 ("M-s <delete>"        . kill-to-end-of-sexp)
 ("C-S-<insert>"        . kill-to-begin-of-sexp)
 ("M-s <insert>"        . kill-to-begin-of-sexp)
 ("C-S-<home>"          . sp-forward-barf-sexp)
 ("C-S-<end>"           . sp-slurp-hybrid-sexp)
 ("C-S-<prior>"         . sp-backward-slurp-sexp)
 ("C-S-<next>"          . sp-backward-barf-sexp)
 ("C-)"                 . sp-unwrap-sexp-lc)
 ("C-("                 . sp-rewrap-sexp-lc)
 ("C-\""                . sp-swap-enclosing-sexp)
 ("C-S-<return>"        . sp-split-sexp)
 ("C-c ( m"             . hydra-sp-change/body)
 ("C-c ( <"             . remove-c-<-as-paren-syntax-backward)
 ("<f7> <"              . toggle-sp-angle-pair)
 ("<f7> ("              . smartparens-mode))



(provide 'smartparens-custom-config)
;;; smartparens-custom-config ends here
