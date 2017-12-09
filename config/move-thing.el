;;; move-thing.el --- Move words and sexps with cursors

;;; Commentary:

;; Usage:
;; (require 'move-thing)

;; THING should be a symbol specifying a type of syntactic entity.
;; Possibilities include ‘symbol’, ‘list’, ‘sexp’, ‘defun’,
;; ‘filename’, ‘url’, ‘email’, ‘word’, ‘sentence’, ‘whitespace’,
;; ‘line’, and ‘page’.

;;; Code:
(require 'config-lib)
(require 'rect)
(require 'smartparens)
(require 'ring)
(fset 'mt-bounds-of-thing-at-point-helper
      (if (require 'thingatpt+ nil t)
          #'tap-bounds-of-thing-at-point
        #'bounds-of-thing-at-point))


(defgroup move-thing ()
  "Move thing minor mode."
  :group 'editing
  :prefix "mt-")

(defcustom move-thing-mode-line
  '(:eval (concat "'" (cdr (assoc mt-from-thing mt-things))
                  "'" (cdr (assoc mt-to-thing mt-things))))
  "Show current selected thing."
  :group 'move-thing
  :risky t
  :type 'sexp)

(defvar move-thing-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-C-<up>") 'mt-move-up)
    (define-key map (kbd "M-C-<down>") 'mt-move-down)
    (define-key map (kbd "M-C-<left>") 'mt-move-left)
    (define-key map (kbd "M-C-<right>") 'mt-move-right)
    (define-key map (kbd "C-<up>") 'mt-up)
    (define-key map (kbd "C-<down>") 'mt-down)
    (define-key map (kbd "C-<left>") 'mt-backward)
    (define-key map (kbd "C-<right>") 'mt-forward)
    (define-key map (kbd "M-C-f") 'mt-cycle-things)
    (define-key map (kbd "M-C-g") 'mt-cycle-to-things)
    map))

(define-minor-mode move-thing-mode
  "Toggle Move thing mode."
  :init-value nil
  :lighter move-thing-mode-line
  :group 'move-thing
  :keymap move-thing-mode-map
  :global t)


(defvar mt-things '((symbol . "s") (sexp . "e") (line . "l") (word . "w")))

(defun mt-bounds-of-thing-at-point (thing)
  (let ((bounds (mt-bounds-of-thing-at-point-helper thing)))
    (if bounds
        bounds
      (error "Not %s found" thing))))


;; [ from
(defvar mt-from-thing-ring nil)
(let ((from-things mt-things))
  (set 'mt-from-thing-ring (make-ring (length from-things)))
  (dolist (elem from-things) (ring-insert mt-from-thing-ring (car elem))))
(make-variable-buffer-local 'mt-from-thing-ring)

(defvar mt-from-thing (ring-ref mt-from-thing-ring 0))
(make-variable-buffer-local 'mt-from-thing)

(defun mt-cycle-from-things ()
  "Cycle from-things in ring."
  (let ((from-thing (ring-ref mt-from-thing-ring -1)))
    (ring-insert mt-from-thing-ring from-thing)
    (setq mt-from-thing from-thing
          mt-to-thing from-thing)
    from-thing))
;; ]

(defun mt-cycle-things ()
  "Cycle things in ring."
  (interactive)
  (if (not (eql mt-from-thing mt-to-thing))
      (while (not (eql mt-from-thing mt-to-thing))
        (mt-cycle-to-things))
    (let ((init-thing (ring-ref mt-from-thing-ring 0))
          current-thing found)
      (while (not (or found
                      (eql init-thing current-thing)))
        (setq found t
              current-thing (mt-cycle-from-things))
        (condition-case nil
            (let ((bounds (mt-bounds-of-thing-at-point current-thing)))
              (pulse-momentary-highlight-region (car bounds) (cdr bounds)))
          (error (set 'found nil)))))))

;; [ to
(defvar mt-to-thing-ring nil)
(let ((to-things mt-things))
  (set 'mt-to-thing-ring (make-ring (length to-things)))
  (dolist (elem to-things) (ring-insert mt-to-thing-ring (car elem))))
(make-variable-buffer-local 'mt-to-thing-ring)

(defvar mt-to-thing (ring-ref mt-to-thing-ring 0))
(make-variable-buffer-local 'mt-to-thing)

(defun mt-cycle-to-things ()
  "Cycle to-things in ring."
  (interactive)
  (let ((to-thing (ring-ref mt-to-thing-ring -1)))
    (ring-insert mt-to-thing-ring to-thing)
    (set 'mt-to-thing to-thing)))
;; ]


(defun mt-insert-rectangle (rectangle)
  "Insert text of RECTANGLE with upper left corner at point.
RECTANGLE's first line is inserted at point, its second
line is inserted at a point vertically under point, etc.
RECTANGLE should be a list of strings."
  (let ((lines rectangle)
        (insertcolumn (current-column)))
    (insert-for-yank (car lines))
    (setq lines (cdr lines))
    (while lines
      (forward-line 1)
      (or (bolp) (insert ?\n))
      (move-to-column insertcolumn t)
      (insert-for-yank (car lines))
      (setq lines (cdr lines)))))

(defun mt-kill-thing-at-point (thing)
  (let* ((region (mt-bounds-of-thing-at-point thing))
         (beg (car region))
         (end (cdr region)))
    
    (prog1
        (list (buffer-substring-no-properties beg end))
      (delete-region beg end))))

(defun mt-kill-from-thing-at-point ()
  (if (use-region-p)
      (let* ((region (sort (list (mark) (point)) '<))
             (beg (car region))
             (end (car (cdr region))))
        (if rectangle-mark-mode
            (prog1
                (extract-rectangle beg end)
              (delete-rectangle beg end))
          (prog1
              (list (buffer-substring-no-properties beg end))
            (delete-region beg end))))
    (mt-kill-thing-at-point mt-from-thing)))





(defun mt-up-thing (arg &optional thing)
  (setq arg (- (abs arg))
        thing (or thing mt-to-thing))
  (let ((pos (point)))
   (cl-case thing
     ('line
      (line-move arg)
      (while (let ((bounds (mt-bounds-of-thing-at-point 'line)))
               (not (and bounds (< 1 (- (cdr bounds) (car bounds))))))
        (goto-char pos)
        (cl-decf arg)
        (line-move arg)))
     ('word
      (line-move arg)
      (while (let ((line (line-number-at-pos)))
               (right-word 1)
               (left-word 1)
               (not (and (= line (line-number-at-pos))
                         (mt-bounds-of-thing-at-point 'word))))
        (goto-char pos)
        (cl-decf arg)
        (line-move arg)))
     ('symbol
      (line-move arg)
      (while (let ((line (line-number-at-pos)))
               (sp-forward-symbol 1)
               (sp-backward-symbol 1)
               (not (and (= line (line-number-at-pos))
                         (mt-bounds-of-thing-at-point 'symbol))))
        (goto-char pos)
        (cl-decf arg)
        (line-move arg)))
     ('sexp
      (line-move arg)
      (while (let ((line (line-number-at-pos)))
               (sp-forward-sexp 1)
               (sp-backward-sexp 1)
               (not (and (= line (line-number-at-pos))
                         (mt-bounds-of-thing-at-point 'sexp))))
        (goto-char pos)
        (cl-decf arg)
        (line-move arg)))
     (t (error "Undefined up for %s" thing))))
  (- arg))

(defun mt-down-thing (arg &optional thing)
  (setq arg (abs arg)
        thing (or thing mt-to-thing))
  (let ((pos (point)))
   (cl-case thing
     ('line
      (line-move arg)
      (while (let ((bounds (mt-bounds-of-thing-at-point 'line)))
               (not (and bounds (< 1 (- (cdr bounds) (car bounds))))))
        (goto-char pos)
        (cl-incf arg)
        (line-move arg)))
     ('word
      (line-move arg)
      (while (let ((line (line-number-at-pos)))
               (right-word 1)
               (left-word 1)
               (not (and (= line (line-number-at-pos))
                         (mt-bounds-of-thing-at-point 'word))))
        (goto-char pos)
        (cl-incf arg)
        (line-move arg)))
     ('symbol
      (line-move arg)
      (while (let ((line (line-number-at-pos)))
               (sp-forward-symbol 1)
               (sp-backward-symbol 1)
               (not (and (= line (line-number-at-pos))
                         (mt-bounds-of-thing-at-point 'symbol))))
        (goto-char pos)
        (cl-incf arg)
        (line-move arg)))
     ('sexp
      (line-move arg)
      (while (let ((line (line-number-at-pos)))
               (sp-forward-sexp 1)
               (sp-backward-sexp 1)
               (not (and (= line (line-number-at-pos))
                         (mt-bounds-of-thing-at-point 'sexp))))
        (goto-char pos)
        (cl-incf arg)
        (line-move arg)))
     (t (error "Undefined up for %s" thing))))
  arg)

(defun mt-forward-thing (arg)
  (cl-case mt-to-thing
    ('word
     (right-word arg))
    ('symbol
     (sp-forward-symbol arg))
    ('sexp
     (sp-forward-sexp arg))
    (t (error "Undefined forward for %s" mt-to-thing))))

(defun mt-backward-thing (arg)
  (cl-case mt-to-thing
    ('word
     (left-word arg))
    ('symbol
     (sp-backward-symbol arg))
    ('sexp
     (sp-backward-sexp arg))
    (t (error "Undefined backward for %s" mt-to-thing))))

(defun mt-correct-pos-up (pos arg from-lengths to-lengths)
  (+ pos
     (-
      (apply '+ (cl-subseq
                 from-lengths 0
                 (min (length from-lengths)
                      arg)))
      (apply '+ (cl-subseq
                 to-lengths 0
                 (min (length to-lengths)
                      arg))))))

(defun mt-correct-pos-down (pos arg to-lengths)
  (+ pos
      (apply '+ (cl-subseq
                 to-lengths 0
                 (min (length to-lengths)
                      arg)))))




(defun mt-move-thing-up (arg)
  (let ((from (mt-kill-from-thing-at-point)))
    (let ((from-lines (length from))
          (from-lengths (mapcar 'length from))
          (pos (point)))
      (set 'arg (mt-up-thing arg))
      (let ((to (mt-kill-thing-at-point mt-to-thing)))
        (let ((to-lines (length to))
              (to-lengths (mapcar 'length to))
              (pos-end (point)))
          (mt-insert-rectangle from)
          (goto-char (mt-correct-pos-up pos arg from-lengths to-lengths))
          (mt-insert-rectangle to)
          (goto-char pos-end))))))
(advice-add 'mt-move-thing-up :around #'rollback-on-error-advice)

(defun mt-move-thing-backward (arg)
  (let ((from (mt-kill-from-thing-at-point)))
    (let ((from-lines (length from))
          (from-lengths (mapcar 'length from))
          (pos (point))
          pos-end)
      (mt-backward-thing arg)
      (let ((to (mt-kill-thing-at-point mt-to-thing)))
        (let ((to-lines (length to))
              (to-lengths (mapcar 'length to))
              (pos-end (point)))
          (mt-insert-rectangle from)
          (goto-char (mt-correct-pos-up pos arg from-lengths to-lengths))
          (mt-insert-rectangle to)
          (goto-char pos-end))))))
(advice-add 'mt-move-thing-backward :around #'rollback-on-error-advice)

(defun mt-move-thing-down (arg)
  (let ((from (mt-kill-from-thing-at-point)))
    (let  ((from-lines (length from))
           (from-lengths (mapcar 'length from))
           (pos (point)))
      (if (not (eql mt-from-thing 'line))
          (set 'arg (mt-down-thing arg))
        (cl-decf arg)
        (set 'arg (mt-down-thing arg))
        (cl-incf arg))
      (let ((to (mt-kill-thing-at-point mt-to-thing)))
        (let ((to-lines (length to))
              (to-lengths (mapcar 'length to)))
          (let ((pos-end (mt-correct-pos-down (point) arg to-lengths)))
            (mt-insert-rectangle from)
            (goto-char pos)
            (mt-insert-rectangle to)
            (goto-char pos-end)))))))
(advice-add 'mt-move-thing-down :around #'rollback-on-error-advice)


(defun mt-move-thing-forward (arg)
  (let ((from (mt-kill-from-thing-at-point)))
    (let   ((from-lines (length from))
            (from-lengths (mapcar 'length from))
            (pos (point)))
      (mt-forward-thing arg)
      (mt-backward-thing 1)
      (let ((to (mt-kill-thing-at-point mt-to-thing)))
        (let ((to-lines (length to))
              (to-lengths (mapcar 'length to)))
          (let ((pos-end (mt-correct-pos-down (point) arg to-lengths)))
            (mt-insert-rectangle from)
            (goto-char pos)
            (mt-insert-rectangle to)
            (goto-char pos-end)))))))
(advice-add 'mt-move-thing-forward :around #'rollback-on-error-advice)



(defun mt-move-down (arg)
  (interactive "p")
  (mt-move-thing-down (or arg 1)))

(defun mt-move-up (arg)
  (interactive "p")
  (mt-move-thing-up (or arg 1)))

(defun mt-move-right (arg)
  (interactive "p")
  (mt-move-thing-forward (or arg 1)))

(defun mt-move-left (arg)
  (interactive "p")
  (mt-move-thing-backward (or arg 1)))

(defun mt-up (arg)
  (interactive "p")
  (mt-up-thing (or arg 1)))

(defun mt-down (arg)
  (interactive "p")
  (mt-down-thing (or arg 1)))

(defun mt-backward (arg)
  (interactive "p")
  (mt-backward-thing (or arg 1)))

(defun mt-forward (arg)
  (interactive "p")
  (mt-forward-thing (or arg 1)))


(provide 'move-thing)
;;; move-thing.el ends here
