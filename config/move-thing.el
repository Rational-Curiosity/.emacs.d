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


(defvar mt-things '(word symbol sexp line))

(defun mt-bounds-of-thing-at-point (thing)
  (let ((bounds (mt-bounds-of-thing-at-point-helper thing)))
    (if bounds
        bounds
      (error "Not %s found" thing))))


;; [ from
(defvar mt-from-thing 'line)
(make-variable-buffer-local mt-from-thing)

(defvar mt-from-thing-ring nil)
(let ((from-things mt-things))
  (set 'mt-from-thing-ring (make-ring (length from-things)))
  (dolist (elem from-things) (ring-insert mt-from-thing-ring elem)))

(defun mt-cycle-from-things ()
  "Cycle from-things in ring."
  (let ((from-thing (ring-ref mt-from-thing-ring -1)))
    (ring-insert mt-from-thing-ring from-thing)
    (setq mt-from-thing from-thing
          mt-to-thing from-thing)
    (let ((bounds (mt-bounds-of-thing-at-point from-thing)))
      (pulse-momentary-highlight-region (car bounds) (cdr bounds)))
    from-thing))
;; ]

(defun mt-cycle-things ()
  "Cycle things in ring."
  (interactive)
  (condition-case nil
      (message "%s thing setted" (mt-cycle-from-things))
    (error (message "%s thing setted, second try" (mt-cycle-from-things)))))

;; [ to
(defvar mt-to-thing 'line)
(make-variable-buffer-local mt-to-thing)

(defvar mt-to-thing-ring nil)
(let ((to-things mt-things))
  (set 'mt-to-thing-ring (make-ring (length to-things)))
  (dolist (elem to-things) (ring-insert mt-to-thing-ring elem)))

(defun mt-cycle-to-things ()
  "Cycle to-things in ring."
  (interactive)
  (let ((to-thing (ring-ref mt-to-thing-ring -1)))
    (ring-insert mt-to-thing-ring to-thing)
    (set 'mt-to-thing to-thing)
    (message "%s to-thing setted" to-thing)))
;; ]



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





(defun mt-up-thing (arg)
  (cl-case mt-to-thing
    ('line
     (line-move arg))
    ('word
     (line-move arg))
    ('symbol
     (line-move arg))
    ('sexp
     (line-move arg))
    (t (error "Undefined forward for %s" mt-to-thing))))

(defun mt-down-thing (arg)
  (when (eql mt-from-thing 'line)
    (cl-decf arg))
  (cl-case mt-to-thing
    ('line
     (line-move arg))
    ('word
     (line-move arg))
    ('symbol
     (line-move arg))
    ('sexp
     (line-move arg))
    (t (error "Undefined forward for %s" mt-to-thing))))

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



(defun mt-check-thing-at-point (thing)
  (let ((line (line-number-at-pos)))
    (cl-case mt-to-thing
      ('word
       (right-word 1)
       (left-word 1))
      ('symbol
       (sp-forward-symbol 1)
       (sp-backward-symbol 1))
      ('sexp
       (sp-forward-sexp 1)
       (sp-backward-sexp 1)))
    (and (= line (line-number-at-pos))
         (mt-bounds-of-thing-at-point thing))))



(defun mt-move-thing-up (arg)
  (let ((from (mt-kill-from-thing-at-point)))
    (let ((from-lines (length from))
          (from-lengths (mapcar 'length from))
          (pos (point)))
      (mt-up-thing (- arg))
      (while (not (mt-check-thing-at-point mt-to-thing))
        (cl-incf arg)
        (goto-char pos)
        (mt-up-thing (- arg)))
      (let ((to (mt-kill-thing-at-point mt-to-thing)))
        (let ((to-lines (length to))
              (to-lengths (mapcar 'length to))
              (pos-end (point)))
          (insert-rectangle from)
          (goto-char (mt-correct-pos-up pos arg from-lengths to-lengths))
          (insert-rectangle to)
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
          (insert-rectangle from)
          (goto-char (mt-correct-pos-up pos arg from-lengths to-lengths))
          (insert-rectangle to)
          (goto-char pos-end))))))
(advice-add 'mt-move-thing-backward :around #'rollback-on-error-advice)

(defun mt-move-thing-down (arg)
  (let ((from (mt-kill-from-thing-at-point)))
    (let  ((from-lines (length from))
           (from-lengths (mapcar 'length from))
           (pos (point)))
      (mt-down-thing arg)
      (while (not (mt-check-thing-at-point mt-to-thing))
        (cl-incf arg)
        (goto-char pos)
        (mt-down-thing arg))
      (let ((to (mt-kill-thing-at-point mt-to-thing)))
        (let ((to-lines (length to))
              (to-lengths (mapcar 'length to)))
          (let ((pos-end (mt-correct-pos-down (point) arg to-lengths)))
            (insert-rectangle from)
            (goto-char pos)
            (insert-rectangle to)
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
            (insert-rectangle from)
            (goto-char pos)
            (insert-rectangle to)
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



(bind-keys
 ("M-C-<up>"    . mt-move-up)
 ("M-C-<down>"  . mt-move-down)
 ("M-C-<left>"  . mt-move-left)
 ("M-C-<right>" . mt-move-right)
 ("M-C-f"       . mt-cycle-things))


(provide 'move-thing)
;;; move-thing.el ends here
