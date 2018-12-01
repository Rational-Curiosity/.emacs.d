;;; move-thing.el --- Move words and sexps with cursors

;;; Commentary:

;; Usage:
;; (require 'move-thing)

;; THING should be a symbol specifying a type of syntactic entity.
;; Possibilities include ‘symbol’, ‘list’, ‘sexp’, ‘defun’,
;; ‘filename’, ‘url’, ‘email’, ‘word’, ‘sentence’, ‘whitespace’,
;; ‘line’, and ‘page’.

;;; Code:
;; Check list sorted
(defun sorted-p (list op)
  (let ((copy (cl-copy-list list)))
    (equal (sort copy op) list)))

;; Protect from errors
(defun rollback-on-error-inc ()
  (cl-incf rollback-on-error-counter))
(defun rollback-on-error-advice (orig-fun &rest args)
  "Rollback (ORIG-FUN ARGS) evaluation on error."
  ;; (undo-boundary)  ; <undo>
  (advice-add 'undo-boundary :before #'rollback-on-error-inc)
  (unwind-protect
      (let ((rollback-on-error-counter 1))
        (condition-case raised-error
            (apply orig-fun args)
          (error (primitive-undo rollback-on-error-counter
                                 buffer-undo-list)
                 (error "%s rolled back (%i)"
                        raised-error
                        rollback-on-error-counter))))
    (advice-remove 'undo-boundary #'rollback-on-error-inc)))
(require 'rect)
(require 'ring)
;; (fset 'mt-bounds-of-thing-at-point
;;       (if (require 'thingatpt+ nil t)
;;           #'tap-bounds-of-thing-at-point
;;         #'bounds-of-thing-at-point))
(fset 'mt-bounds-of-thing-at-point
      #'bounds-of-thing-at-point)

(defun mt-bounds-of-thing-at-point-or-region (thing)
  (if (use-region-p)
      (let ((positions (sort (list (mark) (point)) '<)))
        (if rectangle-mark-mode
            (let ((columns (sort (list (progn (goto-char (car positions))
                                              (current-column))
                                       (progn (goto-char (car (cdr positions)))
                                              (current-column))) '<)))
              (cons 'rectangle
                    (cons (progn (goto-char (car positions))
                                 (move-to-column (car columns))
                                 (point))
                          (progn (goto-char (car (cdr positions)))
                                 (move-to-column (car (cdr columns)))
                                 (point)))))
          (cons 'region
                (cons (car positions) (car (cdr positions))))))
    (cons 'bounds
          (mt-bounds-of-thing-at-point thing))))


;; #     #
;; ##   ##  ####  #####  ######
;; # # # # #    # #    # #
;; #  #  # #    # #    # #####
;; #     # #    # #    # #
;; #     # #    # #    # #
;; #     #  ####  #####  ######
(defgroup move-thing ()
  "Move thing minor mode."
  :group 'editing
  :prefix "mt-")

(defcustom move-thing-mode-line
  '(:eval (concat (cdr (assoc mt-from-thing mt-things))
                  (if mt-interchange-things
                      (cdr (assoc mt-to-thing mt-things)))))
  "Show current selected thing."
  :group 'move-thing
  :risky t
  :type 'sexp)

(defvar move-thing-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-C-<up>")     'mt-move-up)
    (define-key map (kbd "M-C-<down>")   'mt-move-down)
    (define-key map (kbd "M-C-<left>")   'mt-move-left)
    (define-key map (kbd "M-C-<right>")  'mt-move-right)
    (define-key map (kbd "M-S-<up>")     'mt-up)
    (define-key map (kbd "M-S-<down>")   'mt-down)
    (define-key map (kbd "M-S-<left>")   'mt-backward)
    (define-key map (kbd "M-S-<right>")  'mt-forward)
    (define-key map (kbd "M-C-<home>")   'mt-cycle-things)
    (define-key map (kbd "M-C-<end>")    'mt-cycle-to-things)
    (define-key map (kbd "M-C-<prior>")  'mt-shift-mc-left)
    (define-key map (kbd "M-C-<next>")   'mt-shift-mc-right)
    (define-key map (kbd "M-C-<insert>") 'mt-toggle-interchange-things)
    map))

(define-minor-mode move-thing-mode
  "Toggle Move thing mode."
  :init-value nil
  :lighter move-thing-mode-line
  :group 'move-thing
  :keymap move-thing-mode-map
  :global t)

;; #######
;;    #    #    # # #    #  ####   ####
;;    #    #    # # ##   # #    # #
;;    #    ###### # # #  # #       ####
;;    #    #    # # #  # # #  ###      #
;;    #    #    # # #   ## #    # #    #
;;    #    #    # # #    #  ####   ####
(defvar mt-things '((word     . "'w")
                    (symbol   . "'s")
                    (sexp     . "'e")
                    (list     . "'t")
                    (defun    . "'d")
                    (filename . "'f")
                    (url      . "'u")
                    (email    . "'m")
                    (line     . "'l")))

(defvar mt-interchange-things nil)
(make-variable-buffer-local 'mt-interchange-things)

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


;;              #
;; #####       #  #    #
;; #    #     #   #    #
;; #    #    #    #    #
;; #####    #     # ## #
;; #   #   #      ##  ##
;; #    # #       #    #
(defun mt-insert-rectangle (rectangle arg &optional col)
  (let ((lines (if (<= 0 arg) rectangle (nreverse rectangle)))
        (column (or col (current-column))))
    ;; (undo-boundary)  ; <undo>
    (insert (car lines))
    (setq lines (cdr lines))
    (while lines
      (forward-line arg)
      (or (bolp) (insert ?\n))
      (move-to-column column t)
      (insert (car lines))
      (setq lines (cdr lines)))))

(defun mt-kill-rectangle-or-bounds (arg)
  (unless (cdr arg)
    (error "%s not found, kill imposible" arg))
  (cl-case (car arg)
    ('rectangle
     ;; (undo-boundary)  ; <undo>
     (delete-extract-rectangle (car (cdr arg)) (cdr (cdr arg))))
    ((region bounds)
     (prog1
         (list (buffer-substring-no-properties (car (cdr arg)) (cdr (cdr arg))))
       ;; (undo-boundary)  ; <undo>
       (delete-region (car (cdr arg)) (cdr (cdr arg)))))))

(defun mt-kill-bounds (arg)
  (unless arg
    (error "Thing not found, kill imposible"))
  (prog1
      (buffer-substring-no-properties (car arg) (cdr arg))
    ;; (undo-boundary)  ; <undo>
    (delete-region (car arg) (cdr arg))))

;; #     #
;; ##    #   ##   #    # #  ####    ##   ##### #  ####  #    #
;; # #   #  #  #  #    # # #    #  #  #    #   # #    # ##   #
;; #  #  # #    # #    # # #      #    #   #   # #    # # #  #
;; #   # # ###### #    # # #  ### ######   #   # #    # #  # #
;; #    ## #    #  #  #  # #    # #    #   #   # #    # #   ##
;; #     # #    #   ##   #  ####  #    #   #   #  ####  #    #
(defun mt-forward-line (arg &optional column)
  (unless (and (not column) (= 0 arg))
    (setq column (or column (current-column)))
    (unless (= 0 (forward-line arg))
      (error "Buffer limit reached"))
    (= column (move-to-column column))))

(defun mt-exists-thing-at-point (thing)
  (let ((bounds (mt-bounds-of-thing-at-point thing)))
    (and bounds
         (let ((str (buffer-substring-no-properties
                     (car bounds) (cdr bounds))))
           (not (string-equal str "\n"))))))

(defun mt-up-thing (arg &optional column thing)
  (setq arg (- (abs arg))
        thing (or thing mt-to-thing))
  (mt-forward-line arg column)
  (while (not (mt-exists-thing-at-point thing))
    (cl-decf arg)
    (mt-forward-line -1 column))
  (- arg))

(defun mt-down-thing (arg &optional column thing)
  (setq arg (abs arg)
        thing (or thing mt-to-thing))
  (mt-forward-line arg column)
  (while (not (mt-exists-thing-at-point thing))
    (cl-incf arg)
    (mt-forward-line 1 column))
  arg)

(defun mt-forward-thing (arg &optional thing delimiter len)
  (setq thing (or thing mt-to-thing)
        len (or len 1))
  (let (bounds
        pos
        (pos-ini (point)))
    (if delimiter
        (dotimes (i arg)
          (while (not (and
                       (set 'bounds (mt-bounds-of-thing-at-point thing))
                       (set 'pos (point))
                       (not (= pos (cdr bounds)))
                       (< pos-ini pos)
                       (string-match-p delimiter
                                       (buffer-substring-no-properties
                                        (car bounds)
                                        (+ len (car bounds))))))
            (forward-char 1))
          (goto-char (cdr bounds)))
      (dotimes (i arg)
        (while (not (and
                     (set 'bounds (mt-bounds-of-thing-at-point thing))
                     (set 'pos (point))
                     (not (= pos (cdr bounds)))
                     (< pos-ini pos)))
          (forward-char 1))
        (goto-char (cdr bounds))))
    bounds))

(defun mt-backward-thing (arg &optional thing delimiter len)
  (setq thing (or thing mt-to-thing)
        len (or len 1))
  (let (bounds
        pos
        (pos-ini (point)))
    (if delimiter
        (dotimes (i arg)
          (while (not (and
                       (set 'bounds (mt-bounds-of-thing-at-point thing))
                       (set 'pos (point))
                       (not (= pos (car bounds)))
                       (> pos-ini pos)
                       (string-match-p delimiter
                                       (buffer-substring-no-properties
                                        (car bounds)
                                        (+ len (car bounds))))))
            (backward-char 1))
          (goto-char (car bounds)))
      (dotimes (i arg)
        (while (not (and
                     (set 'bounds (mt-bounds-of-thing-at-point thing))
                     (not (= (point) (car bounds)))))
          (backward-char 1))
        (goto-char (car bounds))))
    bounds))


;; #     #
;; ##   ##  ####  #    # ###### #    # ###### #    # #####
;; # # # # #    # #    # #      ##  ## #      ##   #   #
;; #  #  # #    # #    # #####  # ## # #####  # #  #   #
;; #     # #    # #    # #      #    # #      #  # #   #
;; #     # #    #  #  #  #      #    # #      #   ##   #
;; #     #  ####    ##   ###### #    # ###### #    #   #
(defun mt-newline-ending (str)
  (char-equal ?\n (aref str (1- (length str)))))

(defun mt-push-mark (type)
  (cl-case type
    ('rectangle
     (rectangle-mark-mode)
     (push-mark)
     (setq deactivate-mark nil))
    ('region
     (push-mark)
     (setq deactivate-mark nil))))

(defun mt-push-mark-all (type)
  (cl-case type
    ('rectangle
     (rectangle-mark-mode)
     (push-mark)
     (setq deactivate-mark nil))
    ('region
     (push-mark)
     (setq deactivate-mark nil))
    ('bounds
     (set-mark (point))
     (setq deactivate-mark nil))))

(defun mt-move-thing-up (arg)
  (mt-move-thing-down (- arg)))

(defun mt-move-thing-down (arg)
  (let* ((from-sbs (mt-bounds-of-thing-at-point-or-region mt-from-thing))
         (from (mt-kill-rectangle-or-bounds from-sbs))
         (column (current-column)))
    (goto-char (car (cdr from-sbs)))
    (forward-line arg)
    (move-to-column column t)
    (let ((pos (point)))
      (mt-insert-rectangle from 1 column)
      (mt-push-mark-all (car from-sbs))
      (goto-char pos))))

(defun mt-move-thing-backward (arg)
  (mt-move-thing-forward (- arg)))

(defun mt-move-thing-forward (arg)
  (let* ((from-sbs (mt-bounds-of-thing-at-point-or-region mt-from-thing))
         (from (mt-kill-rectangle-or-bounds from-sbs)))
    (goto-char (car (cdr from-sbs)))
    (forward-char arg)
    (let ((pos (point)))
      (mt-insert-rectangle from 1)
      (mt-push-mark-all (car from-sbs))
      (goto-char pos))))

(defun mt-interchange-thing-up (arg)
  (let* ((column (current-column))
         (from-sbs (mt-bounds-of-thing-at-point-or-region mt-from-thing))
         (from (mt-kill-rectangle-or-bounds from-sbs)))
    (goto-char (car (cdr from-sbs)))
    (mt-up-thing arg column mt-to-thing)
    (let* ((to-bs (mt-bounds-of-thing-at-point mt-to-thing))
           (to (mt-kill-bounds to-bs)))
      (goto-char (- (car (cdr from-sbs)) (length to)))
      ;; (undo-boundary)  ; <undo>
      (insert to)
      (goto-char (car to-bs))
      (mt-insert-rectangle from 1 column)
      (mt-push-mark (car from-sbs))
      (goto-char (car to-bs)))))
(advice-add 'mt-interchange-thing-up :around #'rollback-on-error-advice)

(defun mt-interchange-thing-down (arg)
  (let* ((column (current-column))
         (from-sbs (mt-bounds-of-thing-at-point-or-region mt-from-thing))
         (from (mt-kill-rectangle-or-bounds from-sbs)))
    (goto-char (car (cdr from-sbs)))
    (mt-forward-line (1- (length from)))
    (when (mt-newline-ending (car from))
      (cl-decf arg))
    (mt-down-thing arg column mt-to-thing)
    (let* ((to-bs (mt-bounds-of-thing-at-point mt-to-thing))
           (to (mt-kill-bounds to-bs)))
      (goto-char (car (cdr from-sbs)))
      ;; (undo-boundary)  ; <undo>
      (insert to)
      (let ((pos (+ (length to) (car to-bs))))
        (goto-char pos)
        (mt-insert-rectangle from 1 column)
        (mt-push-mark (car from-sbs))
        (goto-char pos)))))
(advice-add 'mt-interchange-thing-down :around #'rollback-on-error-advice)

(defun mt-interchange-thing-backward (arg)
  (let ((from-sbs (mt-bounds-of-thing-at-point-or-region mt-from-thing)))
    (goto-char (car (cdr from-sbs)))
    (mt-backward-thing arg mt-to-thing
                       (and (eq mt-to-thing 'sexp)
                            (let* ((pos (car (cdr from-sbs)))
                                   (delimiter (buffer-substring-no-properties
                                               pos (1+ pos))))
                              (if (member
                                   delimiter
                                   '("\"" "'" "(" "{" "["))
                                  delimiter
                                "[^\"'({[]"))))
    (let* ((to-bs (mt-bounds-of-thing-at-point mt-to-thing))
           (from (mt-kill-rectangle-or-bounds from-sbs))
           (to (mt-kill-bounds to-bs)))
      (goto-char (- (car (cdr from-sbs)) (length to)))
      ;; (undo-boundary)  ; <undo>
      (insert to)
      (goto-char (car to-bs))
      (mt-insert-rectangle from 1)
      (mt-push-mark (car from-sbs))
      (goto-char (car to-bs)))))
(advice-add 'mt-interchange-thing-backward :around #'rollback-on-error-advice)

(defun mt-interchange-thing-forward (arg)
  (let* ((from-sbs (mt-bounds-of-thing-at-point-or-region mt-from-thing))
         (from (mt-kill-rectangle-or-bounds from-sbs)))
    (goto-char (car (cdr from-sbs)))
    (goto-char (car (mt-forward-thing arg mt-to-thing
                                      (and (eq mt-to-thing 'sexp)
                                           (let ((delimiter (substring (car from) 0 1)))
                                             (if (member
                                                  delimiter
                                                  '("\"" "'" "(" "{" "["))
                                                 delimiter
                                               "[^\"'({[]"))))))
    (let* ((to-bs (mt-bounds-of-thing-at-point mt-to-thing))
           (to (mt-kill-bounds to-bs)))
      (goto-char (car (cdr from-sbs)))
      ;; (undo-boundary)  ; <undo>
      (insert to)
      (let ((pos (+ (length to) (car to-bs))))
        (goto-char pos)
        (mt-insert-rectangle from 1)
        (mt-push-mark (car from-sbs))
        (goto-char pos)))))
(advice-add 'mt-interchange-thing-forward :around #'rollback-on-error-advice)

(defun mt-shift-points-left (bounds)
  (let* ((strings (mapcar
                   (lambda (b)
                     (buffer-substring-no-properties (car b) (cdr b)))
                   bounds))
         (item (pop strings))
         (last-correction 0)
         (lengths (mapcar (lambda (b) (- (cdr b) (car b))) bounds))
         (paste-lengths (cons 0 (cdr lengths)))
         (cut-lengths (cons 0 lengths))
         (positions (cl-mapcar
                     (lambda (b c p)
                       (setq last-correction (+ last-correction
                                                (- p c)))
                       (+ (car b) last-correction))
                     bounds
                     cut-lengths
                     paste-lengths))
         (new-bounds (cl-mapcar
                      (lambda (p l)
                        (cons p (+ p l)))
                      positions
                      (nconc (cdr lengths) (list (car lengths))))))
    (setq strings (nreverse strings)
          bounds (nreverse bounds))
    (push item strings)
    (while strings
      (let ((bound (pop bounds)))
        (delete-region (car bound) (cdr bound))
        (goto-char (car bound))
        (insert (pop strings))))
    new-bounds))

(defun mt-shift-points-right (bounds)
  (let* ((strings (mapcar
                   (lambda (b)
                     (buffer-substring-no-properties (car b) (cdr b)))
                   bounds))
         (last-correnction 0)
         (lengths (mapcar (lambda (b) (- (cdr b) (car b))) bounds))
         (cut-lengths (cons 0 lengths))
         (final-lengths (cons (car (last lengths)) lengths))
         (paste-lengths (cons 0 final-lengths))
         (positions (cl-mapcar
                     (lambda (b c p)
                       (setq last-correnction (+ last-correnction
                                                 (- p c)))
                       (+ (car b) last-correnction))
                     bounds
                     cut-lengths
                     paste-lengths))
         (new-bounds (cl-mapcar
                      (lambda (p l)
                        (cons p (+ p l)))
                      positions
                      final-lengths)))
    (push (elt strings (1- (length strings))) strings)
    (setq strings (nreverse strings)
          bounds (nreverse bounds))
    (pop strings)
    (while strings
      (let ((bound (pop bounds)))
        (delete-region (car bound) (cdr bound))
        (goto-char (car bound))
        (insert (pop strings))))
    new-bounds))

(defun mt-shift-points (points arg)
  (let ((bounds
         (mapcar
          (lambda (pos)
            (goto-char pos)
            (mt-bounds-of-thing-at-point mt-from-thing))
          (sort points '<)))
        (neg (> 0 arg))
        listed-bounds)
    (mapc (lambda (x)
            (push (car x) listed-bounds)
            (push (cdr x) listed-bounds)) bounds)
    (if (not (sorted-p listed-bounds '>))
        (error "move-thing: %s's bounds overlap" mt-from-thing)
      (if neg
          (dotimes (i (- arg) bounds)
            (setq bounds (mt-shift-points-left bounds)))
        (dotimes (i arg bounds)
          (setq bounds (mt-shift-points-right bounds)))))))
(advice-add 'mt-shift-points :around #'rollback-on-error-advice)


;; ###
;;  #  #    # ##### ###### #####    ##    ####  ##### # #    # ######
;;  #  ##   #   #   #      #    #  #  #  #    #   #   # #    # #
;;  #  # #  #   #   #####  #    # #    # #        #   # #    # #####
;;  #  #  # #   #   #      #####  ###### #        #   # #    # #
;;  #  #   ##   #   #      #   #  #    # #    #   #   #  #  #  #
;; ### #    #   #   ###### #    # #    #  ####    #   #   ##   ######
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

(defun mt-shift-mc-left (arg)
  (interactive "p")
  (mt-shift-mc-right (- arg)))

(defun mt-shift-mc-right (arg)
  (interactive "p")
  (let* ((bounds (mt-shift-points
                 (cons (point)
                       (mapcar
                        (lambda (x)
                          (overlay-get x 'point))
                        (mc/all-fake-cursors)))
                 arg))
         (bound (pop bounds)))
    (mc/remove-fake-cursors)
    (dolist (b bounds)
      (goto-char (car b))
      (mc/create-fake-cursor-at-point))
    (goto-char (car bound))))

(defun mt-move-down (arg)
  (interactive "p")
  (if mt-interchange-things
      (mt-interchange-thing-down arg)
    (mt-move-thing-down arg)))

(defun mt-move-up (arg)
  (interactive "p")
  (if mt-interchange-things
      (mt-interchange-thing-up arg)
    (mt-move-thing-up arg)))

(defun mt-move-right (arg)
  (interactive "p")
  (if mt-interchange-things
      (mt-interchange-thing-forward arg)
    (mt-move-thing-forward arg)))

(defun mt-move-left (arg)
  (interactive "p")
  (if mt-interchange-things
      (mt-interchange-thing-backward arg)
    (mt-move-thing-backward arg)))

(defun mt-up (arg)
  (interactive "p")
  (mt-up-thing arg))

(defun mt-down (arg)
  (interactive "p")
  (mt-down-thing arg))

(defun mt-backward (arg)
  (interactive "p")
  (mt-backward-thing arg))

(defun mt-forward (arg)
  (interactive "p")
  (mt-forward-thing arg))

(defun mt-toggle-interchange-things ()
  (interactive)
  (setq mt-interchange-things (not mt-interchange-things)))

(provide 'move-thing)
;;; move-thing.el ends here
