;;; move-thing.el --- Move words and sexps with cursors

;;; Commentary:

;; Usage:
;; (require 'move-thing)

;; THING should be a symbol specifying a type of syntactic entity.
;; Possibilities include ‘symbol’, ‘list’, ‘sexp’, ‘defun’,
;; ‘filename’, ‘url’, ‘email’, ‘word’, ‘sentence’, ‘whitespace’,
;; ‘line’, and ‘page’.

;;; Code:

;;  #####
;; #     #  ####  #####  ######
;; #       #    # #    # #
;; #       #    # #    # #####
;; #       #    # #####  #
;; #     # #    # #   #  #
;;  #####   ####  #    # ######
;; (fset 'mt-bounds-of-thing-at-point
;;       (if (require 'thingatpt+ nil t)
;;           #'tap-bounds-of-thing-at-point
;;         #'bounds-of-thing-at-point))
(fset 'mt--bounds-of-thing-at-point
      #'bounds-of-thing-at-point)

(require 'rect)
(require 'ring)
(defvar mt-things
  '((word     . "'w")
    (symbol   . "'s")
    (sexp     . "'e")
    (list     . "'t")
    (defun    . "'d")
    (filename . "'f")
    (url      . "'u")
    (email    . "'m")
    (line     . "'l")))

;; Check list sorted
(defun sorted-p (list op)
  (let ((copy (cl-copy-list list)))
    (equal (sort copy op) list)))

;; [ to
(defvar mt--to-thing-ring nil)
(let ((to-things mt-things))
  (set 'mt--to-thing-ring (make-ring (length to-things)))
  (dolist (elem to-things) (ring-insert mt--to-thing-ring (car elem))))

(defvar mt--to-thing (ring-ref mt--to-thing-ring 0))

(defun mt--cycle-to-things ()
  "Cycle to-things in ring."
  (let ((to-thing (ring-ref mt--to-thing-ring -1)))
    (ring-insert mt--to-thing-ring to-thing)
    (set 'mt--to-thing to-thing)))
;; ]

;; [ from
(defvar mt--from-thing-ring nil)
(let ((from-things mt-things))
  (set 'mt--from-thing-ring (make-ring (length from-things)))
  (dolist (elem from-things) (ring-insert mt--from-thing-ring (car elem))))

(defvar mt--from-thing (ring-ref mt--from-thing-ring 0))

(defun mt--cycle-from-things ()
  "Cycle from-things in ring."
  (let ((from-thing (ring-ref mt--from-thing-ring -1)))
    (ring-insert mt--from-thing-ring from-thing)
    (setq mt--from-thing from-thing
          mt--to-thing from-thing)
    from-thing))
;; ]

(defun mt--bounds-of-thing-at-point-or-region (thing)
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
          (mt--bounds-of-thing-at-point thing))))

;;  #####                                               #     #
;; #     #  ####  #    # #    #   ##   #    # #####     #     #  ####   ####  #    #
;; #       #    # ##  ## ##  ##  #  #  ##   # #    #    #     # #    # #    # #   #
;; #       #    # # ## # # ## # #    # # #  # #    #    ####### #    # #    # ####
;; #       #    # #    # #    # ###### #  # # #    #    #     # #    # #    # #  #
;; #     # #    # #    # #    # #    # #   ## #    #    #     # #    # #    # #   #
;;  #####   ####  #    # #    # #    # #    # #####     #     #  ####   ####  #    #
(defvar mt-movement-commands
  '(
    ;; previous-line
    ;; next-line
    ;; right-char
    ;; right-word
    ;; forward-char
    ;; forward-word
    ;; left-char
    ;; left-word
    ;; backward-char
    ;; backward-word
    ;; forward-paragraph
    ;; backward-paragraph
    ;; forward-list
    ;; backward-list
    ;; end-of-buffer
    ;; end-of-defun
    ;; end-of-line
    ;; end-of-sexp
    ;; exchange-point-and-mark
    ;; move-end-of-line
    ;; beginning-of-buffer
    ;; beginning-of-defun
    ;; beginning-of-line
    ;; beginning-of-sexp
    ;; move-beginning-of-line
    ;; back-to-indentation
    ;; subword-forward
    ;; subword-backward
    ;; subword-mark
    ;; subword-kill
    ;; subword-backward-kill
    ;; subword-transpose
    ;; subword-capitalize
    ;; subword-upcase
    ;; subword-downcase
    ;; smart-forward
    ;; smart-backward
    ;; smart-up
    ;; smart-down
    avy-goto-char
    avy-goto-char-2
    avy-goto-char-2-above
    avy-goto-char-2-below
    avy-goto-char-in-line
    avy-goto-char-timer
    avy-goto-end-of-line
    avy-goto-line
    avy-goto-line-above
    avy-goto-line-below
    avy-goto-subword-0
    avy-goto-subword-1
    avy-goto-symbol-1
    avy-goto-symbol-1-above
    avy-goto-symbol-1-below
    avy-goto-word-0
    avy-goto-word-0-above
    avy-goto-word-0-below
    avy-goto-word-0-regexp
    avy-goto-word-1
    avy-goto-word-1-above
    avy-goto-word-1-below
    avy-goto-word-or-subword-1)
  "Default set of movement commands.")

(defvar mt-interchange-things nil)

(defvar mt--marker nil
  "Beginning of from region marker.")

(defun mt--pre-command ()
  (and (memq this-original-command mt-movement-commands)
       (mt--bounds-of-thing-at-point mt--from-thing)
       (set-marker mt--marker (point))))

(defun mt--post-command ()
  (when (and (memq this-original-command mt-movement-commands)
             mt--marker
             (marker-position mt--marker))
    (condition-case raised-error
        (let ((to-bounds (mt--bounds-of-thing-at-point
                          mt--to-thing)))
          (when (and to-bounds
                     (save-excursion
                       (switch-to-buffer (marker-buffer mt--marker))
                       (goto-char (marker-position mt--marker))
                       (mt--bounds-of-thing-at-point mt--from-thing)))
            (let ((thing (if mt-interchange-things
                             (mt--kill-bounds to-bounds))))
              (save-excursion
                (switch-to-buffer (marker-buffer mt--marker))
                (goto-char (marker-position mt--marker))
                (setq thing (prog1
                                (mt--kill-bounds
                                 (mt--bounds-of-thing-at-point mt--from-thing))
                              (and thing
                                   (not buffer-read-only)
                                   (insert thing)))))
              (insert thing))))
      (error (message "Moving thing: %s" (error-message-string raised-error))))
    (set-marker mt--marker nil)))

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

(defcustom mt-mode-line
  '(:eval (concat (cdr (assoc mt--from-thing mt-things))
                  (if mt-interchange-things
                      (cdr (assoc mt--to-thing mt-things)))))
  "Show current selected thing."
  :group 'move-thing
  :risky t
  :type 'sexp)

(defvar mt-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-j") 'mt-cycle-things)
    (define-key map (kbd "M-k") 'mt-cycle-to-things)
    (define-key map (kbd "M-h") 'mt-toggle-interchange-things)
    (define-key map (kbd "C-p") 'mt-move-up)
    (define-key map (kbd "C-n") 'mt-move-down)
    (define-key map (kbd "C-b") 'mt-move-left)
    (define-key map (kbd "C-f") 'mt-move-right)
    (define-key map (kbd "<up>")     'mt-up)
    (define-key map (kbd "<down>")   'mt-down)
    (define-key map (kbd "<left>")   'mt-backward)
    (define-key map (kbd "<right>")  'mt-forward)
    (define-key map (kbd "<prior>")  'mt-shift-mc-left)
    (define-key map (kbd "<next>")   'mt-shift-mc-right)
    map))

(define-minor-mode mt-mode
  "Toggle Move thing mode."
  :init-value nil
  :lighter mt-mode-line
  :group 'move-thing
  :keymap mt-mode-map
  :global t
  (if mt-mode
      (progn
        (add-hook 'post-command-hook 'mt--post-command)
        (add-hook 'pre-command-hook 'mt--pre-command)
        (setq mt--marker (make-marker)))
    (setq mt--marker nil)
    (remove-hook 'post-command-hook 'mt--post-command)
    (remove-hook 'pre-command-hook 'mt--pre-command)))

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

(defun mt--kill-bounds (bounds)
  (let ((beg (car bounds))
        (end (cdr bounds)))
    (prog1
        (buffer-substring-no-properties beg end)
      ;; (undo-boundary)  ; <undo>
      (unless buffer-read-only
        (delete-region beg end)))))

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
  (let ((bounds (mt--bounds-of-thing-at-point thing)))
    (and bounds
         (let ((str (buffer-substring-no-properties
                     (car bounds) (cdr bounds))))
           (not (string-equal str "\n"))))))

(defun mt-up-thing (arg &optional column thing)
  (setq arg (- (abs arg))
        thing (or thing mt--to-thing))
  (mt-forward-line arg column)
  (while (not (mt-exists-thing-at-point thing))
    (cl-decf arg)
    (mt-forward-line -1 column))
  (- arg))

(defun mt-down-thing (arg &optional column thing)
  (setq arg (abs arg)
        thing (or thing mt--to-thing))
  (mt-forward-line arg column)
  (while (not (mt-exists-thing-at-point thing))
    (cl-incf arg)
    (mt-forward-line 1 column))
  arg)

(defun mt-forward-thing (arg &optional thing delimiter len)
  (setq thing (or thing mt--to-thing)
        len (or len 1))
  (let (bounds
        pos
        (pos-ini (point)))
    (if delimiter
        (dotimes (i arg)
          (while (not (and
                       (set 'bounds (mt--bounds-of-thing-at-point thing))
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
                     (set 'bounds (mt--bounds-of-thing-at-point thing))
                     (set 'pos (point))
                     (not (= pos (cdr bounds)))
                     (< pos-ini pos)))
          (forward-char 1))
        (goto-char (cdr bounds))))
    bounds))

(defun mt-backward-thing (arg &optional thing delimiter len)
  (setq thing (or thing mt--to-thing)
        len (or len 1))
  (let (bounds
        pos
        (pos-ini (point)))
    (if delimiter
        (dotimes (i arg)
          (while (not (and
                       (set 'bounds (mt--bounds-of-thing-at-point thing))
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
                     (set 'bounds (mt--bounds-of-thing-at-point thing))
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
  (let* ((from-sbs (mt--bounds-of-thing-at-point-or-region mt--from-thing))
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
  (let* ((from-sbs (mt--bounds-of-thing-at-point-or-region mt--from-thing))
         (from (mt-kill-rectangle-or-bounds from-sbs)))
    (goto-char (car (cdr from-sbs)))
    (forward-char arg)
    (let ((pos (point)))
      (mt-insert-rectangle from 1)
      (mt-push-mark-all (car from-sbs))
      (goto-char pos))))

(defun mt-interchange-thing-up (arg)
  (let* ((column (current-column))
         (from-sbs (mt--bounds-of-thing-at-point-or-region mt--from-thing))
         (from (mt-kill-rectangle-or-bounds from-sbs)))
    (goto-char (car (cdr from-sbs)))
    (mt-up-thing arg column mt--to-thing)
    (let* ((to-bs (mt--bounds-of-thing-at-point mt--to-thing))
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
         (from-sbs (mt--bounds-of-thing-at-point-or-region mt--from-thing))
         (from (mt-kill-rectangle-or-bounds from-sbs)))
    (goto-char (car (cdr from-sbs)))
    (mt-forward-line (1- (length from)))
    (when (mt-newline-ending (car from))
      (cl-decf arg))
    (mt-down-thing arg column mt--to-thing)
    (let* ((to-bs (mt--bounds-of-thing-at-point mt--to-thing))
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
  (let ((from-sbs (mt--bounds-of-thing-at-point-or-region mt--from-thing)))
    (goto-char (car (cdr from-sbs)))
    (mt-backward-thing arg mt--to-thing
                       (and (eq mt--to-thing 'sexp)
                            (let* ((pos (car (cdr from-sbs)))
                                   (delimiter (buffer-substring-no-properties
                                               pos (1+ pos))))
                              (if (member
                                   delimiter
                                   '("\"" "'" "(" "{" "["))
                                  delimiter
                                "[^\"'({[]"))))
    (let* ((to-bs (mt--bounds-of-thing-at-point mt--to-thing))
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
  (let* ((from-sbs (mt--bounds-of-thing-at-point-or-region mt--from-thing))
         (from (mt-kill-rectangle-or-bounds from-sbs)))
    (goto-char (car (cdr from-sbs)))
    (goto-char (car (mt-forward-thing arg mt--to-thing
                                      (and (eq mt--to-thing 'sexp)
                                           (let ((delimiter (substring (car from) 0 1)))
                                             (if (member
                                                  delimiter
                                                  '("\"" "'" "(" "{" "["))
                                                 delimiter
                                               "[^\"'({[]"))))))
    (let* ((to-bs (mt--bounds-of-thing-at-point mt--to-thing))
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
            (mt--bounds-of-thing-at-point mt--from-thing))
          (sort points '<)))
        (neg (> 0 arg))
        listed-bounds)
    (mapc (lambda (x)
            (push (car x) listed-bounds)
            (push (cdr x) listed-bounds)) bounds)
    (if (not (sorted-p listed-bounds '>))
        (error "move-thing: %s's bounds overlap" mt--from-thing)
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
  (set-marker mt--marker nil)
  (if (not (eql mt--from-thing mt--to-thing))
      (while (not (eql mt--from-thing mt--to-thing))
        (mt--cycle-to-things))
    (let ((init-thing (ring-ref mt--from-thing-ring 0))
          current-thing found)
      (while (not (or found
                      (eql init-thing current-thing)))
        (setq found t
              current-thing (mt--cycle-from-things))
        (condition-case nil
            (let ((bounds (mt--bounds-of-thing-at-point current-thing)))
              (pulse-momentary-highlight-region (car bounds) (cdr bounds)))
          (error (set 'found nil)))))))

(defun mt-cycle-to-things ()
  (interactive)
  (set-marker mt--marker nil)
  (mt--cycle-to-things)
  (force-mode-line-update))

(defun mt-toggle-interchange-things ()
  (interactive)
  (set-marker mt--marker nil)
  (setq mt-interchange-things (not mt-interchange-things))
  (force-mode-line-update))

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


(provide 'move-thing)
;;; move-thing.el ends here
