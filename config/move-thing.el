;;; move-thing.el --- Move words and sexps with cursors

;;; Commentary:

;; Usage:
;; (require 'move-thing)

;; THING should be a symbol specifying a type of syntactic entity.
;; Possibilities include ‘symbol’, ‘list’, ‘sexp’, ‘defun’,
;; ‘filename’, ‘url’, ‘email’, ‘word’, ‘sentence’, ‘whitespace’,
;; ‘line’, and ‘page’.

;;; Code:
;; Protect from errors
(defun rollback-on-error-inc ()
  (cl-incf rollback-on-error-counter))
(defun rollback-on-error-advice (orig-fun &rest args)
  "Rollback (ORIG-FUN ARGS) evaluation on error."
  (undo-boundary)
  (advice-add 'undo-boundary :before #'rollback-on-error-inc)
  (unwind-protect
      (let ((rollback-on-error-counter 1))
        (condition-case raised-error
            (apply orig-fun args)
          (error (primitive-undo rollback-on-error-counter
                                 buffer-undo-list)
                 (message "%s rolled back (%i)"
                          raised-error
                          rollback-on-error-counter))))
    (advice-remove 'undo-boundary #'rollback-on-error-inc)))
(require 'rect)
(require 'ring)
(fset 'mt-bounds-of-thing-at-point
      (if (require 'thingatpt+ nil t)
          #'tap-bounds-of-thing-at-point
        #'bounds-of-thing-at-point))


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
    (define-key map (kbd "M-S-<up>") 'mt-up)
    (define-key map (kbd "M-S-<down>") 'mt-down)
    (define-key map (kbd "M-S-<left>") 'mt-backward)
    (define-key map (kbd "M-S-<right>") 'mt-forward)
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

;; #######
;;    #    #    # # #    #  ####   ####
;;    #    #    # # ##   # #    # #
;;    #    ###### # # #  # #       ####
;;    #    #    # # #  # # #  ###      #
;;    #    #    # # #   ## #    # #    #
;;    #    #    # # #    #  ####   ####
(defvar mt-things '((word     . "w")
                    (symbol   . "s")
                    (sexp     . "e")
                    (list     . "t")
                    (defun    . "d")
                    (filename . "f")
                    (url      . "u")
                    (email    . "m")
                    (line     . "l")))

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


;;              #
;; #####       #  #    #
;; #    #     #   #    #
;; #    #    #    #    #
;; #####    #     # ## #
;; #   #   #      ##  ##
;; #    # #       #    #
(defun mt-insert-rectangle (rectangle arg)
  (let ((lines (if (<= 0 arg) rectangle (nreverse rectangle)))
        (insertcolumn (current-column)))
    (undo-boundary)
    (insert-for-yank (car lines))
    (setq lines (cdr lines))
    (while lines
      (forward-line arg)
      (or (bolp) (insert ?\n))
      (move-to-column insertcolumn t)
      (insert-for-yank (car lines))
      (setq lines (cdr lines)))))

(defun mt-kill-rectangle-or-bounds (arg)
  (unless (cdr arg)
    (error "%s not found, kill imposible" arg))
  (cl-case (car arg)
    ('rectangle
     (undo-boundary)
     (delete-extract-rectangle (car (cdr arg)) (cdr (cdr arg))))
    ((region bounds)
     (prog1
         (list (buffer-substring-no-properties (car (cdr arg)) (cdr (cdr arg))))
       (undo-boundary)
       (delete-region (car (cdr arg)) (cdr (cdr arg)))))))

(defun mt-kill-bounds (arg)
  (unless arg
    (error "Thing not found, kill imposible"))
  (prog1
      (buffer-substring-no-properties (car arg) (cdr arg))
    (undo-boundary)
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

(defun mt-forward-thing (arg &optional thing)
  (setq thing (or thing mt-to-thing))
  (dotimes (i arg)
    (let (bounds)
      (while (not (and
                   (set 'bounds (mt-bounds-of-thing-at-point thing))
                   (not (= (point) (cdr bounds)))))
        (forward-char 1))
      (goto-char (1- (cdr bounds))))))

(defun mt-backward-thing (arg &optional thing)
  (setq thing (or thing mt-to-thing))
  (dotimes (i arg)
    (let (bounds)
      (while (not (and
                   (set 'bounds (mt-bounds-of-thing-at-point thing))
                   (not (= (point) (car bounds)))))
        (backward-char 1))
      (goto-char (car bounds)))))


;; #     #
;; ##   ##  ####  #    # ###### #    # ###### #    # #####
;; # # # # #    # #    # #      ##  ## #      ##   #   #
;; #  #  # #    # #    # #####  # ## # #####  # #  #   #
;; #     # #    # #    # #      #    # #      #  # #   #
;; #     # #    #  #  #  #      #    # #      #   ##   #
;; #     #  ####    ##   ###### #    # ###### #    #   #
(defun mt-newline-ending (str)
  (char-equal ?\n (aref str (1- (length str)))))

(defun mt-move-thing-up (arg)
  (let* ((column (current-column))
         (from-sbs (mt-bounds-of-thing-at-point-or-region mt-from-thing))
         (from (mt-kill-rectangle-or-bounds from-sbs)))
    (goto-char (car (cdr from-sbs)))
    (mt-up-thing arg column mt-to-thing)
    (let* ((to-bs (mt-bounds-of-thing-at-point mt-to-thing))
           (to (mt-kill-bounds to-bs)))
      (goto-char (- (car (cdr from-sbs)) (length to)))
      (undo-boundary)
      (insert to)
      (goto-char (car to-bs))
      (mt-insert-rectangle from 1)
      (cl-case (car from-sbs)
        ('rectangle
         (rectangle-mark-mode)
         (push-mark)
         (setq deactivate-mark nil))
        ('region
         (push-mark)
         (setq deactivate-mark nil)))
      (goto-char (car to-bs)))))
(advice-add 'mt-move-thing-backward :around #'rollback-on-error-advice)

(defun mt-move-thing-down (arg)
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
      (undo-boundary)
      (insert to)
      (let ((pos (+ (length to) (car to-bs))))
        (goto-char pos)
        (mt-insert-rectangle from 1)
        (cl-case (car from-sbs)
          ('rectangle
           (rectangle-mark-mode)
           (push-mark)
           (setq deactivate-mark nil))
          ('region
           (push-mark)
           (setq deactivate-mark nil)))
        (goto-char pos)))))
(advice-add 'mt-move-thing-forward :around #'rollback-on-error-advice)

(defun mt-move-thing-backward (arg)
  (let* ((from-sbs (mt-bounds-of-thing-at-point-or-region mt-from-thing))
         (from (mt-kill-rectangle-or-bounds from-sbs)))
    (goto-char (car (cdr from-sbs)))
    (mt-backward-thing arg mt-to-thing)
    (let* ((to-bs (mt-bounds-of-thing-at-point mt-to-thing))
           (to (mt-kill-bounds to-bs)))
      (goto-char (- (car (cdr from-sbs)) (length to)))
      (undo-boundary)
      (insert to)
      (goto-char (car to-bs))
      (mt-insert-rectangle from 1)
      (cl-case (car from-sbs)
        ('rectangle
         (rectangle-mark-mode)
         (push-mark)
         (setq deactivate-mark nil))
        ('region
         (push-mark)
         (setq deactivate-mark nil)))
      (goto-char (car to-bs)))))
(advice-add 'mt-move-thing-backward :around #'rollback-on-error-advice)

(defun mt-move-thing-forward (arg)
  (let* ((from-sbs (mt-bounds-of-thing-at-point-or-region mt-from-thing))
         (from (mt-kill-rectangle-or-bounds from-sbs)))
    (goto-char (car (cdr from-sbs)))
    (mt-forward-thing arg mt-to-thing)
    (let* ((to-bs (mt-bounds-of-thing-at-point mt-to-thing))
           (to (mt-kill-bounds to-bs)))
      (goto-char (car (cdr from-sbs)))
      (undo-boundary)
      (insert to)
      (let ((pos (+ (length to) (car to-bs))))
        (goto-char pos)
        (mt-insert-rectangle from 1)
        (cl-case (car from-sbs)
          ('rectangle
           (rectangle-mark-mode)
           (push-mark)
           (setq deactivate-mark nil))
          ('region
           (push-mark)
           (setq deactivate-mark nil)))
        (goto-char pos)))))
(advice-add 'mt-move-thing-forward :around #'rollback-on-error-advice)


;; ###
;;  #  #    # ##### ###### #####    ##    ####  ##### # #    # ######
;;  #  ##   #   #   #      #    #  #  #  #    #   #   # #    # #
;;  #  # #  #   #   #####  #    # #    # #        #   # #    # #####
;;  #  #  # #   #   #      #####  ###### #        #   # #    # #
;;  #  #   ##   #   #      #   #  #    # #    #   #   #  #  #  #
;; ### #    #   #   ###### #    # #    #  ####    #   #   ##   ######
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
