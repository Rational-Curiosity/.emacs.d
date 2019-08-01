;;; ido-config.el --- Configure ido

;;; Commentary:

;; Usage:
;; (require 'ido-config)

;;; Code:

(require 'ag)
(require 'ido)
(require 'ido-occur)

;; Colors
(face-spec-set 'ido-subdir '((t (:foreground "#66ff00"))))
(face-spec-set 'ido-first-match '((t (:foreground "#ccff66"))))
(face-spec-set 'ido-only-match '((t (:foreground "#ffcc33"))))
(face-spec-set 'ido-indicator '((t (:foreground "#ffffff"))))
(face-spec-set 'ido-incomplete-regexp '((t (:foreground "#ffffff"))))

(defface ido-substring-match-face
  '((t (:foreground "#ffd700" :bold t :underline t)))
  "Face used by ido-config for the matched part.")
;; ido mode
(setq ido-enable-flex-matching nil
      ido-enable-regexp t
      ido-enable-prefix nil
      ido-max-prospects 20
      ido-use-filename-at-point 'guess
      ido-use-url-at-point t
      ido-create-new-buffer 'always
      ido-use-virtual-buffers nil
      ido-default-buffer-method 'selected-window
      ido-default-file-method 'selected-window)

(ido-mode 1)
(ido-everywhere 1)

;; old like-ido completion
(icomplete-mode 1)

;; ido-completing-read+
(require 'ido-completing-read+)
(ido-ubiquitous-mode 1)
(require 'crm-custom)
(crm-custom-mode 1)

;; ido-at-point
(require 'ido-at-point)
(ido-at-point-mode)

;;;;;;;;;;;;;;;;;;;;;;
;; Custom functions ;;
;;;;;;;;;;;;;;;;;;;;;;
(bug-check-function-bytecode 'ido-completions
                             "CIlAOoURAIlAQUfGVoURAAnHQwGDJAAKgyQAyMnGyssGBiWICoNqAAKDagDMA0AhiUcCzQMhoIjIyQLKBghHxlWDUAALg0wAw4JRAM6CUQDPBgeiJYgDg2EAAomiBVCgiAKiBUFCsgW2AgKEqQAMg3wA0A04hlcB0YJXAQ4sg4sA0g04hlcB04JXAQ4tg5oA1A04hlcB1YJXAQ4ug6UA1g04glcB14JXAQuDtADYA0BQglcBAkGEBwEOL4TJAMwDQCFHBEdVgt0A2QTMBUAhIojaycwFQCEizARAIZiD5ADXgvsA2w04hu0A3A04zARAId0NOIb6AN4NOFEKP4UDAd8NOFCCVwEOMMlWgxQBDjBUghUB4EPh4uHj5OXm5+jpBgsGCyLqItTrJQYJIiJBIg4xO4VOAQ4xRwYGR1aFTgHcDTgOMQYHR8dP3g04UQ1AAg1BQFK2goc=")
(defun ido-completions (name)
  "Return the string that is displayed after the user's text.
Modified from `icomplete-completions'."
  (let* ((comps ido-matches)
         (ind (and (consp (car comps)) (> (length (cdr (car comps))) 1)
                   ido-merged-indicator))
         first)

    (if (and ind ido-use-faces)
        (put-text-property 0 1 'face 'ido-indicator ind))

    (if (and ido-use-faces comps)
        (let* ((fn (ido-name (car comps)))
               (ln (length fn)))
          (setq first (copy-sequence fn))
          (put-text-property 0 ln 'face
                             (if (= (length comps) 1)
                                 (if ido-incomplete-regexp
                                     'ido-incomplete-regexp
                                   'ido-only-match)
                               'ido-first-match)
                             first)
          (if ind (setq first (concat first ind)))
          (setq comps (cons first (cdr comps)))))

    (cond ((null comps)
           (cond
            (ido-show-confirm-message
             (or (nth 10 ido-decorations) " [Confirm]"))
            (ido-directory-nonreadable
             (or (nth 8 ido-decorations) " [Not readable]"))
            (ido-directory-too-big
             (or (nth 9 ido-decorations) " [Too big]"))
            (ido-report-no-match
             (nth 6 ido-decorations))  ;; [No match]
            (t "")))
          (ido-incomplete-regexp
           (concat " " (car comps)))
          ((null (cdr comps))          ;one match
           (concat (if (if (not ido-enable-regexp)
                           (= (length (ido-name (car comps))) (length name))
                         ;; We can't rely on the length of the input
                         ;; for regexps, so explicitly check for a
                         ;; complete match
                         (string-match name (ido-name (car comps)))
                         (string-equal (match-string 0 (ido-name (car comps)))
                                       (ido-name (car comps))))
                       ""
                     ;; When there is only one match, show the matching file
                     ;; name in full, wrapped in [ ... ].
                     (concat
                      (or (nth 11 ido-decorations) (nth 4 ido-decorations))
                      (ido-name (car comps))
                      (or (nth 12 ido-decorations) (nth 5 ido-decorations))))
                   (if (not ido-use-faces) (nth 7 ido-decorations))))  ;; [Matched]
          (t                           ;multiple matches
           (let* ((items (if (> ido-max-prospects 0) (1+ ido-max-prospects) 999))
                  (alternatives
                   (apply
                    #'concat
                    (cdr (apply
                          #'nconc
                          (mapcar
                           (lambda (com)
                             (setq com (ido-name com))
                             (setq items (1- items))
                             (cond
                              ((< items 0) ())
                              ((= items 0) (list (nth 3 ido-decorations))) ; " | ..."
                              (t
                               (list (or ido-separator (nth 2 ido-decorations)) ; " | "
                                     (let ((str (substring com 0)))
                                       (when ido-use-faces                                                  ;; +
                                         (if (and
                                              ;; ido-use-faces ;; -
                                              (not (string= str first))
                                              (ido-final-slash str))
                                             (put-text-property 0 (length str) 'face 'ido-subdir str))
                                         (if (/= 0 (length name))                                           ;; +
                                             (let ((regexp (if ido-enable-regexp name (regexp-quote name))) ;; +
                                                   (pos 0)                                                  ;; +
                                                   end)                                                     ;; +
                                               (while (and (string-match regexp str pos)                    ;; +
                                                           (< pos (setq end (match-end 0))))                ;; +
                                                 (ignore-errors                                             ;; +
                                                   (add-face-text-property (match-beginning 0)              ;; +
                                                                           (match-end 0)                    ;; +
                                                                           'ido-substring-match-face        ;; +
                                                                           nil str))                          ;; +
                                                 (setq pos end)))))                                         ;; +
                                       str)))))
                           comps))))))

             (concat
              ;; put in common completion item -- what you get by pressing tab
              (if (and (stringp ido-common-match-string)
                       (> (length ido-common-match-string) (length name)))
                  (concat (nth 4 ido-decorations)   ;; [ ... ]
                          (substring ido-common-match-string (length name))
                          (nth 5 ido-decorations)))
              " [" (int-to-string (length ido-matches)) "]" ;; +
              ;; list all alternatives
              (nth 0 ido-decorations)  ;; { ... }
              alternatives
              (nth 1 ido-decorations)))))))

;; (defun ido-completions-advice (orig-fun name)
;;   "Colorize ido-completions text result."
;;   (let ((text (funcall orig-fun name)))
;;     (if (and (/= 0 (length name))
;;              (/= 0 (length text)))
;;         (let ((regexp (if ido-enable-regexp name (regexp-quote name)))
;;               (beg (if (string-match "^ \\[[^]]*\\]" text)
;;                        (match-end 0)
;;                      0))
;;               pos)
;;           (setq pos beg)
;;           (while (string-match regexp text pos)
;;             (ignore-errors
;;               (add-face-text-property (match-beginning 0)
;;                                       (match-end 0)
;;                                       'ido-substring-match-face
;;                                       nil text))
;;             (setq pos (match-end 0)))
;;           (if (= 0 beg)
;;               (concat " [" (int-to-string (length ido-matches)) "]" text)
;;             (concat (substring text 0 beg)
;;                     "[" (int-to-string (length ido-matches)) "]"
;;                     (substring text beg))))
;;       (concat " [" (int-to-string (length ido-matches)) "]" text))))
;; (advice-add 'ido-completions :around 'ido-completions-advice)

;; (defun self-insert-command-advice (orig-fun N)
;;   (if (<= 0 N)
;;       (funcall orig-fun N)
;;     (setq N (- N))
;;     (funcall orig-fun N)
;;     (backward-char N)
;;     (dotimes (i N)
;;       (insert "\\")
;;       (forward-char))))
;; (advice-add 'self-insert-command :around 'self-insert-command-advice)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; hippy-expand with ido ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun hippie-completion-at-point ()
  (let ((bounds (bounds-of-thing-at-point 'symbol))
        (expands (hippie-expand-completions)))
    (when (and bounds expands)
      (let* ((beg (car bounds))
             (end (cdr bounds))
             (symbol (buffer-substring-no-properties beg end)))
        (list beg end
              (mapcar (lambda (s) (replace-regexp-in-string (concat ".*" (regexp-quote symbol)) symbol s)) expands)
              :exclusive 'no)))))

(defun hippie-expand-completions (&optional hippie-expand-function)
  "Return the full list of possible completions generated by `hippie-expand'.
    The optional argument can be generated with `make-hippie-expand-function'."
  (let ((this-command 'hippie-expand-completions)
        (last-command last-command)
        (buffer-modified (buffer-modified-p))
        (hippie-expand-function (or hippie-expand-function 'hippie-expand)))
    (cl-letf (((symbol-function 'ding) (lambda ()))) ; avoid the (ding) when hippie-expand exhausts its options.
      (while (progn
                   (funcall hippie-expand-function nil)
                   (setq last-command 'hippie-expand-completions)
                   (not (equal he-num -1)))))
    ;; Evaluating the completions modifies the buffer, however we will finish
    ;; up in the same state that we began.
    (set-buffer-modified-p buffer-modified)
    ;; Provide the options in the order in which they are normally generated.
    (delete he-search-string (reverse he-tried-table))))

(defun ido-hippie-expand-with (hippie-expand-function)
  "Offer ido-based completion using the specified hippie-expand function."
  (let* ((options (hippie-expand-completions hippie-expand-function))
         (selection (and options
                         (ido-completing-read "Completions: " options))))
    (if selection
        (he-substitute-string selection t)
      (message "No expansion found"))))

(defun ido-hippie-expand ()
  "Offer ido-based completion for the word at point."
  (interactive)
  (ido-hippie-expand-with 'hippie-expand))

;; Dangerous
;; (setcdr (last completion-at-point-functions) '(hippie-completion-at-point))

;;;;;;;;;;;;;;;;;;;;;;;
;; Semantic with ido ;;
;;;;;;;;;;;;;;;;;;;;;;;
(defun ido-semantic-complete-jump (sym)
  (interactive (list
                (thing-at-point 'symbol)))
  (let ((tags (custom/semantic/deep-brute-tags-query sym))
        chosen-tag)
    (if (< 1 (length tags))
        (let* ((summaries (mapcar #'custom/semantic/tag-summary tags))
               (chosen-summary (ido-completing-read "Choose tag: "
                                                    summaries)))
          (setq chosen-tag (custom/semantic/get-tag-by-summary chosen-summary
                                                          tags)))
      (setq chosen-tag (car tags)))
    (if chosen-tag
        (progn
          (if (boundp 'semantic-tags-location-ring)
              (ring-insert semantic-tags-location-ring (point-marker)))
          (push-mark)
          (find-file (nth 2 chosen-tag))
          (goto-char (nth 3 chosen-tag))
          (recenter)
          (pulse-momentary-highlight-region (nth 3 chosen-tag)
                                            (nth 4 chosen-tag)))
      (select-window (get-buffer-window (ag/search sym (ag/project-root default-directory)))))))

(defun custom/semantic/tag-summary (tag)
  (format "%s:%s -> %s"
          (nth 0 tag)
          (nth 1 tag)
          (nth 2 tag)))

(defun custom/semantic/get-tag-by-summary (summary tags)
  (let ((res nil))
    (dolist (tag tags)
      (if (and (not res)
               (string= summary
                        (custom/semantic/tag-summary tag)))
          (setq res tag)))
    res))

(defun custom/semantic/deep-brute-tags-query (sym &optional buff)
  (let ((acc nil))
    (dolist (tag (semanticdb-strip-find-results
                  (semanticdb-brute-deep-find-tags-for-completion
                   sym
                   (if buff buff (current-buffer)))
                  t))
      (if (semantic-tag-buffer tag)
          (setq acc (push (list (semantic-tag-class tag)
                                (semantic-tag-name tag)
                                (buffer-file-name (semantic-tag-buffer tag))
                                (semantic-tag-start tag)
                                (semantic-tag-end tag))
                          acc))))
    acc))

;;;;;;;;;;;;;;;;;
;; ido recentf ;;
;;;;;;;;;;;;;;;;;
(defun ido-recentf-open ()
  "Use `ido-completing-read' to find a recent file."
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))

;;;;;;;;;;
;; smex ;;
;;;;;;;;;;
(smex-initialize)  ;; Can be omitted. This might cause a (minimal) delay
                   ;; when Smex is auto-initialized on its first run.

;;;;;;;;;;
;; Keys ;;
;;;;;;;;;;
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
;; icomplete keys
(define-key icomplete-minibuffer-map "\M-q" 'abort-recursive-edit)
(define-key icomplete-minibuffer-map "\M-j" 'icomplete-force-complete-and-exit)
(define-key icomplete-minibuffer-map "\M-f" 'icomplete-forward-completions)
(define-key icomplete-minibuffer-map "\M-b" 'icomplete-backward-completions)

;; ido keys
(defun ido-setup-completion-map-advice ()
  (define-key ido-completion-map "\M-q" 'abort-recursive-edit)
  (define-key ido-completion-map "\M-j" 'ido-select-text)
  (define-key ido-completion-map (kbd "SPC") nil)
  (define-key ido-completion-map (kbd "M-f") 'ido-next-match)
  (define-key ido-completion-map (kbd "M-b") 'ido-prev-match)
  (define-key ido-completion-map (kbd "M-v") 'ido-preview)
  (define-key ido-completion-map (kbd "M-F") 'ido-preview-next)
  (define-key ido-completion-map (kbd "M-B") 'ido-preview-prev))
(advice-add 'ido-setup-completion-map :after 'ido-setup-completion-map-advice)

(global-set-key (kbd "M-i") #'ido-occur-at-point)
(global-set-key (kbd "M-I") #'ido-occur-from-isearch)
(global-set-key (kbd "C-x C-r") #'ido-recentf-open)


(provide 'ido-config)
;;; ido-config.el ends here
