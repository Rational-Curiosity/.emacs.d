(defun minimap-apply-semantic-overlays (tags)
  "Apply semantic overlays to the minimap.
TAGS is the list of tags.  If it is t, fetch tags from buffer."
  (when (and tags
             minimap-mode)
    (with-current-buffer minimap-active-buffer
      (let (tag class ov ovnew my-type)
        (when (eq tags t)
          (setq tags (semantic-fetch-tags)))
        (while tags
          (setq tag (car tags))
          (setq tags (cdr tags))
          (setq my-type (semantic-tag-type tag))
          (if (equal my-type "namespace")
              (setq tags (append (semantic-tag-get-attribute tag :members) tags))
            (progn
              (setq class (semantic-tag-class tag))
              (setq ov (semantic-tag-overlay tag))
              (when (and (overlayp ov)
                         (or (eq class 'function)
                             (eq class 'type)
                             (eq class 'variable)))
                (with-current-buffer minimap-buffer-name
                  (let* ((start (overlay-start ov))
                         (end (overlay-end ov))
                         (name (semantic-tag-name tag))
                         (lstart (line-number-at-pos start))
                         (lend (line-number-at-pos end)))
                    ;; First, remove old Semantic overlays.
                    (remove-overlays start end 'minimap-semantic t)
                    (if minimap-tag-only
                        ;; Now put the new ones.
                        (overlay-put
                         (setq ovnew (make-overlay start end))
                         'face `(:background ,(face-background
                                               (intern (format "minimap-semantic-%s-face"
                                                               (symbol-name class))))
                                             :foreground
                                             ,(face-background
                                               (intern (format "minimap-semantic-%s-face"
                                                               (symbol-name class))))
                                             ))
                      ;; Now put the new ones.
                      (overlay-put
                       (setq ovnew (make-overlay start end))
                       'face `(:background ,(face-background
                                             (intern (format "minimap-semantic-%s-face"
                                                             (symbol-name class)))))))
                    (overlay-put ovnew 'priority 4)
                    (when (and (eq class 'function)
                               (> (- lend lstart) 5))
                      (overlay-put ovnew 'priority 1)
                      (overlay-put ovnew 'minimap-semantic t)
                      (overlay-put (setq ovnew (make-overlay start (progn (goto-char start) (point-at-eol))))
                                   'display (make-string 200 ?\u203E))
                      (overlay-put ovnew 'minimap-semantic t)
                      (overlay-put ovnew 'face `(:foreground ,(face-foreground 'default) :overline nil))
                      (overlay-put ovnew 'priority 8)
                      (overlay-put (setq ovnew (make-overlay (progn (goto-char end) (point-at-bol)) end))
                                   'display (make-string 200 ?_))
                      (overlay-put ovnew 'face `(:foreground ,(face-foreground 'default)))
                      (overlay-put ovnew 'minimap-semantic t)
                      (overlay-put ovnew 'priority 8))
                    (setq start
                          (minimap-line-to-pos (/ (+ lstart lend) 2)))
                    (goto-char start)
                    (while (looking-at "^$")
                      (forward-line -1))
                    (setq start (point))
                    (setq end (progn (goto-char start) (point-at-eol)))
                    (setq ovnew (make-overlay start end))
                    (overlay-put ovnew 'face (format "minimap-semantic-%s-face"
                                                     (symbol-name class)))
                    (overlay-put ovnew 'display (concat " " name " "))
                    (overlay-put ovnew 'priority 7)
                    (overlay-put ovnew 'minimap-semantic t)))))))))))


(provide 'minimap-bug)
