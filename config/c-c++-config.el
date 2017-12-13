;;; c-c++-config.el --- configure and utils for c and c++

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'cc-mode
;;   (require 'c-c++-config))
;; or:
;; (with-eval-after-load 'c-c++-config
;;   )
;; never:
;; (require 'c-c++-config)

;; Do not include in this file:
;; (require 'cc-mode)

;;; Code:

(message "Importing c-c++-config")
(defvar c-c++-include-paths nil)

(defvar c-c++-default-mode-for-headers 'c++-mode)
(defvar c-c++-enable-clang-support t)

(require 'faces)
(require 'find-file)
;; Directorios para busqueda del archivo dual
(dolist (path c-c++-include-paths)
  (add-to-list 'cc-search-directories path))

(bind-key "C-x o" 'ff-find-other-file)

;;;;;;;;;;;;;;
;; C macros ;;
;;;;;;;;;;;;;;

;; Create Header Guards
(defun c-c++-header-guards ()
  "Prevent read more than one time a c or c++ header file."
  (interactive)
  (if buffer-file-name
      (let*
          ((name (file-name-nondirectory
                  (file-name-sans-extension buffer-file-name)))
           (proj-name (condition-case nil
                          (replace-regexp-in-string
                           (file-name-directory (oref (ede-current-project) file)) ""
                           (file-name-sans-extension buffer-file-name))
                        (error name)))
           (ext   (file-name-extension buffer-file-name))
           (label (concat "__" (upcase
                                (replace-regexp-in-string "[^a-zA-Z0-9_]" "_" proj-name))
                          "_" (upcase ext) "__")))
        (save-excursion
          (when (< (buffer-size) 5)
            (insert "\nclass " (upcase-initials name) "\n{\n\npublic:\n\n};\n"))
          (goto-char (point-min))
          (insert "#ifndef " label "\n#define " label "\n\n")
          (goto-char (point-max))
          (insert "\n\n#endif" " // " label)))
    (message (concat "Buffer " (buffer-name) " must have a filename"))))

;; supply by package
;; (defun toggle-camelcase-underscores ()
;;   "Toggle between camelcase and underscore notation for the symbol at point."
;;   (interactive)
;;   (save-excursion
;;     (let* ((bounds (bounds-of-thing-at-point 'symbol))
;;            (start (car bounds))
;;            (end (cdr bounds))
;;            (currently-using-underscores-p (progn (goto-char start)
;;                                                  (re-search-forward "_" end t))))
;;       (if currently-using-underscores-p
;;           (progn
;;             (upcase-initials-region start end)
;;             (replace-string "_" "" nil start end)
;;             (downcase-region start (1+ start)))
;;         (replace-regexp "\\([A-Z]\\)" "_\\1" nil (1+ start) end)
;;         (downcase-region start (cdr (bounds-of-thing-at-point 'symbol)))))))

;;;;;;;;;
;; C++ ;;
;;;;;;;;;
(defun avoid-shared-ptr-by-ref (arg)
  (interactive "P")
  (goto-char (point-min))
  (let ((counts 0))
    (while (search-forward-regexp "> *&[^&]" nil t)
      (let ((pos-end (- (point) 2)))
        (goto-char (match-beginning 0))
        (let ((angles 1))
          (while (< 0 angles)
            (backward-char 1)
            (if (char-equal (char-after (point)) ?<)
                (cl-decf angles)
              (if (char-equal (char-after (point)) ?>)
                  (cl-incf angles))))
          (search-backward-regexp "[^_a-zA-Z0-9: \t\n]" nil t)
          (if (or (char-equal (char-after (point)) ?,)
                  (char-equal (char-after (point)) ?\())
              (let ((pos-beg (point)))
                (when (and
                       (cl-search
                        "shared_ptr"
                        (buffer-substring-no-properties pos-beg pos-end))
                       (or arg
                           (progn
                             (goto-char pos-end)
                             (pulse-momentary-highlight-region (1+ pos-beg)
                                                               (1+ pos-end) 'region)
                             (y-or-n-p "Delete match? "))))
                  (delete-region pos-end (1+ pos-end))
                  (save-restriction
                    (narrow-to-region pos-beg pos-end)
                    (goto-char (point-min))
                    (if (search-forward-regexp "[^a-zA-Z_]\\(const +\\)[a-zA-Z_:]" nil t)
                      (delete-region (match-beginning 1) (match-end 1))))
                  (cl-incf counts)))))
        (goto-char pos-end)))
    (message "Found %i pointers by ref" counts)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                             ;;
;;   Funciones para compilar   ;;
;;                             ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'compile)
(setq ;compile-command "cbuild -g "
      compilation-scroll-output 'first-error)

;;;;;;;;;;;;
;; Estilo ;;
;;;;;;;;;;;;
(require 'clang-format)
(require 'clang-format-bug)
;;(setq clang-format-executable "clang-format-3.6")
;;;;;;;;;;
;; Keys ;;
;;;;;;;;;;

(dolist (mode-map (list c-mode-map c++-mode-map))
  (bind-keys :map mode-map
           ([C-M-tab] . clang-format-region)
           ("C-c c h" . c-c++-header-guards)
           ([(shift return)] . c-context-line-break)
           ("M-b" . c-beginning-of-statement)
           ("M-e" . c-end-of-statement)))
;; (define-key c-mode-map (kbd "C-c c h") 'c-c++-header-guards)
;; (define-key c++-mode-map (kbd "C-c c h") 'c-c++-header-guards)
;; (define-key c-mode-map [(shift return)] 'c-context-line-break)
;; (define-key c++-mode-map [(shift return)] 'c-context-line-break)


(provide 'c-c++-config)
;;; c-c++-config.el ends here