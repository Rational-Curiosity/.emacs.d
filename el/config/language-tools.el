;;; language-tools.el --- Translation tools

;;; Commentary:

;; Usage:
;; (require 'language-tools)

;;; Code:

(defvar language-url-builder #'language-url-wordreference)
(defvar language-phonemic-script-regex ">/\\([^/]*\\)/<")
(defvar language-translation-regex-wordreference "class='ToWrd' >\\([^<]*\\)<")
(defvar language-items-number 6)

(defun decode-coding-string-to-current (string)
  "Decode STRING to buffer file codings system."
  (decode-coding-string string buffer-file-coding-system))

(defun language-url-wordreference (word &optional from to)
  "Build url request for WORD translation from language FROM to language TO."
  (if to
      (if (string-equal from "en")
          (format "http://www.wordreference.com/%s/translation.asp?tranword=%s" to word)
        (format "http://www.wordreference.com/%s/%s/translation.asp?tranword=%s" from to word))
    (cond
     ((string-equal from "es")
      (concat "http://www.wordreference.com/definicion/" word))
     (t
      (concat "http://www.wordreference.com/definition/" word)))))

(defun language-url-request-to-buffer (word &optional from to)
  "Synchronous request translation of WORD from language FROM to language TO."
  (url-retrieve-synchronously
       (funcall language-url-builder word from to)))

(defun language-get-phonemic-script (word &optional from)
  "Get phonemic script of WORD in language FROM."
  (with-current-buffer
      (language-url-request-to-buffer word from)
    (goto-char (point-min))
    (if (re-search-forward language-phonemic-script-regex nil t)
        (match-string 1)
      (error "Phonemic script not found"))))

(defun language-get-translation (word from to &optional items)
  "Get items posible translations of WORD from FROM to TO.
If no ITEMS `language-items-number'."
  (with-current-buffer
      (language-url-request-to-buffer word from to)
    (goto-char (point-min))
    (let ((matches ())
          (items-number (or items language-items-number)))
      (while (and (re-search-forward language-translation-regex-wordreference nil t)
                  (< 0 items-number))
        (let ((item (string-trim (match-string 1))))
          (unless (member item matches)
            (cl-decf items-number)
            (push item matches))))
      (if matches
          (nreverse matches)
        (error "Translation not found")))))

;; (require 'subr-x)
;; (defun language-get-phonemic-script-and-translation (word from to &optional items)
;;   "Get ITEMS posible translations of WORD from FROM to TO, with phonemic script."
;;   (with-current-buffer
;;       (language-url-request-to-buffer word from to)
;;     (goto-char (point-min))
;;     (if (re-search-forward language-phonemic-script-regex nil t)
;;         (let ((matches ())
;;               (items-number (or items language-items-number))
;;               (phonemic-script (decode-coding-string (match-string 1) 'utf-8)))
;;           (while (and (re-search-forward language-translation-regex-wordreference nil t)
;;                       (< 0 items-number))
;;             (let ((item (string-trim (match-string 1))))
;;               (unless (member item matches)
;;                 (cl-decf items-number)
;;                 (push item matches))))
;;           (cons phonemic-script (nreverse matches)))
;;       (error "Phonemic script not found"))))

(defun language-goto-insertion-point ()
  "Goto proper insertion point."
  (let ((curr-char (char-after (point))))
    (condition-case-unless-debug nil
        (when (and curr-char
                   (memq (get-char-code-property curr-char 'general-category)
                         '(Ll Lu Lo Lt Lm Mn Mc Me Nl)))
          (right-word 1))
      (error nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'thingatpt)
;;(mapconcat 'identity matches ", ")

(defun language-en-es-translation-at-point (&optional items)
  "Get ITEMS translations of the word at point.
By default insert it, with prefix display a message with it."
  (interactive "P")
  (cond
   ((or (equal items '(4))
        buffer-read-only)
    (let ((translation
           (language-get-translation
            (thing-at-point 'word 'no-properties) "en" "es" 3)))
      (message (mapconcat 'decode-coding-string-to-current
                          translation ", "))))
   (t
    (let ((translation
           (language-get-translation
            (thing-at-point 'word 'no-properties) "en" "es" items)))
      (language-goto-insertion-point)
      (insert
       (concat " "
               (mapconcat 'decode-coding-string-to-current
                          translation ", ")))))))

(defun language-en-es-phonemic-script-and-translation-at-point (&optional items)
  "Get the phonemic script and ITEMS translations of the word at point.
By default insert it, with prefix display a message with it."
  (interactive "P")
  (cond
   ((or (equal items '(4))
        buffer-read-only)
    (let ((translation
           `(,(language-get-phonemic-script (thing-at-point 'word 'no-properties)) .
             ,(language-get-translation
               (thing-at-point 'word 'no-properties) "en" "es" 3))))
      (message
       (concat "/"
               (decode-coding-string-to-current (car translation))
               "/ "
               (mapconcat 'decode-coding-string-to-current
                          (cdr translation) ", ")))))
   (t
    (let ((translation
           `(,(language-get-phonemic-script (thing-at-point 'word 'no-properties)) .
             ,(language-get-translation
               (thing-at-point 'word 'no-properties) "en" "es" items))))
      (language-goto-insertion-point)
      (insert
       (concat " /"
               (decode-coding-string-to-current (car translation))
               "/ "
               (mapconcat 'decode-coding-string-to-current
                          (cdr translation) ", ")))))))

(defun language-phonemic-script-at-point (&optional paren)
  "Get the phonemic script of the word at point.
By default insert it, with prefix display a message with it."
  (interactive "P")
  (cond
   ((or (equal paren '(4))
        buffer-read-only)
    (message
     (concat "/"
             (decode-coding-string-to-current
              (language-get-phonemic-script (thing-at-point 'word 'no-properties)))
             "/")))
   (t
    (let ((parenthesis (or paren "/")))
      (language-goto-insertion-point)
      (insert
       (concat " "
               parenthesis
               (decode-coding-string-to-current
                (language-get-phonemic-script (thing-at-point 'word 'no-properties)))
               parenthesis))))))

(defvar language-text-to-speak-process nil)

(require 'guess-language)
(add-hook 'text-mode-hook 'guess-language-mode)
(defface guess-language-mode-line
  '((t  (:foreground "#822")))
  "Face used in search mode for titles."
  :group 'guess-language)
(setcar (cdr (assq 'guess-language-mode minor-mode-alist))
        '(:eval
          (propertize (format "%s" (or guess-language-current-language "∅"))
                      'face 'guess-language-mode-line)))
(setq guess-language-languages '(en es)
      guess-language-min-paragraph-length 35)

(defun language-text-to-speak-region (start end)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list (point) (point-max))))
  (let ((string (buffer-substring-no-properties start end))
        (language (symbol-name (guess-language-region start end))))
    (setq language-text-to-speak-process
          (cond
           ((executable-find "espeak")
            (start-process "*espeak-process*" nil
                           "espeak" "--stdin" "-v" language))
           ((executable-find "festival")
            (make-process
             :name "*festival-process*"
             :command (list "festival" "--tts" "--language"
                            (pcase language
                              ("es" "spanish")
                              ("en" "english")))
             :coding 'latin-1))))
    (process-send-string language-text-to-speak-process (concat string "\n"))
    (process-send-eof language-text-to-speak-process)))

(defun language-text-to-speak-stop ()
  (interactive)
  (if language-text-to-speak-process
      (interrupt-process language-text-to-speak-process)))

;;;;;;;;;;
;; Keys ;;
;;;;;;;;;;
(defun language-tools-config ()
  (mapc (lambda (x)
          (global-set-key
           (kbd (concat "C-c g " (car x))) (cdr x)))
        '(("p" . language-phonemic-script-at-point)
          ("t" . language-en-es-translation-at-point)
          ("b" . language-en-es-phonemic-script-and-translation-at-point)
          ("R" . language-text-to-speak-region)
          ("S" . language-text-to-speak-stop))))


(provide 'language-tools)
;;; language-tools.el ends here
