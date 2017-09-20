;;; company-extensions-config.el --- Configure company

;;; Commentary:

;;; Code:


;; [ required
;; sudo apt-get install libclang-3.4-dev clang-3.4 clang-format-3.4 clang-modernize-3.4 clang
;; ]
;;(add-to-list 'load-path "~/.emacs.d/elpa/company-0.8.12")
;;(add-to-list 'load-path "~/.emacs.d/elpa/elpa/company-c-headers-20150801.901")
(require 'company)
(require 'company-template)
(require 'company-capf)
(require 'company-c-headers)
(require 'company-yasnippet)
(add-hook 'after-init-hook 'global-company-mode)
;; [ disable slow 'company-semantic'. comment with ede-project
;;(delete 'company-semantic company-backends)
;; ]
(defun toggle-company-semantic ()
  "Toggle semantic backend."
  (interactive)
  (if (memq 'company-semantic company-backends)
      (delete 'company-semantic company-backends)
    (add-to-list 'company-backends 'company-semantic)))
;; (define-key c-mode-map  [(tab)] 'company-complete)
;; (define-key c++-mode-map  [(tab)] 'company-complete)
(define-key company-active-map [return] nil)
(define-key company-active-map (kbd "RET") nil)
(define-key company-active-map [tab] #'company-complete-selection)
(define-key company-active-map (kbd "TAB") #'company-complete-selection)
(define-key company-active-map [(control tab)] #'company-complete-common-or-cycle)
;;(define-key company-mode-map [(control tab)] 'company-complete)
;; `company-complete` conflicts with `company-template-forward-field` with TAB #515
(define-key company-template-nav-map [tab] nil)
(define-key company-template-nav-map (kbd "TAB") nil)
(define-key company-template-nav-map (kbd "<C-tab>") #'company-template-forward-field)
(define-key company-template-nav-map [(control tab)] #'company-template-forward-field)

(custom-set-faces
 '(company-preview
   ((t (:foreground "darkgray" :underline t))))
 '(company-preview-common
   ((t (:inherit company-preview))))
 '(company-tooltip
   ((t (:background "lightgray" :foreground "black"))))
 '(company-tooltip-selection
   ((t (:background "steelblue" :foreground "white"))))
 '(company-tooltip-common
   ((((type x)) (:inherit company-tooltip :weight bold))
    (t (:inherit company-tooltip))))
 '(company-tooltip-common-selection
   ((((type x)) (:inherit company-tooltip-selection :weight bold))
    (t (:inherit company-tooltip-selection)))))


;; company-c-headers
(add-to-list 'company-backends 'company-c-headers)
;; company c++ system headers
(with-eval-after-load 'cc-mode
  (require 'semantic/bovine/gcc)
  (let ((dirs (semantic-gcc-get-include-paths "c++")))
    (dolist (dir dirs)
      (add-to-list 'company-c-headers-path-system (concat dir "/"))))
  (delete-dups company-c-headers-path-system))
;; company c++ user headers
(with-eval-after-load 'c-c++-config
  (dolist (path c-c++-include-paths)
    (add-to-list 'company-c-headers-path-user path)))
;; hs-minor-mode for folding source code
;;(add-hook 'c-mode-common-hook 'hs-minor-mode)

;; Available C style:
;; “gnu”: The default style for GNU projects
;; “k&r”: What Kernighan and Ritchie, the authors of C used in their book
;; “bsd”: What BSD developers use, aka “Allman style” after Eric Allman.
;; “whitesmith”: Popularized by the examples that came with Whitesmiths C, an early commercial C compiler.
;; “stroustrup”: What Stroustrup, the author of C++ used in his book
;; “ellemtel”: Popular C++ coding standards as defined by “Programming in C++, Rules and Recommendations,” Erik Nyquist and Mats Henricson, Ellemtel
;; “linux”: What the Linux developers use for kernel development
;; “python”: What Python developers use for extension modules
;; “java”: The default style for java-mode (see below)
;; “user”: When you want to define your own style
;; (setq
;;  c-default-style "linux" ;; set style to "linux"
;;  )

;; company-web completion framework
(add-to-list 'company-backends 'company-web-html)
(add-to-list 'company-backends 'company-web-jade)
(add-to-list 'company-backends 'company-web-slim)

;;;;;;;;;;;;
;; Auxtex ;;
;;;;;;;;;;;;
(with-eval-after-load 'auctex
  (require 'company-auctex)
  (company-auctex-init))

(bind-keys
 ("<f7> ," . toggle-company-semantic)
 ("C-c y"  . company-yasnippet))


(provide 'company-extensions-config)
;;; company-extensions-config.el ends here
