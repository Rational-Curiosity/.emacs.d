;;; helm-extensions-config.el --- Configure helm

;;; Commentary:

;; Usage:
;; (require 'helm-extensions-config)

;;; Code:

(require 'helm)
(require 'helm-config)
(require 'helm-files)
(require 'helm-command)

;; Fix helm-occur load org
(require 'helm-utils)
(require 'helm-bug)

(setq helm-completion-mode-string ""
      helm-input-idle-delay 0.01
      helm-cycle-resume-delay 1.0
      ;;helm-follow-input-idle-delay 1.0
      ;; config
      helm-display-header-line nil
      helm-split-window-inside-p t
      helm-move-to-line-cycle-in-source t
      helm-ff-file-name-history-use-recentf t
      helm-scroll-amount 8
      helm-buffer-max-length 20
      helm-autoresize-max-height 40
      helm-autoresize-min-height 5
      helm-M-x-always-save-history t)

(set-face-attribute 'helm-selection nil :background "purple")
(set-face-attribute 'helm-source-header nil :height 1.0)

(bind-key* "C-x M-x" 'execute-extended-command)

(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "M-x") 'undefined)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
(global-set-key (kbd "C-x C-r") 'helm-recentf)


(defun helm-decrease-buffer-max-length ()
  (interactive)
  (with-helm-alive-p
    (let* ((buf (helm-get-selection))
           (preselect (helm-buffer--get-preselection buf)))
      (setq helm-buffer-max-length (- helm-buffer-max-length 4))
      (helm-update (lambda ()
                     (helm-awhile (re-search-forward preselect nil t)
                       (helm-mark-current-line)
                       (when (equal buf (helm-get-selection))
                         (cl-return t))))))))
(put 'helm-decrease-buffer-max-length 'helm-only t)

(defun helm-increase-buffer-max-length ()
  (interactive)
  (with-helm-alive-p
    (let* ((buf (helm-get-selection))
           (preselect (helm-buffer--get-preselection buf)))
      (setq helm-buffer-max-length (+ helm-buffer-max-length 4))
      (helm-update (lambda ()
                     (helm-awhile (re-search-forward preselect nil t)
                       (helm-mark-current-line)
                       (when (equal buf (helm-get-selection))
                         (cl-return t))))))))
(put 'helm-increase-buffer-max-length 'helm-only t)

;; rebind tab to run persistent action
(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
;; make TAB works in terminal
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
;; new Actions key
(define-key helm-map (kbd "C-c a")  'helm-select-action)
;; toggle buffers details
(define-key helm-buffer-map (kbd "C-*") 'helm-toggle-buffers-details)
(define-key helm-buffer-map (kbd "C-+") 'helm-increase-buffer-max-length)
(define-key helm-buffer-map (kbd "C--") 'helm-decrease-buffer-max-length)

(helm-autoresize-mode 1)
(helm-mode 1)
;; [ bind mouse
;; (defun helm-select-candidate-by-mouse (prefix event)
;;   "Select helm candidate by using mouse(click).  With PREFIX, also execute its first action."
;;   (interactive "P\ne")
;;   (if (helm-alive-p)
;;       (progn
;;         (with-helm-buffer
;;           (let* ((posn (elt event 1))
;;                  (cursor (line-number-at-pos (point)))
;;                  (pointer (line-number-at-pos (posn-point posn))))
;;             (helm--next-or-previous-line (if (> pointer cursor)
;;                                              'next
;;                                            'previous)
;;                                          (abs (- pointer cursor)))))
;;         (when prefix (helm-maybe-exit-minibuffer)))
;;     (mouse-drag-region event)))
;; (bind-keys* :map helm-map
;;             ("<down-mouse-1>" . helm-select-candidate-by-mouse)
;;             ("<mouse-1>" . ignore))
;; ]

;;;;;;;;;;;
;; occur ;;
;;;;;;;;;;;
;; (require 'helm-swoop)

;; Change the keybinds to whatever you like :)
(global-set-key (kbd "M-i") 'helm-occur)
;; (global-set-key (kbd "M-I") (lambda ()
;;                               (interactive)
;;                               (mark-thing 'symbol)
;;                               (helm-occur)))
;; (global-set-key (kbd "C-x M-i") 'helm-swoop-back-to-last-point)
;; (global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
(global-set-key (kbd "C-v") nil)
;; (global-set-key (kbd "C-v M-i") 'helm-multi-swoop-all)

;; ;; When doing isearch, hand the word over to helm-swoop
;; (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
;; ;; From helm-swoop to helm-multi-swoop-all
;; (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
;; ;; When doing evil-search, hand the word over to helm-swoop
;; ;; (define-key evil-motion-state-map (kbd "M-i") 'helm-swoop-from-evil-search)

;; ;; Instead of helm-multi-swoop-all, you can also use helm-multi-swoop-current-mode
;; (define-key helm-swoop-map (kbd "M-m") 'helm-multi-swoop-current-mode-from-helm-swoop)

;; ;; Move up and down like isearch
;; (define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
;; (define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
;; (define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
;; (define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)

;; ;; Save buffer when helm-multi-swoop-edit complete
;; (setq helm-multi-swoop-edit-save t)

;; ;; If this value is t, split window inside the current window
;; (setq helm-swoop-split-with-multiple-windows t)

;; ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
;; (setq helm-swoop-split-direction 'split-window-vertically)

;; ;; If nil, you can slightly boost invoke speed in exchange for text color
;; (setq helm-swoop-speed-or-color nil)

;; ;; ;; Go to the opposite side of line from the end or beginning of line
;; (setq helm-swoop-move-to-line-cycle t)

;; ;; Optional face for line numbers
;; ;; Face name is `helm-swoop-line-number-face`
;; (setq helm-swoop-use-line-number-face nil)

;; ;; If you prefer fuzzy matching
;; (setq helm-swoop-use-fuzzy-match nil)

;; ;; This function can pre-input keywords when helm-swoop invoked
;; (setq helm-swoop-pre-input-function
;;       (lambda ()
;;         (if (use-region-p)
;;             (buffer-substring-no-properties (region-beginning) (region-end)))))

;; (dolist (mode-map (list helm-swoop-map helm-multi-swoop-map))
;;   (bind-keys :map mode-map
;;              ("C-x n" . (lambda ()
;;                           (interactive)
;;                           (insert "\\ .*")))
;;              ("C-x i" . (lambda ()
;;                           (interactive)
;;                           (insert "\\ .*[0-9]+")))
;;              ("C-x f" . (lambda ()
;;                           (interactive)
;;                           (insert "[0-9]*\\.[0-9]+\\|[0-9]+\\.")))
;;              ("C-x d" . (lambda ()
;;                           (interactive)
;;                           (insert "[0-9]\\{4\\}-[0-9][0-9]-[0-9][0-9]")))
;;              ("C-x D" . (lambda ()
;;                           (interactive)
;;                           (insert "[0-9]\\{8\\}")))))

;;;;;;;;;;;;;;;
;;  company  ;;
;;;;;;;;;;;;;;;
(autoload 'helm-company "helm-company") ;; Not necessary if using ELPA package
(with-eval-after-load 'company
  (define-key company-mode-map (kbd "C-:") 'helm-company)
  (define-key company-active-map (kbd "C-:") 'helm-company))


;;;;;;;;;;;;
;; cscope ;;
;;;;;;;;;;;;
;; ;; Enable helm-cscope-mode
;; (add-hook 'c-mode-common-hook #'helm-cscope-mode)

;; ;; key bindings
;; (with-eval-after-load 'helm-cscope
;;   (bind-keys :map helm-cscope-mode-map
;;              ("C-c s s" . helm-cscope-find-this-symbol)
;;              ("C-c s d" . helm-cscope-find-global-definition)
;;              ("C-c s =" . helm-cscope-find-assignments-to-this-symbol)
;;              ("C-c s c" . helm-cscope-find-calling-this-function)
;;              ("C-c s C" . helm-cscope-find-called-function)
;;              ("C-c s t" . helm-cscope-find-this-text-string)
;;              ("C-c s e" . helm-cscope-find-egrep-pattern)
;;              ("C-c s f" . helm-cscope-find-this-file)
;;              ("C-c s i" . helm-cscope-find-files-including-file)
;;              ("C-c s u" . helm-cscope-pop-mark))
;;   (setcar (cdr (assq 'helm-cscope-mode minor-mode-alist)) "Hc"))

;;;;;;;;
;; ag ;;
;;;;;;;;
(require 'grep)
(add-to-list 'grep-find-ignored-files "#*")
(require 'helm-ag)
(setq helm-ag-use-grep-ignore-list t)



(provide 'helm-extensions-config)
;;; helm-extensions-config.el ends here
