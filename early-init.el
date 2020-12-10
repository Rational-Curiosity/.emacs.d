(defun save-messages-buffer ()
  (with-current-buffer "*Messages*"
    (write-region (point-min) (point-max)
                  (expand-file-name "last-messages-buffer.txt"
                                    user-emacs-directory))))
(add-hook 'kill-emacs-hook 'save-messages-buffer 91)

;; [ garbage collect
(add-hook 'emacs-startup-hook
          `(lambda ()
             (setq gc-cons-percentage ,gc-cons-percentage
                   gc-cons-threshold ,gc-cons-threshold)
             (require 'gcmh)
             (setq minor-mode-alist
                   (cl-delete 'gcmh-mode minor-mode-alist :key 'car))
             (setq gcmh-idle-delay 20
                   gcmh-verbose t
                   gcmh-low-cons-threshold ,gc-cons-threshold
                   gcmh-high-cons-threshold (eval-when-compile
                                              (* 10 1024 1024)))
             (gcmh-mode 1))
          t)
(setq gc-cons-percentage 0.6
      gc-cons-threshold (eval-when-compile
                          (* 10 1024 1024)))
(defun gcmh-idle-garbage-collect-advice (orig-fun)
  (unless (or ;; cursor-in-echo-area
           prefix-arg
           (< 0 (length (this-single-command-keys)))
           (active-minibuffer-window))
    (funcall orig-fun)))
(advice-add 'gcmh-idle-garbage-collect :around 'gcmh-idle-garbage-collect-advice)
;; ]
;; [ file name handlers
(add-hook 'after-init-hook
          `(lambda ()
             (setq file-name-handler-alist (quote ,file-name-handler-alist))))
(setq file-name-handler-alist nil)
;; ]
;; [ long lines
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)
(global-so-long-mode 1)
;; ]

(when (getenv "BENCHMARK")
  (setq benchmark-last-time (current-time)
        benchmark-last-feature "early-init"
        benchmark-buffer (generate-new-buffer "*Benchmarks*"))
  (require 'time-date)
  (defun benchmark-require-advice (orig-fun feature &optional filename noerror)
    (let* ((time (float-time (time-since benchmark-last-time)))
           (initial-time (current-time))
           (result (funcall orig-fun feature filename noerror))
           (require-time (float-time (time-since initial-time))))
      (with-current-buffer benchmark-buffer
        (insert (format "%fs  between `%s' and `%s'\n%fs  loading `%s'\n"
                        time benchmark-last-feature feature
                        require-time feature)))
      (setq benchmark-last-time (current-time)
            benchmark-last-feature feature)
      result))
  (advice-add 'require :around 'benchmark-require-advice)
  (add-hook 'emacs-startup-hook
            (lambda ()
              (advice-remove 'require 'benchmark-require-advice))))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; (fringe-mode '(4 . 4))
(setq-default indicate-buffer-boundaries 'right)
(defface visual-line-fringe-face
  '((t :foreground "gold1"))
  "Visual line fringe face" :group 'visual-line)
(set-fringe-bitmap-face 'left-curly-arrow 'visual-line-fringe-face)
(set-fringe-bitmap-face 'right-curly-arrow 'visual-line-fringe-face)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
