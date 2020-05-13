;;; eww-config.el --- Configure eww

;;; Commentary:

;; Usage:
;; (require 'eww-config)

;; Default eww key bindings
;; |-----------+----------------------------------------------------------------------|
;; | Key       | Function                                                             |
;; |-----------+----------------------------------------------------------------------|
;; | &         | Browse the current URL with an external browser.                     |
;; | -         | Begin a negative numeric argument for the next command.              |
;; | 0 .. 9    | Part of the numeric argument for the next command.                   |
;; | C         | Display a buffer listing the current URL cookies, if there are any.  |
;; | H         | List the eww-histories.                                              |
;; | F         | Toggle font between variable-width and fixed-width.                  |
;; | G         | Go to a URL                                                          |
;; | R         | Readable mode                                                        |
;; | S         | List eww buffers                                                     |
;; | d         | Download URL under point to `eww-download-directory'.                |
;; | g         | Reload the current page.                                             |
;; | q         | Quit WINDOW and bury its buffer.                                     |
;; | v         | `eww-view-source'                                                    |
;; | w         | `eww-copy-page-url'                                                  |
;; |-----------+----------------------------------------------------------------------|
;; | b         | Add the current page to the bookmarks.                               |
;; | B         | Display the bookmark list.                                           |
;; | M-n       | Visit the next bookmark                                              |
;; | M-p       | Visit the previous bookmark                                          |
;; |-----------+----------------------------------------------------------------------|
;; | t         | Go to the page marked `top'.                                         |
;; | u         | Go to the page marked `up'.                                          |
;; |-----------+----------------------------------------------------------------------|
;; | n         | Go to the page marked `next'.                                        |
;; | p         | Go to the page marked `previous'.                                    |
;; |-----------+----------------------------------------------------------------------|
;; | l         | Go to the previously displayed page.                                 |
;; | r         | Go to the next displayed page.                                       |
;; |-----------+----------------------------------------------------------------------|
;; | TAB       | Move point to next link on the page.                                 |
;; | S-TAB     | Move point to previous link on the page.                             |
;; |-----------+----------------------------------------------------------------------|
;; | SPC       | Scroll up                                                            |
;; | DEL/Bkspc | Scroll down                                                          |
;; | S-SPC     | Scroll down                                                          |
;; |-----------+----------------------------------------------------------------------|

;;; Code:


(require 'eww)

(setq eww-search-prefix "https://www.google.com/search?q="
      eww-download-directory "~/Descargas")

(defun browse-url-browser-function-selection ()
  (interactive)
  (setq browse-url-browser-function
        (intern
         (completing-read
          "Select function to open urls: "
          (let (browsers)
            (if (locate-library "w3") 'browse-url-w3)
            (if (executable-find browse-url-xterm-program) (push 'browse-url-text-xterm browsers))
            (if (executable-find browse-url-kde-program) (push 'browse-url-kde browsers))
            (if (executable-find browse-url-conkeror-program) (push 'browse-url-conkeror browsers))
            (if (executable-find browse-url-chrome-program) (push 'browse-url-chrome browsers))
            (if (executable-find browse-url-chromium-program) (push 'browse-url-chromium browsers))
            (if (executable-find browse-url-firefox-program) (push 'browse-url-firefox browsers))
            (if (executable-find browse-url-mozilla-program) (push 'browse-url-mozilla browsers))
            (push 'eww-browse-url browsers)
            (push 'browse-url-default-browser browsers)
            browsers)
          nil t nil nil browse-url-browser-function))))

(global-set-key (kbd "C-x W") 'browse-url-browser-function-selection)
;; Make the binding for `revert-buffer' do `eww-reload' in eww-mode
(define-key eww-mode-map (kbd "<backtab>") 'shr-previous-link)
(define-key eww-mode-map (kbd ":") 'eww)
(define-key eww-mode-map (kbd "h") 'eww-list-histories)
(define-key eww-mode-map (kbd "w") 'modi/eww-copy-url-dwim)
(define-key eww-mode-map (kbd "/") 'hlt-highlight-symbol)
(define-key eww-mode-map (kbd "k") 'modi/eww-keep-lines)
(define-key eww-mode-map [remap revert-buffer] 'eww-reload)
(define-key eww-checkbox-map (kbd "<down-mouse-1>") 'eww-toggle-checkbox)


(provide 'eww-config)
;;; eww-config.el ends here
