;;; elfeed-config.el --- Configure and improve elfeed

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'elfeed
;;   (require 'elfeed-config))
;; never:
;; (require 'elfeed-config)

;; Do not include in this file:
;; (require 'elfeed)

;;; Code:

(message "Importing elfeed-config")

(defface elfeed-search-audio-title-face
  '((((class color) (background light)) (:foreground "#f77"))
    (((class color) (background dark))  (:foreground "#f77")))
  "Face used in search mode for titles."
  :group 'elfeed)

(defface elfeed-search-image-title-face
  '((((class color) (background light)) (:foreground "#077"))
    (((class color) (background dark))  (:foreground "#077")))
  "Face used in search mode for titles."
  :group 'elfeed)

(push '(audio elfeed-search-audio-title-face) elfeed-search-face-alist)
(push '(image elfeed-search-image-title-face) elfeed-search-face-alist)

(setq elfeed-feeds
      '(("https://e00-expansion.uecdn.es/rss/portada.xml" expansion spanish paper)
        ("https://e00-elmundo.uecdn.es/elmundo/rss/portada.xml" elmundo spanish paper)
        ("https://www.abc.es/rss/feeds/abc_ultima.xml" abc spanish paper)
        ("http://ep00.epimg.net/rss/tags/ultimas_noticias.xml" elpais spanish paper)
        ("http://feeds.bbci.co.uk/news/rss.xml?edition=int" bbc english paper)
        ("https://podcasts.files.bbci.co.uk/p02pc9ny.rss" bbc english audio)
        ("https://www.theguardian.com/international/rss" theguardian english paper)
        ("https://www.theguardian.com/news/series/todayinfocus/podcast.xml" theguardian english audio)
        ("https://rss.nytimes.com/services/xml/rss/nyt/World.xml" nytimes world english paper)
        ("https://rss.nytimes.com/services/xml/rss/nyt/Europe.xml" nytimes europe english paper)
        ("https://www.sciencenews.org/feed" sciencenews english paper)
        ("https://xkcd.com/atom.xml" xkcd english image))
      elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory)
      elfeed-search-filter "@1-month-ago +unread")


(define-key elfeed-show-mode-map "R" #'language-text-to-speak-region)
(define-key elfeed-show-mode-map "S" #'language-text-to-speak-stop)


(provide 'elfeed-config)
;;; elfeed-config.el ends here
