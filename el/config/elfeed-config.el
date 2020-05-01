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

(defface elfeed-search-science-title-face
  '((((class color) (background light)) (:foreground "#fd0"))  ;; gold
    (((class color) (background dark))  (:foreground "#fd0")))
  "Face used in search mode for titles."
  :group 'elfeed)

(defface elfeed-search-arxiv-title-face
  '((((class color) (background light)) (:foreground "#ad3" :underline t))  ;; yellow green
    (((class color) (background dark))  (:foreground "#ad3" :underline t)))
  "Face used in search mode for titles."
  :group 'elfeed)

(defface elfeed-search-health-title-face
  '((((class color) (background light)) (:foreground "#fcd"))  ;; pink
    (((class color) (background dark))  (:foreground "#fcd")))
  "Face used in search mode for titles."
  :group 'elfeed)

(defface elfeed-search-audio-title-face
  '((t  (:background "#3d3")))  ;; lime green
  "Face used in search mode for titles."
  :group 'elfeed)

(defface elfeed-search-image-title-face
  '((t  (:background "#ad3")))  ;; yellow green
  "Face used in search mode for titles."
  :group 'elfeed)

(defface elfeed-search-video-title-face
  '((t  (:background "#a33")))  ;; brown
  "Face used in search mode for titles."
  :group 'elfeed)

(push '(aud elfeed-search-audio-title-face) elfeed-search-face-alist)
(push '(img elfeed-search-image-title-face) elfeed-search-face-alist)
(push '(vid elfeed-search-video-title-face) elfeed-search-face-alist)
(push '(science elfeed-search-science-title-face) elfeed-search-face-alist)
(push '(arxiv elfeed-search-arxiv-title-face) elfeed-search-face-alist)
(push '(health elfeed-search-health-title-face) elfeed-search-face-alist)

(defun elfeed-youtube-expand (id)
  (format
   (pcase (substring id 0 2)
     ("UC" "https://www.youtube.com/feeds/videos.xml?channel_id=%s")
     ("PL" "https://www.youtube.com/feeds/videos.xml?playlist_id=%s")
     (_    "https://www.youtube.com/feeds/videos.xml?user=%s"))
   id))

(setq elfeed-feeds
      `(("https://e00-expansion.uecdn.es/rss/portada.xml" expansion es txt)
        ("http://estaticos.elmundo.es/elmundo/rss/espana.xml" elmundo spain es txt)
        ("http://estaticos.elmundo.es/elmundo/rss/internacional.xml" elmundo world es txt)
        ("http://www.abc.es/rss/feeds/abc_EspanaEspana.xml" abc spain es txt)
        ("http://www.abc.es/rss/feeds/abc_Internacional.xml" abc world es txt)
        ("https://feeds.elpais.com/mrss-s/pages/ep/site/elpais.com/portada" elpais spain es txt)
        ("https://feeds.elpais.com/mrss-s/pages/ep/site/elpais.com/section/internacional/portada" elpais world es txt)
        ;; english
        ("http://feeds.bbci.co.uk/news/rss.xml?edition=int" bbc world en txt)
        ("https://www.theguardian.com/international/rss" theguardian world en txt)
        ("https://rss.nytimes.com/services/xml/rss/nyt/World.xml" nytimes world en txt)
        ;; science
        ("http://rss.sciam.com/ScientificAmerican-Global" SA science en txt)
        ("https://www.sciencenews.org/feed" sciencenews science en txt)
        ;; arxiv papers
        ,@(mapcar (lambda (category)
                    (list (concat "http://arxiv.org/rss/" (symbol-name category))
                          'arxiv category 'en 'txt))
                  '(astro-ph cond-mat cs econ eess gr-qc hep-ex hep-lat
                             hep-ph hep-th math math-ph nlin nucl-ex nucl-th
                             physics q-bio q-fin quant-ph stat))
        ;; health
        ("https://www.who.int/rss-feeds/news-english.xml" oms health es txt)
        ;; podcast
        ("https://podcasts.files.bbci.co.uk/p02pc9ny.rss" bbc en aud)
        ("https://www.theguardian.com/news/series/todayinfocus/podcast.xml" theguardian en aud)
        ;; images
        ("https://xkcd.com/atom.xml" xkcd en img)
        ;; video
        (,(elfeed-youtube-expand "UCHnyfMqiRRG1u-2MsSQLbXA") veritasium science en vid)
        (,(elfeed-youtube-expand "UCW3iqZr2cQFYKdO9Kpa97Yw") utbh es vid))
      elfeed-db-directory (expand-file-name "elfeed" user-emacs-directory)
      elfeed-search-filter "@1-month-ago +unread"
      elfeed-search-date-format '("%Y-%m-%d %H:%M" 16 :left)
      elfeed-search-trailing-width 30
      elfeed-search-title-min-width 20
      elfeed-search-title-max-width 100)

(setq elfeed-search--tags
      (let ((tags #s(hash-table size 30 test eq data (unread t))))
        (dolist (item elfeed-feeds)
          (if (listp item)
              (mapc (lambda (tag) (puthash tag t tags))
                    (cdr item))))
        (mapcar 'symbol-name
                (hash-table-keys tags))))

(defun elfeed-search-filter-tags-selection (arg)
  (interactive "P")
  (let ((tag (completing-read "Select tag: " elfeed-search--tags nil t)))
    (elfeed-search-set-filter
     (if (string-match (concat "\\( ?\\)\\([-+]\\)" tag) elfeed-search-filter)
         (replace-match (if arg
                            (concat
                             (match-string 1 elfeed-search-filter)
                             (if (string-equal "+" (match-string
                                                    2 elfeed-search-filter))
                                 "-"
                               "+")
                             tag)
                          "")
                        t t elfeed-search-filter)
       (concat elfeed-search-filter " " (if arg "-" "+") tag)))))

(define-key elfeed-show-mode-map "h" nil)
(define-key elfeed-show-mode-map "?" #'describe-mode)
(define-key elfeed-show-mode-map "R" #'language-text-to-speak-region)
(define-key elfeed-show-mode-map "S" #'language-text-to-speak-stop)

(define-key elfeed-search-mode-map "h" nil)
(define-key elfeed-search-mode-map "?" #'describe-mode)
(define-key elfeed-search-mode-map "t" 'elfeed-search-filter-tags-selection)


(provide 'elfeed-config)
;;; elfeed-config.el ends here
