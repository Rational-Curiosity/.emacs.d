;;; org-config.el --- Configure org and utils

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'org
;;   (require 'org-config))
;; or:
;; (with-eval-after-load 'org
;;   )
;; never:
;; (require 'org-config)

;; Do not include in this file:
;; (require 'org)

;;; Code:

(message "Importing org-config")
;; Corrector ortográfico
(add-hook 'org-mode-hook 'flyspell-mode)
(plist-put org-format-latex-options :scale 1.)

;;;;;;;;;;;
;; Faces ;;
;;;;;;;;;;;
(set-face-attribute 'org-scheduled-previously nil :foreground "rosy brown")
(set-face-attribute 'org-upcoming-deadline nil :foreground "orange")
(set-face-attribute 'org-warning nil :foreground "gold")
(set-face-attribute 'org-tag nil :bold nil)

(defface my-face-org-keystroke
  '((t (:inherit shadow
                 :box (:line-width -2 ;neg. in order to keep the same size of lines
                                   :color "grey75"
                                   :style pressed-button)))) "Face for keystrokes"
                                   :group 'org-faces)

(setq org-hide-emphasis-markers nil
      org-bullets-bullet-list
      '("α" "β" "γ" "δ" "ε" "ζ")
      org-priority-faces
      '((?A . "DodgerBlue1") (?B . "SteelBlue1") (?C . "LightSkyBlue1"))
      org-emphasis-alist
      '(("*" (bold :foreground "#AAF") bold)
        ("/" (italic :foreground "#FF8") italic)
        ("_" underline)
        ("=" org-verbatim verbatim)
        ("~" (org-code :box t) org-code)
        ("+" (:strike-through t)))
      ;; TODO keyword faces
      org-todo-keyword-faces
      '(("TODO" :foreground "orange red" :weight bold)
        ("NEXT" :foreground "gold" :weight bold)
        ("DONE" :foreground "forest green" :weight bold)
        ("STARTED" :foreground "deep sky blue" :weight bold)
        ("FINISHED" :foreground "dark olive green" :weight bold)
        ("ENOUGH" :foreground "green yellow" :weight bold)
        ("WAITING" :foreground "blue violet" :weight bold :underline t)
        ("HOLD" :foreground "dark violet" :weight bold :underline t)
        ("CANCELLED" :foreground "dark green" :weight bold))
      ;; TAG faces
      org-tag-faces
      '(("@business"     :foreground "#e5786d")
        ("@admin"        :foreground "#e68a00")
        ("@job"          :foreground "#996633")
        ("@improvement"  :foreground "#e6e600")
        ("@home"         :foreground "#95e454")
        ("business"     :foreground "#e5786d")
        ("admin"        :foreground "#e68a00")
        ("job"          :foreground "#996633")
        ("improvement"  :foreground "#e6e600")
        ("home"         :foreground "#95e454"))
      org-tag-alist
      '((:startgrouptag) ("business")    (:grouptags) ("@business")    (:endgrouptag)
        (:startgrouptag) ("admin")       (:grouptags) ("@admin")       (:endgrouptag)
        (:startgrouptag) ("job")         (:grouptags) ("@job")         (:endgrouptag)
        (:startgrouptag) ("improvement") (:grouptags) ("@improvement") (:endgrouptag)
        (:startgrouptag) ("home")        (:grouptags) ("@home")        (:endgrouptag)))
;;(:box t :foreground "#AAF")

(require 'language-tools)
(require 'org-bullets)
(add-hook 'org-mode-hook #'org-bullets-mode)

;;;;;;;;;;;;;
;; Modules ;;
;;;;;;;;;;;;;

(add-to-list 'org-modules 'org-habit)
(add-to-list 'org-modules 'org-mouse)

;;;;;;;;;;;;;
;; Exports ;;
;;;;;;;;;;;;;
(add-to-list 'org-export-backends 'taskjuggler)

;;;;;;;;;;;;;;;;;;;;;
;; Startup options ;;
;;;;;;;;;;;;;;;;;;;;;

(setq ;org-hide-block-startup t
      org-startup-folded 'content
      org-startup-with-inline-images t
      org-pretty-entities t
      org-use-property-inheritance t
      org-tags-column -80
      org-tags-sort-function #'string>
      org-ellipsis "▼"
      org-use-speed-commands
      (lambda () (and (looking-at org-outline-regexp)
                 (looking-back "^\**" (line-beginning-position)))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Babel block options ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-babel-sh-command "bash"
      org-babel-python-command "python3"
      ;; No pregunta al usuario antes de evaluar un bloque de código
      org-confirm-babel-evaluate nil
      org-src-fontify-natively t
      org-plantuml-jar-path (expand-file-name "~/.emacs.d/cache/java/plantuml.jar")
      org-babel-default-header-args:plantuml
      '((:results . "file")
        (:exports . "results")
        (:java . "-Dfile.encoding=UTF-8"))
      org-ditaa-jar-path (expand-file-name "~/.emacs.d/cache/java/ditaa.jar"))
;; active Babel languages
(org-babel-do-load-languages
  'org-babel-load-languages
  '((C          . t)
    (csharp     . t)
    (python     . t)
    (emacs-lisp . t)
    (java       . t)
    (go         . t)
    (dart       . t)
    (perl       . t)
    (awk        . t)
    (latex      . t)
    (maxima     . t)
    (gnuplot    . t)
    (calc       . t)
    (ditaa      . t)
    (shell      . t)
    (dot        . t)
    (plantuml   . t)))

(require 'ob)

(defvar org-babel-default-header-args:Python
  '((:session . "*python*")
    (:results . "output ")
    (:cache   . "yes")
    (:exports . "results")))
;; new Babel languages
(defun org-babel-execute:sh-async (body params)
  "Execute a block of shell code BODY asynchronously.  PARAMS ignored."
  (message "executing shell source code block asynchronously")
  (async-start
   `(lambda ()
      (shell-command ,body))
   (lambda (result)
     (message "%s" result))))
;; Activated by default
;; (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))
;; (add-to-list 'org-src-lang-modes '("conf" . conf))
(add-to-list 'org-src-lang-modes '("ini" . conf))
;;;;;;;;;;;;;;;;;;
;; TODO options ;;
;;;;;;;;;;;;;;;;;;
(require 'org-clock)
(setq org-time-clocksum-use-effort-durations t
      org-effort-durations '(("min" . 1)
                             ("h" . 60)
                             ("d" . 480)
                             ("w" . 2400)
                             ("m" . 9600)
                             ("y" . 96000))
      org-enforce-todo-dependencies t
      ;; [ 'note Graba el tiempo y una nota cuando se realiza una tarea
      org-log-done 'time ; 'time - solo guarda el tiempo
      ;; ]
      ;;org-clock-in-switch-to-state "STARTED"
      org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
        (sequence "STARTED(s!)" "|" "ENOUGH(e@/!)" "FINISHED(f!)")
        (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))
      org-archive-location "archived.org::* From %s"
      org-archive-file-header-format nil)

;;;;;;;;;;;;;;;;;;;;;
;; Convert options ;;
;;;;;;;;;;;;;;;;;;;;;
(require 'ox)
(setq org-html-validation-link nil)
;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert to Markdown ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
;;(require 'ox-md)
(require 'ox-gfm)  ; Include ox-md
(defun org-gfm-publish-to-gfm (plist filename pub-dir)
  (org-publish-org-to 'gfm filename ".md" plist pub-dir))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert to reStructuredText ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ox-rst)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert to LibreOffice ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ox-odt nil t)
(require 'ox-odt-bug)
(setq org-odt-fontify-srcblocks t
      org-odt-create-custom-styles-for-srcblocks t
      org-odt-table-styles
      (append org-odt-table-styles
              '(("TableWithBandingRows" "Custom"
                 ((use-banding-rows-styles . t)))
                ("TableWithBandingColumns" "Custom"
                 ((use-banding-column-styles . t)))
                ("TableWithHeaderRow" "Custom"
                 ((use-first-row-styles . t)))))
      ;; Convert to other formats
      org-odt-convert-processes
      '(("LibreOffice"
         "soffice --headless --convert-to %f%x --outdir %d %i")
        ("unoconv"
         "unoconv -f %f -o %d %i"))
      org-odt-convert-process (cond
                               ((executable-find "soffice") "LibreOffice")
                               ((executable-find "unoconv") "unoconv")
                               (t nil))
      org-odt-preferred-output-format (and org-odt-convert-process "doc"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert pytex environments ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Formatea el archivo antes del proceso de org-mode
;; para tener una salida adecuada.
(defun pytex-org-export-replacement (backend)
  "Límpia los códigos pytex para dejar sólo el latex."
  (org-map-entries
    (lambda ()
      (if (cl-find backend '(latex html)) (pytex-clean-environments)))))
(add-hook 'org-export-before-processing-hook 'pytex-org-export-replacement)

;;;;;;;;;;;;;;;;;;;;;;
;; Convert to latex ;;
;;;;;;;;;;;;;;;;;;;;;;

;; [ Formato latex por defecto
;; (org los coloca en orden inverso)
;; (add-to-list 'org-latex-packages-alist
;; '("" "color" nil))
;; (add-to-list 'org-latex-packages-alist
;; '("verbose,a4paper,headheight=5mm,footskip=7mm,left=19mm,right=16mm,top=37mm,bottom=23mm" "geometry" nil))
;; (add-to-list 'org-latex-packages-alist
;; '("" "multirow" nil))
;; (add-to-list 'org-latex-packages-alist
;; '("" "multicol" nil))
;; (add-to-list 'org-latex-packages-alist
;; '("AUTO" "babel" nil))
(require 'ox-latex)
(setq org-latex-image-default-width nil
      org-latex-packages-alist
'(("verbose,a4paper,headheight=5mm,footskip=7mm,left=19mm,right=16mm,top=37mm,bottom=23mm" "geometry" nil)
  ("AUTO" "babel" nil)
  ("" "color" nil)
  ("" "multirow" nil)
  ("" "multicol" nil)
  ("" "titlesec" nil)
  "
\\makeatletter
\\@ifpackageloaded{titlesec}{
\\definecolor{partcolor}{rgb}{0,0,0.1}
\\definecolor{sectcolor}{rgb}{0,0,0.3}
\\definecolor{ssctcolor}{rgb}{0,0,0.5}
\\definecolor{ssstcolor}{rgb}{0,0,0.7}
%\\renewcommand{\\headrulewidth}{0pt}
\\titleformat{\\part}[hang]{\\huge\\sc{\\color{blue}\\titlerule[1pt]\\vspace{2pt}\\titlerule[1pt]}}{\\llap{\\bf\\color{blue}\\thepart.}}{10pt}{\\color{partcolor}}[{\\color{blue}\\titlerule[1pt]\\vspace{2pt}\\titlerule[1pt]}]
\\titleformat{\\section}[hang]{\\Large\\sc}{\\llap{\\bf\\color{blue}\\thesection.}}{10pt}{\\color{sectcolor}}[{\\color{blue}\\nobreak\\titlerule[1pt]}]
\\titleformat{\\subsection}[hang]{\\large\\bf}{\\llap{\\color{blue}\\thesubsection.}}{7pt}{\\color{ssctcolor}\\underline}
\\titleformat{\\subsubsection}[hang]{\\normalsize\\it}{\\llap{\\bf\\color{blue}\\thesubsubsection.}}{4pt}{\\color{ssstcolor}}
}{\\relax}
\\makeatother
"))
;; ]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert to beamer presentation ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ox-beamer)

;;;;;;;;;;;;;;;;;;;;;
;; Convert to html ;;
;;;;;;;;;;;;;;;;;;;;;

;; Formato html por defecto
(require 'ox-html)
(setq  org-html-head
       "<style type=\"text/css\">
 <!--/*--><![CDATA[/*><!--*/
body{
	font-family: Titillium, Arial, sans-serif;
	background: #fff repeat top left;
	font-weight: 400;
	font-size: 14px;
	color:#505050;
	-webkit-font-smoothing: antialiased;
	-moz-font-smoothing: antialiased;
	font-smoothing: antialiased;
        margin: 10px;
        padding: 60px;
}
a{
	color: #0066FF;
	text-decoration: underline;
}
a:hover{
	color: #66A3FF;
	text-decoration: none;
}
math, math[mode=\"inline\"] {
  display: inline;
  font-family: sans-serif;
  font-style: normal;
font-size: 17px;
}
math[mode=\"display\"] {
  display: block;
  text-align: center;
  font-family: sans-serif;
  font-style: normal;
  font-size: 17px;
}

h1 {
	font-size: 40px;
	line-height: 40px;
	margin: 10px;
	position: relative;
	font-weight: 300;
	color: #0000A0;
	text-shadow: 1px 1px 1px rgba(255,255,255,0.7);
}
h2 {
	font-size: 27px;
	line-height: 27px;
	margin: 16px;
	position: relative;
	font-weight: 300;
	color: #0041C2;
	text-shadow: 1px 1px 1px rgba(255,255,255,0.7);
        text-indent: -20px;
        border-bottom: 2px solid blue;
}
h3 {
	font-size: 18px;
	line-height: 18px;
	margin: 22px;
	position: relative;
	font-weight: 300;
	color: #1569C7;
	text-shadow: 1px 1px 1px rgba(255,255,255,0.7);
        text-indent: -40px;
        text-decoration: underline;
        text-decoration-color: blue;
}
h4 {
	font-size: 17px;
	line-height: 17px;
	margin: 28px;
	position: relative;
	font-weight: 300;
	color: #659EC7;
	text-shadow: 1px 1px 1px rgba(255,255,255,0.7);
        text-indent: -60px;
        text-decoration: underline;
        text-decoration-color: blue;
}

p {
	font-size: 17px;
	line-height: 22px;
	text-align: justify;
	text-indent: 14px;
	margin: 3px 40px 5px 0;
	padding-left: 15px;
	/*font-family: Cambria, Georgia, serif;*/
}

p img {
	vertical-align: middle;
}
li img {
	vertical-align: middle;
}
img {
    max-width: 820px;
}

/* org-mode */

ol {
    list-style-type: decimal;
}
ol > li > ol {
    list-style-type: lower-latin;
}
ol > li > ol > li > ol {
    list-style-type: lower-roman;
}
ol p {
    border-left: none;
}

/*
ol p, ul p, div.figure p{
    border-left: none;
}*/

.todo   { font-family: monospace; color: red; }
.done   { color: green; }
.tag    { background-color: #eee; font-family: monospace;
          padding: 2px; font-size: 80%; font-weight: normal; }
.timestamp { color: #bebebe; }
.timestamp-kwd { color: #5f9ea0; }
.right  { margin-left: auto; margin-right: 0px;  text-align: right; }
.left   { margin-left: 0px;  margin-right: auto; text-align: left; }
.center { margin-left: auto; margin-right: auto; text-align: center; }
.underline { text-decoration: underline; }
#postamble p, #preamble p { font-size: 90%; margin: .2em; }

p.verse { margin-left: 3%; }
div.verbatim {
  border: 1px solid #ccc;
  box-shadow: 3px 3px 3px #eee;
  padding: 8pt;
  overflow: auto;
  margin: 1.2em;
}

samp {
  border: 1px solid #ccc;
  font-size: 14px;
  font-family: \"Lucida Console\", Monaco, monospace;
  background-color: #E8E8E8;
}
code {
  border: 1px solid #ccc;
  font-size: 14px;
  font-family: \"Lucida Console\", Monaco, monospace;
  background-color: #FFFFFF;
}
.org-src-container {
  overflow: auto;
  position: relative;
}
pre {
  border: 1px solid #ccc;
  box-shadow: 3px 3px 3px #eee;
  padding: 8pt;
  font-family: monospace;
  overflow: visible;
  margin: 1.2em;
}
pre.src {
  display: table;
  margin: 1em 0 0 0;
  position: initial;
  padding-top: 1em;
  color: #C0C0C0;
  Background-color: #202020;
}
pre.src:before {
  display: none;
  position: absolute;
  background-color: white;
  top: 0px;
  right: 10px;
  padding: 3px;
  border: 1px solid black;
}
pre.src:hover:before { display: inline;}
pre.src-sh:before    { content: 'sh'; }
pre.src-sh-async:before    { content: 'sh'; }
pre.src-bash:before  { content: 'bash'; }
pre.src-emacs-lisp:before { content: 'Emacs Lisp'; }
pre.src-c:before   { content: 'C'; }
pre.src-cpp:before   { content: 'C++'; }
pre.src-R:before     { content: 'R'; }
pre.src-perl:before  { content: 'Perl'; }
pre.src-java:before  { content: 'Java'; }
pre.src-sql:before   { content: 'SQL'; }

table { border-collapse:collapse; }
caption.t-above { caption-side: top; }
caption.t-bottom { caption-side: bottom; }
td, th { vertical-align:top;  }
th.right  { text-align: center;  }
th.left   { text-align: center;   }
th.center { text-align: center; }
td.right  { text-align: right;  }
td.left   { text-align: left;   }
td.center { text-align: center; }
dt { font-weight: bold; }
.footpara:nth-child(2) { display: inline; }
.footpara { display: block; }
.footdef  { margin-bottom: 1em; }
.figure { padding: 1em; }
.figure p { text-align: center; }
.inlinetask {
  padding: 10px;
  border: 2px solid gray;
  margin: 10px;
  background: #ffffcc;
}
#org-div-home-and-up
 { text-align: right; font-size: 70%; white-space: nowrap; }
textarea { overflow-x: auto; }
.linenr { font-size: smaller }
.code-highlighted { background-color: #ffff00; }
.org-info-js_info-navigation { border-style: none; }
#org-info-js_console-label
  { font-size: 10px; font-weight: bold; white-space: nowrap; }
.org-info-js_search-highlight
  { background-color: #ffff00; color: #000000; font-weight: bold; }
/* modificado org-mode */
#text-table-of-contents ul
  { list-style-type: none; }
span.section-number-2 {
  font-size: 30px;
  color: #003399;
}
span.section-number-3 {
  font-size: 20px;
  color: #003399;
}
span.section-number-4 {
  font-size: 18px;
  color: #003399;
}

blockquote {
  border: 1px solid #ccc;
  box-shadow: 3px 3px 3px #eee;
  padding: 8pt;
  overflow: auto;
  margin: 1.2em;
}
  /*]]>*/-->
</style>"
       ;; [ No se debería modificar
       ;; org-html-style-default
       ;; "<link rel=\"stylesheet\" type=\"text/css\" href=\"css/style.css\" />"
       )
(defcustom org-html-mathjax-options
  '((path  "js/MathJax.js")
    (scale "100")
    (align "center")
    (indent "2em")
    (mathml nil))
  "Options for MathJax setup.

path        The path where to find MathJax
scale       Scaling for the HTML-CSS backend, usually between 100 and 133
align       How to align display math: left, center, or right
indent      If align is not center, how far from the left/right side?
mathml      Should a MathML player be used if available?
            This is faster and reduces bandwidth use, but currently
            sometimes has lower spacing quality.  Therefore, the default is
            nil.  When browsers get better, this switch can be flipped.

You can also customize this for each buffer, using something like

#+MATHJAX: scale:\"133\" align:\"right\" mathml:t path:\"/MathJax/\""
  :group 'org-export-html
  :type '(list :greedy t
               (list :tag "path   (the path from where to load MathJax.js)"
                     (const :format "       " path) (string))
               (list :tag "scale  (scaling for the displayed math)"
                     (const :format "       " scale) (string))
               (list :tag "align  (alignment of displayed equations)"
                     (const :format "       " align) (string))
               (list :tag "indent (indentation with left or right alignment)"
                     (const :format "       " indent) (string))
               (list :tag "mathml (should MathML display be used is possible)"
                     (const :format "       " mathml) (boolean))))
;; Definen =verbatim= y ~code~ en html
(eval-after-load 'ox-html
  '(progn
    (push '(verbatim . "<samp>%s</samp>") org-html-text-markup-alist)
    (push '(code . "<code>%s</code>") org-html-text-markup-alist)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert to html with Twitter Bootstrap ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ox-twbs)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert taskjuggler ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ox-taskjuggler)

;;;;;;;;;;;;;
;; Publish ;;
;;;;;;;;;;;;;
(require 'ox-publish)
(bound-and-eval 'config-03)

;;;;;;;;;;;
;; Links ;;
;;;;;;;;;;;

(setq org-return-follows-link t)
(delete '("\\.pdf\\'" . default) org-file-apps)
(add-to-list 'org-file-apps '("\\.pdf::\\([0-9]+\\)\\'" . "evince \"%s\" -p %1"))
(add-to-list 'org-file-apps '("\\.png\\'" . "eog \"%s\""))

;;;;;;;;;;;;
;; Agenda ;;
;;;;;;;;;;;;

;; (require 'calfw-org)
;; (defun cfw:open-org-calendar-command (&args)
;;   (interactive)
;; ;;  (let ((org-agenda-skip-function 'org-agenda-skip-work))
;;   (cfw:open-org-calendar))
;; ;;)
(require 'org-agenda-property)

(setq org-directory (loop for folder in
                          `("~/var/Dropbox/Orgzly"
                            ,(concat "~/Prog/org/" (getenv "JOB_FOLDER"))
                            "~/Prog/org"
                            "~")
                          when (file-exists-p folder)
                          return folder)
      org-default-notes-file (concat org-directory "/.notes.org")
      org-agenda-files `(,org-directory)
      ;; <property>
      org-agenda-property-list '("REQUIRED")
      ;; <Calendar>
      calendar-week-start-day 1
      calendar-day-name-array     ["domingo" "lunes" "martes"
                                   "miércoles" "jueves" "viernes" "sábado"]
      calendar-day-abbrev-array   ["dom" "lun" "mar" "míe" "jue" "vie" "sáb"]
      calendar-day-header-array   ["D" "L" "M" "X" "J" "V" "S"]
      calendar-month-name-array   ["Enero" "Febrero" "Marzo" "Abril" "Mayo"
                                   "Junio" "Julio" "Agosto" "Septiembre"
                                   "Octubre" "Noviembre" "Diciembre"]
      calendar-month-abbrev-array ["Ene" "Feb" "Mar" "Abr" "May" "Jun"
                                   "Jul" "Ago" "Sep" "Oct" "Nov" "Dic"]
      ;; <Agenda includes>
      org-agenda-skip-deadline-prewarning-if-scheduled t
      org-agenda-skip-scheduled-if-deadline-is-shown t
      org-agenda-include-deadlines t
      ;; org-agenda-include-diary t
      ;; org-agenda-include-inactive-timestamps t
      ;; <Agenda format>
      org-agenda-time-grid
      '((daily today require-timed remove-match)
        (800 1000 1200 1400 1600 1800 2000)
        "......" "----------------")
      org-agenda-prefix-format
      '((agenda . " %i %-4.4 c%?-12t% s") ; (agenda . " %i %-12:c%?-12t% s")
        (timeline . "  % s")
        (todo . " %i %-4.4 c%?-12t% s") ; (todo . " %i %-12:c")
        (tags . " %i %-12:c")
        (search . " %i %-12:c"))
      org-agenda-scheduled-leaders '("Sche" "S-%3dd")
      org-agenda-deadline-leaders  '("Dead" "D+%3dd" "D-%3dd")
      org-agenda-sorting-strategy
      '((agenda habit-down time-up todo-state-up priority-down category-keep)
        (todo priority-down category-keep)
        (tags priority-down category-keep)
        (search category-keep))
      org-agenda-deadline-faces
      '((1.01 . '(:foreground "magenta" :bold t))
        (1.0 . org-warning)
        (0.5 . org-upcoming-deadline)
        (0.0 . default))
      org-deadline-warning-days 15
      org-agenda-start-on-weekday 1
      org-habit-show-habits-only-for-today t
      ;; <Agenda commands>
      org-agenda-custom-commands
      '(("h" "[h]abits" 
         ((agenda ""))
         ((org-agenda-show-log t)
          (org-agenda-ndays 7)
          (org-agenda-log-mode-items '(state))
          (org-agenda-skip-function
           '(org-agenda-skip-entry-if 'notregexp ":STYLE: *habit"))))
        ("c" "[c]alendar" cfw:open-org-calendar-command)
        ("u" "[u]nscheduled tasks"
         ((alltodo ""
           ((org-agenda-skip-function
           '(org-agenda-skip-entry-if
             'scheduled 'deadline
             ;;'todo '("DONE" "CANCELLED" "FINISHED" "ENOUGH")
             ))))))
        ("A" "[A]ll scheduled or not"
         ((agenda "")
          (alltodo ""
                   ((org-agenda-skip-function
                     '(org-agenda-skip-entry-if
                       'scheduled 'deadline
                       ;;'todo '("DONE" "CANCELLED" "FINISHED" "ENOUGH")
                       ))))))
        ;;("u" "[u]nscheduled tasks" tags "-SCHEDULED={.+}/!+TODO|+NEXT|+STARTED|+WAITING|+HOLD")
        ))
;;;;;;;;;;;;;;;
;; Functions ;;
;;;;;;;;;;;;;;;
(defun org-sort-entries-by-tags-then-todo ()
  (interactive)
  (let ((order '(("TODO"      . "a")
                 ("NEXT"      . "b")
                 ("DONE"      . "z")
                 ("STARTED"   . "c")
                 ("FINISHED"  . "y")
                 ("ENOUGH"    . "o")
                 ("WAITING"   . "h")
                 ("HOLD"      . "i")
                 ("CANCELLED" . "x"))))
    (org-sort-entries nil ?f
                      (lambda ()
                        (let ((pos (point)))
                          (concat
                           (or (cdr (assoc (org-entry-get pos "TODO") order)) " ")
                           (mapconcat
                            'identity
                            (sort
                             (split-string
                              (or (org-entry-get pos "TAGS") "") ":" t)
                             'string<)
                            " ")))))))

(require 'config-lib)
(defun org-af (&optional prefix suffix)
  "Return filename counting blocks using this function.
Filename returned has the format:

PREFIX block-number SUFFIX

Example: doc-000012.png

PREFIX - default buffer name without extension
SUFFIX - default .png"
  (let ((number 0)
        (pre (or prefix (file-name-sans-extension (buffer-name))))
        (suf (or suffix ".png")))
    (save-excursion
      (forward-line 1)
      (while (re-search-forward (format "#\\+BEGIN_SRC.*%s" (compile-time-function-name)) nil t)
        (cl-incf number)))
    (concat pre (format "-%06d" number) suf)))

(defun org-archive-done-tasks (&optional scope)
  (interactive "P")
  (cond
   ((equal scope '(16))
    (setq scope 'agenda))
   ((equal scope '(4))
    (setq scope 'tree))
   ((equal scope nil)
    (setq scope 'file)))
  (org-map-entries
   (lambda ()
     (org-archive-subtree)
     (setq org-map-continue-from (outline-previous-heading)))
   "/DONE|ENOUGH|FINISH|CANCELLED" scope))

(defun org-read-entry-property-name ()
  "Read a property name from the current entry."
  (let ((completion-ignore-case t)
        (default-prop (or (and (org-at-property-p)
                               (match-string-no-properties 2))
                          org-last-set-property)))
    (org-completing-read
     (format "Property [%s]: " (if default-prop default-prop ""))
     (org-entry-properties nil nil)
     nil nil nil nil default-prop)))

(defun org-region-to-property (&optional property)
  "Copies the region as value to an Org-mode PROPERTY."
  (interactive)
  ;; if no region is defined, do nothing
  (if (use-region-p)
      ;; if a region string is found, ask for a property and set property to
      ;; the string in the region
      (let ((val (replace-regexp-in-string
                  "\\`[ \t\n]*" ""
                  (replace-regexp-in-string "[ \t\n]*\\'" ""
                                            (substring (buffer-string)
                                                       (- (region-beginning) 1)
                                                       (region-end))))
                 )
            ;; if none was stated by user, read property from user
            (prop (or property
                      (org-read-entry-property-name))))
        ;; set property
        (org-set-property prop val))))


(require 'org-table)
(defun org-table-sort-column ()
  "Sort table column at point."
  (interactive)
  (let* ((thisline (org-current-line))
         (thiscol (org-table-current-column))
         (otc org-table-overlay-coordinates)
         beg end bcol ecol tend tbeg column lns pos)
    (when (equal thiscol 0)
      (if (called-interactively-p 'any)
          (setq thiscol
                (string-to-number
                 (read-string "Use column N for sorting: ")))
        (setq thiscol 1))
      (org-table-goto-column thiscol))
    (org-table-check-inside-data-field)
    (if (org-region-active-p)
        (progn
          (setq beg (region-beginning) end (region-end))
          (goto-char beg)
          (setq column (org-table-current-column)
                beg (point-at-bol))
          (goto-char end)
          (setq end (point-at-bol 2)))
      (setq column (org-table-current-column)
            pos (point)
            tbeg (org-table-begin)
            tend (org-table-end))
      (if (re-search-backward org-table-hline-regexp tbeg t)
          (setq beg (point-at-bol 2))
        (goto-char tbeg)
        (setq beg (point-at-bol 1)))
      (goto-char pos)
      (if (re-search-forward org-table-hline-regexp tend t)
          (setq end (point-at-bol 1))
        (goto-char tend)
        (setq end (point-at-bol))))
    (setq beg (move-marker (make-marker) beg)
          end (move-marker (make-marker) end))
    (untabify beg end)
    (goto-char beg)
    (org-table-goto-column column)
    (skip-chars-backward "^|")
    (setq bcol (point))
    (goto-char end)
    (forward-line -1)
    (org-table-goto-column column)
    (skip-chars-forward "^|")
    (setq ecol (point))
    (org-table-copy-region bcol ecol nil)
    (setq lns (mapcar (lambda (x) (cons 
                                   (org-sort-remove-invisible 
                                    (substring-no-properties x) )
                                   x))
                      ( mapcar 'car org-table-clip)))
    (setq lns (org-do-sort lns "Column" ))
    (setq org-table-clip (mapcar 'list (mapcar 'cdr lns)))
    (goto-char beg)
    (org-table-goto-column column)
    (org-table-paste-rectangle)
    (org-goto-line thisline)
    (org-table-goto-column thiscol)
    (when otc (org-table-toggle-coordinate-overlays))
    (message "%d element sorted in column %d" (length lns) column)))

(defun org-auto-redisplay-inline-images ()
  (when org-inline-image-overlays
    (org-redisplay-inline-images)))

(add-hook 'org-babel-after-execute-hook 'org-auto-redisplay-inline-images)

(defun copy-buffer-file-name-org-link ()
  (interactive)
  (let* ((path-name (abbreviate-file-name buffer-file-name))
         (name (file-name-nondirectory path-name)))
    (kill-new (concat "[[" path-name "][" name "]]"))))

(defun copy-buffer-file-name-and-line-org-link ()
  (interactive)
  (let* ((path-name (abbreviate-file-name buffer-file-name))
         (name (file-name-nondirectory path-name)))
    (kill-new (concat "[[" path-name "::" (line-number-at-pos) "][" name "]]"))))

(defun org-todo-prompt-date (&optional arg)
  (interactive "P")
  (cl-letf* ((org-read-date-prefer-future nil)
             (my-current-time (org-read-date t t nil "when:" nil nil nil))
             ((symbol-function #'org-current-effective-time)
              #'(lambda () my-current-time)))
    (org-todo arg)))

(defun org-block-and-result-hide-all ()
  (interactive)
  (call-interactively #'org-hide-block-all)
  (call-interactively #'org-babel-result-hide-all))

(defun org-block-and-result-show-all ()
  (interactive)
  (call-interactively #'org-show-block-all)
  (call-interactively #'org-babel-show-result-all))
;;;;;;;;;;
;; Keys ;;
;;;;;;;;;;

(bind-keys :map org-mode-map
           ("C-c v e"   . org-show-entry)
           ("C-c TAB"   . org-show-subtree)
           ("C-c v t"   . org-show-todo-tree)
           ("C-c v s"   . org-block-and-result-show-all) 
           ("C-c v h"   . org-block-and-result-hide-all) 
           ("C-c s"     . org-sort-entries-by-tags-then-todo)
           ("C-c c"     . org-capture)
           ("C-c a"     . org-agenda)
           ("C-c C-l"   . org-store-link)
           ("C-c L"     . org-insert-link-global)
           ("C-c O"     . org-open-at-point-global)
           ("C-c p"     . org-publish)
           ("C-c C-x D" . org-archive-done-tasks))


(provide 'org-config)
;;; org-config.el ends here
