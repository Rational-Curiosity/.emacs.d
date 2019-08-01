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
;; (plist-put org-format-latex-options :scale 1.) ;; default

;;;;;;;;;;;
;; Faces ;;
;;;;;;;;;;;
(set-face-attribute 'org-scheduled-previously nil :foreground "rosy brown")
(set-face-attribute 'org-upcoming-deadline nil :foreground "orange")
(set-face-attribute 'org-warning nil :foreground "gold")
(set-face-attribute 'org-tag nil :bold nil)
(set-face-attribute 'org-level-1 nil :bold t)
(set-face-attribute 'org-level-2 nil :bold t :foreground "light sea green")

(set-face-attribute 'org-block-begin-line nil
                    :underline "#A7A6AA")  ;; :foreground "#388EFF" :background "#222210")

;; (set-face-attribute 'org-block nil
;;                     :background "#292929")

(set-face-attribute 'org-block-end-line nil
                    :underline nil
                    :overline "#A7A6AA") ;; :foreground "#388EFF" :background "#222210")

(defface my-face-org-keystroke
  '((t (:inherit shadow
                 :box (:line-width -2 ;neg. in order to keep the same size of lines
                                   :color "grey75"
                                   :style pressed-button)))) "Face for keystrokes"
                                   :group 'org-faces)


(org-link-set-parameters
 "file"
 :face (lambda (path) (if (file-exists-p path) 'org-link 'org-warning)))

(setq org-hide-emphasis-markers nil
      org-bullets-bullet-list
      '("α" "β" "γ" "δ" "ε" "ζ")
      org-priority-faces
      '((?A . "#C39BD3")
        (?B . "#AF7AC5")
        (?C . "#9B59B6")
        (?D . "#884EA0")
        (?E . "#76448A")

        (?F . "#85C1E9")
        (?G . "#5DADE2")
        (?H . "#3498DB")
        (?I . "#2E86C1")
        (?J . "#2874A6")

        (?K . "#82E0AA")
        (?L . "#58D68D")
        (?M . "#2ECC71")
        (?N . "#28B463")
        (?O . "#239B56"))
      org-emphasis-alist
      '(("*" (bold :foreground "#AAF") bold)
        ("/" (italic :foreground "#FF8") italic)
        ("_" underline)
        ("=" org-verbatim verbatim)
        ("~" (org-code :box t) org-code)
        ("+" (:strike-through t)))
      ;; TODO keyword faces
      org-todo-keyword-faces
      '(("TODO" :foreground "orange red" :weight bold) ;; TODO
        ("NEXT" :foreground "gold" :weight bold) ;; NEXT
        ("DONE" :foreground "forest green" :weight bold) ;; DONE
        ("PLAN" :foreground "deep sky blue" :weight bold)
        ("STAR" :foreground "sky blue" :weight bold) ;; STARTED
        ("REOP" :foreground "indian red" :weight bold) ;; REOPENED
        ("FINI" :foreground "dark olive green" :weight bold) ;; FINISHED
        ("ENOU" :foreground "green yellow" :weight bold) ;; ENOUGH
        ("DELE" :foreground "light green" :weight bold) ;; DELEGATED
        ("LINK" :foreground "violet" :weight bold) ;; LINKED
        ("WAIT" :foreground "blue violet" :weight bold :underline t) ;; WAITING
        ("HOLD" :foreground "dark violet" :weight bold :underline t) ;; HOLD
        ("CANC" :foreground "dark green" :weight bold) ;; CANCELED
        ("FIXM" :foreground "dark red" :weight bold) ;; FIXME
        ("VERI" :foreground "dodger blue" :weight bold) ;; VERIFY
        ("UNDO" :foreground "royal blue" :weight bold)) ;; UNDO
      ;; TAG faces
      org-tag-faces
      '(("@business"    :foreground "deep sky blue")
        ("@admin"       :foreground "blue")
        ("@job"         :foreground "pink")
        ("@improvement" :foreground "green")
        ("@research"    :foreground "yellow")
        ("@language"    :foreground "orange")
        ("@sport"       :foreground "purple")
        ("@others"      :foreground "red")
        ("business"     :foreground "#e5786d")
        ("admin"        :foreground "#e68a00")
        ("job"          :foreground "#996633")
        ("improvement"  :foreground "#e6e600")
        ("research"     :foreground "#e5f442")
        ("language"     :foreground "#e69900")
        ("sport"        :foreground "#ff471a")
        ("others"       :foreground "#ff0000"))
      org-tag-alist
      '((:startgrouptag) ("business")    (:grouptags) ("@business")    (:endgrouptag)
        (:startgrouptag) ("admin")       (:grouptags) ("@admin")       (:endgrouptag)
        (:startgrouptag) ("job")         (:grouptags) ("@job")         (:endgrouptag)
        (:startgrouptag) ("improvement") (:grouptags) ("@improvement") (:endgrouptag)
        (:startgrouptag) ("health")      (:grouptags) ("@health")      (:endgrouptag)
        (:startgrouptag) ("home")        (:grouptags) ("@home")        (:endgrouptag)
        (:startgrouptag) ("research")    (:grouptags) ("@research")    (:endgrouptag)
        (:startgrouptag) ("language")    (:grouptags) ("@language")    (:endgrouptag)
        (:startgrouptag) ("sport")       (:grouptags) ("@sport")       (:endgrouptag)
        (:startgrouptag) ("others")      (:grouptags) ("@others")      (:endgrouptag)))
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
      org-descriptive-links nil ;; nil display the full links
      org-startup-folded 'content
      org-startup-with-inline-images t
      org-pretty-entities t
      org-use-property-inheritance t
      org-use-sub-superscripts nil
      org-export-with-sub-superscripts '{}
      org-export-allow-bind-keywords nil
      org-tags-column -80
      org-cycle-include-plain-lists t ;; 'integrate ;; list initally folded
      org-tags-sort-function #'string>
      org-ellipsis "▼"
      org-use-speed-commands
      (lambda () (and (looking-at org-outline-regexp)
                 (looking-back "^\**" (line-beginning-position)))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Babel block options ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
(setenv "GRAPHVIZ_DOT" (executable-find "dot"))
(setq org-edit-src-content-indentation 0
      org-babel-sh-command "bash"
      org-babel-python-command "python3"
      ;; No pregunta al usuario antes de evaluar un bloque de código
      org-confirm-babel-evaluate nil
      org-src-fontify-natively t
      org-src-tab-acts-natively t
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
(setq org-duration-units '(("min" . 1)
                           ("h" . 60)
                           ("d" . 480)
                           ("w" . 2400)
                           ("m" . 9600)
                           ("y" . 96000))
      org-effort-durations '(("min" . 1)
                             ("h" . 60)
                             ("d" . 480)
                             ("w" . 2400)
                             ("m" . 9600)
                             ("y" . 96000))
      org-effort-threshold 20
      org-enforce-todo-dependencies t
      ;; [ 'note Graba el tiempo y una nota cuando se realiza una tarea
      org-log-done 'time ; 'time - solo guarda el tiempo
      ;; ]
      ;;org-clock-in-switch-to-state "STARTED"
      org-clock-out-when-done t
      org-columns-default-format
      "%51ITEM %8EFFORT(Estimate){:} %8CLOCKSUM(Clocked) %4TODO(State)" ;; "%25ITEM %TODO %3PRIORITY %TAGS"
      org-clock-into-drawer "LOGCLOCK"
      org-log-into-drawer "LOGSTATE"
      org-todo-keywords
      '(;; Basic
        (sequence "TODO(t)" "NEXT(n)" "STAR(s!)" "|" "DONE(d!)")
        ;; Actions
        (sequence "UNDO(u!)" "VERI(v@/!)" "|" "ENOU(e@/!)" "DELE(l@/!)")
        ;; States
        (sequence "PLAN(p!)" "LINK(k@/!)" "WAIT(w@/!)" "|" "FINI(f!)")
        ;; Problems
        (sequence "FIXM(b@/!)" "REOP(r@/!)" "HOLD(h@/!)" "|" "CANC(c@/!)"))
      org-archive-location "archived.org::* From %s"
      org-archive-file-header-format nil
      ;; PRIORITIES
      org-highest-priority ?A
      org-default-priority ?H
      org-lowest-priority ?O)

(setcdr (assoc 'state org-log-note-headings) "%-6S --> %-6s at %t")

;;;;;;;;;;;;;;;;;;;;;
;; Convert options ;;
;;;;;;;;;;;;;;;;;;;;;
(require 'ox)
(defun org-element-find-all (key)
  (let ((alist '())
        (pattern (concat "^[ \t]*#\\+\\(" key "\\):")))
    (org-with-wide-buffer
     (goto-char (point-min))
     (while (re-search-forward
             pattern nil t)
       (let ((element (org-element-at-point)))
         (when (eq (org-element-type element) 'keyword)
           (let ((val (org-element-property :value element)))
             (if (equal (org-element-property :key element)
                        key)
                 (push (read (format "%s" val)) alist))))))
     alist)))
;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert to Markdown ;;
;;;;;;;;;;;;;;;;;;;;;;;;;
;;(require 'ox-md)
(require 'ox-gfm)  ; Include ox-md
(defun org-gfm-publish-to-gfm (plist filename pub-dir)
  (org-publish-org-to 'gfm filename ".md" plist pub-dir))

;;;;;;;;;;;;;;;;;;;;;
;; Convert to Wiki ;;
;;;;;;;;;;;;;;;;;;;;;
(require 'ox-mediawiki)
;; Change dispatcher bind key because of conflict with markdown
(setcar (org-export-backend-menu (-first (lambda (b) (string-equal "mw" (symbol-name (org-export-backend-name b))))
        org-export-registered-backends)) ?M)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert to reStructuredText ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ox-rst)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert to LibreOffice ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'ox-odt nil t)

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
      ;; Convert formulas
      org-latex-to-mathml-convert-command
      "java -jar %j -unicode -force -df %o %I"
      org-latex-to-mathml-jar-file
      (expand-file-name "~/.emacs.d/cache/java/mathtoweb.jar")
      ;; Convert to other formats
      org-odt-convert-processes
      `(("WINWORD"
         ,(concat "WINWORD /q \"%i\" /mFormatAll"))
        ("LibreOffice"
         "soffice --headless --convert-to %f%x --outdir %d %i")
        ("unoconv"
         "unoconv -f %f -o %d %i"))
      org-odt-convert-process (cond
                               ((executable-find "WINWORD") "WINWORD")
                               ((executable-find "soffice") "LibreOffice")
                               ((executable-find "unoconv") "unoconv")
                               (t nil))
      org-odt-preferred-output-format (and org-odt-convert-process "doc"))
(defun org-odt-export-update-convert-processes ()
  (interactive)
  (setq org-odt-convert-processes
        `(("WINWORD"
           ,(concat "WINWORD /q \""
                    (concat (file-name-sans-extension buffer-file-name) ".odt")
                    "\" /m"
                    (or (car (org-element-find-all "MACRO"))
                        "FormatAll")))
          ("LibreOffice"
           "soffice --headless --convert-to %f%x --outdir %d %i")
          ("unoconv"
           "unoconv -f %f -o %d %i"))))
(defun org-export-update (backend)
  (cond
   ((eq 'odt backend)
    (org-odt-export-update-convert-processes))))
(add-hook 'org-export-before-processing-hook 'org-export-update)
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
(setq org-latex-listings 'minted ;; xor 'listings
      org-latex-listings-options
      '(("basicstyle" "\\small\\ttfamily")
        ("breaklines" "true")
        ("captionpos" "b")
        ("extendedchars" "true")
        ("tabsize" "4")
        ("columns" "fixed")
        ("keepspaces" "true")
        ("showstringspaces" "false")
        ("breaklines" "true")
        ("frame" "tb")
        ("framerule" "0.5pt")
        ("framexleftmargin" "0.5em")
        ("framexrightmargin" "0.5em")
        ("xleftmargin" "0.5em")
        ("xrightmargin" "0.5em")
        ("mathescape" "false")
        ("escapeinside" "{\\%*}{*)}")
        ("showspaces" "false")
        ("showstringspaces" "false")
        ("numbers" "none")
        ("numberstyle" "\\tiny\\ttfamily")
        ("commentstyle" "\\color{red}")
        ("keywordstyle" "\\color{blue}\\bfseries")
        ("stringstyle" "\\color{green}"))
      org-latex-minted-options
      '(("bgcolor" "blockbg"))
      org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
        "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")
      org-latex-packages-alist
      '(
        ;; ("verbose,a4paper,headheight=5mm,footskip=7mm,left=19mm,right=16mm,top=37mm,bottom=23mm" "geometry" nil)  ;; problems with beamer class
        ("AUTO" "babel" nil)
        ("" "multirow" nil)
        ("" "multicol" nil)
        ;; ("" "titlesec" nil)  ;; problems with beamer class
        ;; ("" "listings")
        ;; xor
        ("" "minted")
        ("" "color" nil)
        "
\\makeatletter
\\definecolor{blockbg}{rgb}{0.95,0.95,0.95}
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
")
      ;; org-latex-image-default-width nil
      org-beamer-outline-frame-options "allowframebreaks=0.9"
      org-beamer-frame-default-options "allowframebreaks=0.9"
      )
;; ]


(when (and (eq org-latex-listings 'minted)
           (not (executable-find "pygmentize")))
  (message-color #("WARN pygmentize not found, add to path or run 'apt install python-pygments'."
                       0 4 (face warning))))
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

;;;;;;;;;;;
;; Links ;;
;;;;;;;;;;;
(setq org-html-validation-link nil
      org-return-follows-link t)
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

(setq org-directory (or (loop for folder in
                              `("~/var/Dropbox/Orgzly"
                                ,(concat "~/Prog/org/" (getenv "JOB_FOLDER"))
                                "~/Prog/org")
                              when (file-exists-p folder)
                              return folder)
                        org-directory)
      org-default-notes-file (concat (or org-directory "~") "/.notes.org")
      org-agenda-files (if org-directory
                           `(,org-directory)
                         nil)
      ;; <property>
      org-agenda-property-list '("REQUIRED")
      ;; <Calendar>
      calendar-week-start-day 1
      calendar-day-name-array     ["domingo" "lunes" "martes"
                                   "miércoles" "jueves" "viernes" "sábado"]
      calendar-day-abbrev-array   ["dom" "lun" "mar" "mié" "jue" "vie" "sáb"]
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
      '((agenda habit-down time-up user-defined-up category-keep)
        (todo priority-down category-keep)
        (tags priority-down category-keep)
        (search category-keep))
      org-agenda-cmp-user-defined 'org-agenda-cmp-user-defined-function
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
           '(org-agenda-skip-entry-if
             'notregexp
             ":STYLE: *habit"))))
        ("c" "[c]alendar" cfw:open-org-calendar-command)
        ("n" "[n]ot closed timestamp"
         ((alltodo ""
                   ((org-agenda-skip-function
                     '(org-agenda-skip-entry-if
                       'regexp
                       "CLOSED: \\[[0-9][0-9][0-9][0-9]-[0-1][0-9]-[0-3][0-9]"))))))
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

;;;;;;;;;;;
;; Brain ;;
;;;;;;;;;;;
(require 'org-brain)

(setq org-brain-path (or (loop for folder in
                               '("~/var/Dropbox/Brain")
                               when (file-exists-p folder)
                               return folder)
                         org-brain-path)
      )

;;;;;;;;;;;;;;;
;; Functions ;;
;;;;;;;;;;;;;;;
(defun org-entry-is-todo-get-subtree (pos property)
  (save-excursion
    (goto-char pos)
    (save-restriction
      (org-narrow-to-subtree)
      (goto-char (point-max))
      (save-match-data
        (cl-loop while (re-search-backward org-heading-regexp nil t)
                 when (org-entry-is-todo-p)
                 collect (org-entry-get (point) property))))))


(defun org-entry-to-key (&optional pos)
  (let ((entry-pos (or pos (point)))
        (order '(;; Todo
                 ("FIXM" . 49) ;; 48 ?0
                 ("REOP" . 50)
                 ("VERI" . 51)
                 ("PLAN" . 52)
                 ("STAR" . 53)
                 ("NEXT" . 54)
                 ("TODO" . 55)
                 ("UNDO" . 56)
                 ;; Hold
                 ("LINK" . 64) ;; ?A
                 ("WAIT" . 65)
                 ("HOLD" . 66)
                 ;; Done
                 ("ENOU" . 71) ;; 97 ?a
                 ("DELE" . 72)
                 ("CANC" . 73)
                 ("FINI" . 74)
                 ("DONE" . 75)
                 )))
    (let ((todo (cdr (assoc (org-entry-get entry-pos "TODO") order))))
      (cond
       ((not todo)
        " ")
       ((< todo 65)
        ;;
        ;; todo
        ;;
        (let ((effort-list (cl-loop for minutes in (org-entry-is-todo-get-subtree entry-pos "EFFORT")
                                    when minutes
                                    collect (org-duration-to-minutes minutes)))
              (priority (org-entry-get entry-pos "PRIORITY"))
              (tags (mapconcat 'identity
                               (sort (split-string
                                      (or (org-entry-get entry-pos "TAGS") "") ":" t)
                                     'string<) " ")))
          (concat
           ;; type of todo
           "0"
           ;; effort condition
           (if (and effort-list (< (apply 'min effort-list) org-effort-threshold))
               " "
             "~")
           priority
           (byte-to-string todo)
           tags)))
       ((< todo 71)
        ;;
        ;; blocked
        ;;
        (let ((priority (org-entry-get entry-pos "PRIORITY"))
              (tags (mapconcat 'identity
                               (sort (split-string
                                      (or (org-entry-get entry-pos "TAGS") "") ":" t)
                                     'string<) " ")))
          (concat
           "1"
           (byte-to-string todo)
           priority
           tags)))
       (t
        ;;
        ;; done
        ;;
        (let ((closed (let ((closed-time (org-entry-get entry-pos "CLOSED")))
                        (if closed-time
                            (number-to-string
                             (- most-positive-fixnum
                                (round (org-time-string-to-seconds closed-time))))
                          " "))))
          (concat
           "2"
           closed)))))))

(defun org-agenda-cmp-user-defined-function (a b)
  (let ((a-pos (or (get-text-property 1 'org-marker a)
                   (get-text-property 1 'org-hd-marker a)))
        (b-pos (or (get-text-property 1 'org-marker b)
                   (get-text-property 1 'org-hd-marker b))))
    (if (and a-pos b-pos)
        (let ((a-key (with-current-buffer (marker-buffer a-pos)
                       (org-entry-to-key a-pos)))
              (b-key (with-current-buffer (marker-buffer b-pos)
                       (org-entry-to-key b-pos))))
          (if (string< a-key b-key)
              -1
            1))
      ;; (message "A pos: %s heading: %s entry: %s" a-pos (text-property-any 0 (length a) 'org-heading t a) a)
      ;; (message "B pos: %s heading: %s entry: %s" b-pos (text-property-any 0 (length b) 'org-heading t b) b)
      (if (string< a b)
          -1
        1))))

(defun org-sort-entries-user-defined ()
  (interactive)
  (org-sort-entries nil ?f 'org-entry-to-key 'string<))

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

;;;;;;;;;;;;;;;;;;;
;; Org templates ;;
;;;;;;;;;;;;;;;;;;;
(require 'org-tempo)
;; [ Thanks to: https://gist.github.com/alphapapa/84ec3396442915ca277b
(require 's)
(defun org-read-structure-template ()
  "Read org-mode structure template with completion.  Returns template string."
  (let* ((templates (map 'list 'cdr org-structure-template-alist))
         (template (completing-read "Template: " templates nil t)))
    (car (rassoc template org-structure-template-alist))))

(defun org-babel-insert-structure-template-or-enclose-region ()
  "Insert structure block template.  When region is active, enclose region in block."
  (interactive)
  (let* ((template (org-read-structure-template))
         (text ""))
    (if (use-region-p)
        (progn
          (setq text (buffer-substring-no-properties (region-beginning) (region-end)))
          (delete-region (region-beginning) (region-end))
          (insert (concat "<" template))
          (org-tempo-complete-tag)
          (when (char-equal ?  (char-before (point)))
            (forward-line)
            (end-of-line))
          (insert text))
      (insert (concat "<" template))
      (org-tempo-complete-tag))))
;; ]
;;;;;;;;;;
;; Keys ;;
;;;;;;;;;;
(add-hook 'org-mode-hook '(lambda()
                            (set
                             (make-local-variable 'semantic-mode) nil)))

(define-key org-mode-map (kbd "M-.") #'org-priority)
(define-key org-mode-map (kbd "C-c v e") #'org-show-entry)
(define-key org-mode-map (kbd "C-c TAB") #'org-show-subtree)
(define-key org-mode-map (kbd "C-c v t") #'org-show-todo-tree)
(define-key org-mode-map (kbd "C-c v s") #'org-block-and-result-show-all) 
(define-key org-mode-map (kbd "C-c v h") #'org-block-and-result-hide-all) 
(define-key org-mode-map (kbd "C-c M-s") #'org-sort-entries-user-defined)
(define-key org-mode-map (kbd "C-c c") #'org-capture)
(define-key org-mode-map (kbd "C-c a") #'org-agenda)
(define-key org-mode-map (kbd "C-c C-l") #'org-store-link)
(define-key org-mode-map (kbd "C-c L") #'org-insert-link-global)
(define-key org-mode-map (kbd "C-c O") #'org-open-at-point-global)
(define-key org-mode-map (kbd "C-c p") #'org-publish)
(define-key org-mode-map (kbd "C-c C-v <") #'org-babel-insert-structure-template-or-enclose-region)
(define-key org-mode-map (kbd "C-c C-x D") #'org-archive-done-tasks)
(define-key org-mode-map (kbd "C-c C-x C-k") #'org-toggle-link-display)


(provide 'org-config)
;;; org-config.el ends here
