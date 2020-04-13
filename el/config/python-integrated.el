;;; python-integrated.el --- Configure elpy and utils

;;; Commentary:

;; Usage:
;; (require 'python-integrated)

;;; Code:

;; Los « y » ya no serán delimitadores, tan sólo
;; palabras (words "w") para no afectar el
;; funcionamiento de los modos.
(modify-syntax-entry ?« "w")
(modify-syntax-entry ?» "w")

(defvar pytex-filter-print-line-p nil)
(defvar pytex-filter-python-io-busy-p nil)
(defvar pytex-filter-python-io-complete-string "")
(defvar pytex-my-python-output-buffer nil)
(defvar pytex-point-for-insertion 0)
(defvar pytex-retrieve-last-command "")

;; Funciones para procesar comandos python
(defun pytex-release-io-term ()
  "Desbloquea el flujo con la terminal python."
  (interactive)
  (set 'pytex-filter-print-line-p nil))


(require 'term)
(defun pytex-set-io-term-filter ()
  "Establece el filtro de las interacciones con el terminal de python.
Las salidas de la terminal de python aparecerán en el buffer donde
se ejecute esta función."
  (set 'pytex-filter-print-line-p nil)
  (set 'pytex-filter-python-io-busy-p nil)
  (set 'pytex-filter-python-io-complete-string "")
  (defun pytex-insertion-filter (proc string)
    (term-emulate-terminal proc string)
    (when (and pytex-filter-python-io-busy-p
               (eq pytex-my-python-output-buffer (current-buffer)))
      (set 'pytex-filter-python-io-complete-string
        (concat pytex-filter-python-io-complete-string
                (replace-regexp-in-string "" "" string t t)))
      (when (equal (substring pytex-filter-python-io-complete-string
                              (- (min (length pytex-filter-python-io-complete-string)
                                      4))) ">>> ")
        (let* ((complete-string (replace-regexp-in-string
                                 "^\\(>>>\\|\\.\\.\\.\\) [^\n]*\n" ""
                                 (substring pytex-filter-python-io-complete-string
                                            (+ 1 (string-match "\n"
                                       pytex-filter-python-io-complete-string)) -4) t t))
               (complete-string-length (length complete-string)))
          (set 'pytex-filter-python-io-complete-string "")
          (if (or (equal (substring complete-string
                                    0 (min complete-string-length 11))
                         "Traceback (")
                  (equal (substring complete-string
                                    0 (min complete-string-length 9))
                         "  File \"<"))
            (progn
              (save-excursion
                (goto-char pytex-point-for-insertion)
                (insert "\n")
                (insert complete-string)
                (set 'pytex-point-for-insertion
                     (+ complete-string-length pytex-point-for-insertion 1)))
              (set 'pytex-filter-print-line-p nil)
              (set 'pytex-filter-python-io-busy-p nil))
            (if (equal complete-string "")
              (set 'pytex-filter-python-io-busy-p nil)
              (if pytex-filter-print-line-p
                (progn
                  (save-excursion
                    (goto-char pytex-point-for-insertion)
                    (insert (concat (substring complete-string 0 -1) " "))
                    (set 'pytex-point-for-insertion
                         (+ (length complete-string) pytex-point-for-insertion)))
                  (set 'pytex-filter-print-line-p nil)
                  (set 'pytex-filter-python-io-busy-p nil))
                (progn
                  (process-send-string
                    (get-buffer-process "*Python-IO*")
                    (concat pytex-retrieve-last-command "\n"))
                  (set 'pytex-filter-print-line-p t)))))))))
  (set-process-filter (get-buffer-process "*Python-IO*") 'pytex-insertion-filter))

;; Funciones de arranque del servicio.
(defun pytex-start-python-term ()
  "Abre un buffer con una terminal de python."
  (interactive)
  (if (get-buffer "*Python-IO*")
    (message "Another python terminal has already started.")
    (progn
      (set 'pytex-retrieve-last-command "_")
      (ansi-term "/usr/bin/python3" "Python-IO")
      ;;(term-line-mode) o (term-char-mode) por defecto.
      (previous-buffer)
      (pytex-set-io-term-filter)
      (message "Python term started"))))

(defun pytex-start-sympy-term ()
  "Abre un buffer con una terminal de python."
  (interactive)
  (if (get-buffer "*Python-IO*")
    (message "Another python terminal has already started.")
    (progn
      (set 'pytex-retrieve-last-command "print(latex(_))")
      (ansi-term "/usr/bin/python3" "Python-IO")
      ;;(term-line-mode) o (term-char-mode) por defecto.
      (process-send-string (get-buffer-process (current-buffer))
  "from __future__ import division
from sympy import *
x, y, z, t = symbols('x y z t')
k, m, n = symbols('k m n', integer=True)
f, g, h = symbols('f g h', cls=Function)
")
      (while (not (equal (let ((end (point-max)))
                      (buffer-substring (max 1 (- end 4)) end)) ">>> ")
             )
        (sleep-for 0 10)
      )
      (previous-buffer)
      (pytex-set-io-term-filter)
      (message "Sympy term started"))))

(defun pytex-start-pytex-term ()
  "Abre un buffer con una terminal de python."
  (interactive)
  (if (get-buffer "*Python-IO*")
    (message "Another python terminal has already started.")
    (progn
      (set 'pytex-retrieve-last-command "lp(_,\"newline\")")
      (ansi-term "/usr/bin/python3" "Python-IO")
      ;;(term-line-mode) o (term-char-mode) por defecto.
      (process-send-string (get-buffer-process (current-buffer))
        "import sys
sys.path.insert(0,'/home/edo/Prog/Python-LaTeX/python')
from pytex import *
")
      (while (not (equal (let ((end (point-max)))
                      (buffer-substring (max 1 (- end 4)) end)) ">>> "))
        (sleep-for 0 10))
      (previous-buffer)
      (pytex-set-io-term-filter)
      (message "Pytex term started"))))

;; Edita exclusivamente la región python
(defun pytex-python-mode ()
  "Edita el bloque python."
  (interactive)
  (let* ((pytex-begin (progn (search-backward "««" nil t) (match-end 0)))
         (pytex-end (progn (search-forward "»»" nil t) (match-beginning 0)))
         (str-command (buffer-substring pytex-begin pytex-end))
        )
    (narrow-to-region pytex-begin pytex-end)
    (python-mode)))

;; Edita todo el documento otra vez en latex
(defun pytex-latex-mode ()
  "Edita todo el documento de nuevo en latex."
  (interactive)
  (widen)
  (latex-mode))

(defun pytex-xml-mode ()
  "Edita todo el documento de nuevo en xml."
  (interactive)
  (widen)
  (xml-mode))

;; Funciones de evaluación de entornos pytex.
(defun pytex-to-tex ()
  "Evalua la línea en python."
  (interactive)
  (if (get-buffer "*Python-IO*")
    (progn
      (while pytex-filter-python-io-busy-p (sleep-for 0 10))
      (set 'pytex-filter-python-io-busy-p t)
      (let ((pytex-goto-pos nil))
        (save-excursion
          (let* ((pytex-begin (progn (search-backward "««" nil t) (match-end 0)))
                 (pytex-end (progn (search-forward "»»" nil t) (match-beginning 0)))
                 (str-command (buffer-substring pytex-begin pytex-end)))
            (save-restriction
              (narrow-to-region pytex-begin pytex-end)
              (unless (condition-case data
                        (scan-sexps (point-min) (point-max))
                        (scan-error (set 'pytex-goto-pos (nth 2 data))
                                    (set 'pytex-filter-python-io-busy-p nil)
                                    (message "Unmatched bracket or quote.")))
                (progn
                  (set 'str-command (cl-subseq str-command 0
                                      (string-match "#=" str-command)))
                  (set 'str-command (replace-regexp-in-string
                                     "\\`\\( *\n\\)+" "" str-command t t))
                  (if (string-match "[^ \n]" str-command)
                    (progn
                      (set 'str-command (replace-regexp-in-string
                                         "\\( *\n\\)* *\\'" "" str-command t t))
                      (while (equal (cl-subseq str-command 0 1) " ")
                        (set 'str-command (replace-regexp-in-string
                                              "\n " "\n" (cl-subseq str-command 1) t t)))
                      (let* ((last-command (last (split-string str-command "\n"))))
                        (if (equal (cl-subseq last-command 0 1) " ")
                          (set 'str-command (concat str-command "\n\n"))
                          (set 'str-command (concat str-command "\n"))))
                      (goto-char pytex-end)
                      (if (search-backward "#=" nil t)
                        (progn (goto-char (match-end 0))
                          (delete-region (match-end 0) pytex-end)
                          (insert " "))
                        (insert " #= "))
                      (set 'pytex-point-for-insertion (point))
                      (set 'pytex-my-python-output-buffer (current-buffer))
                      (process-send-string (get-buffer-process "*Python-IO*") str-command))
                    (progn
                      (message "Empty pytex environment.")
                      (set 'pytex-filter-python-io-busy-p nil))))))))
        (when pytex-goto-pos (goto-char pytex-goto-pos))))
    (message "Python terminal is not ready.")))

(defun pytex-to-tex-all ()
  "Evalua todas las líneas pytex del fichero en python."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "««" nil t)
      (pytex-to-tex)))
  (message "All sent."))

(defun pytex-to-tex-region ()
  "Evalua todas las líneas pytex de la región en python."
  (interactive)
  (save-excursion
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (goto-char (point-min))
      (while (search-forward "««" nil t)
        (pytex-to-tex))))
  (message "Region sent."))

(defun pytex-clean-environments ()
  "Remove all pytex environments and leave results."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "««" nil t)
      (let* ((str-latex (delete-and-extract-region
                         (match-beginning 0)
                         (search-forward "»»" nil t)))
             (ind-latex (cl-search "#=" str-latex))
             (pos (point)))
        (when ind-latex
          (insert
            (replace-regexp-in-string " +\\'" ""
              (replace-regexp-in-string "\\` +" ""
                  (cl-subseq str-latex (+ 2 ind-latex) (- (length "»»"))) t t) t t)))
        (if (string-match "[^ ]" (buffer-substring (progn (beginning-of-line) (point))
                       (progn (end-of-line) (point))))
          (goto-char pos)
          (kill-whole-line)))))
  (message "PyTeX: code cleaned up."))

(defun pytex-export ()
  "Limpia y graba en un archivo diferente."
  (interactive)
  (let* ((name-split (split-string (buffer-name) "\\."))
         (last-split (last name-split 2))
         (middle-split (cl-first last-split)))
    (if (equal middle-split "py")
      (progn
        (set 'middle-split
          (mapconcat 'identity
            (append (butlast name-split 2) (last last-split)) "."))
        (copy-to-buffer middle-split (buffer-end -1) (buffer-end 1))
        (switch-to-buffer middle-split)
        (pytex-clean-environments)
        (save-buffer)
        (previous-buffer))
      (message "Wrong buffer name. Insert .py"))))

(defun pytex-stop-term ()
  "Finaliza la terminal de python."
  (interactive)
  (process-send-string (get-buffer-process "*Python-IO*") "quit()\n"))

;; Keys
(defun pytex-set-keys ()
  "Set pytex keys."
  (interactive)
  (global-set-key (kbd "M-p M-p") 'pytex-python-mode)
  (global-set-key (kbd "M-p M-l") 'pytex-latex-mode)
  (global-set-key (kbd "M-p M-x") 'pytex-xml-mode)
  (global-set-key (kbd "M-p M-t") 'pytex-start-pytex-term)
  (global-set-key (kbd "M-p M-o") 'pytex-to-tex)
  (global-set-key (kbd "M-p M-a") 'pytex-to-tex-all)
  (global-set-key (kbd "M-p M-r") 'pytex-to-tex-region)
  (global-set-key (kbd "M-p M-c") 'pytex-clean-environments)
  (global-set-key (kbd "M-p M-e") 'pytex-export)
  (global-set-key (kbd "M-p M-q") 'pytex-stop-term))

(global-set-key (kbd "M-p M-k") 'pytex-set-keys)
(pytex-set-keys)

;; Funciones para web
(defun pytex-library-mathjax ()
  "Add links to library mathjax and configure it."
  (interactive)
  (save-excursion
    (goto-char 0)
    (let ((pos (search-forward "</head>" nil t)))
      (if pos
        (progn
          (goto-char (- pos 7))
          (insert "\n")
          (backward-char)
          (indent-for-tab-command)
          (insert "<script type=\"text/x-mathjax-config\">MathJax.Hub.Config({tex2jax: {inlineMath: [['$','$'], ['\\(','\\)']],displayMath:[['$$','$$'],['\\[','\\]']]}});</script>\n")
          (indent-for-tab-command)
          (insert "<script type=\"text/javascript\" async src=\"/home/edo/Prog/javascript/lib/MathJax/MathJax.js?config=TeX-AMS-MML_SVG\"></script>")
          (forward-char)
          (indent-for-tab-command))
        (message "Head requiered.")))))

(defun pytex-library-jquery ()
  "Add links to library jquery and configure it."
  (interactive)
  (save-excursion
    (goto-char 0)
    (let ((pos (search-forward "<head>" nil t)))
      (if pos
        (progn
          (insert "\n")
          (indent-for-tab-command)
          (insert "<script type=\"text/javascript\" src=\"/home/edo/Prog/javascript/lib/jquery/jquery.min.js\"></script>\n")
          (indent-for-tab-command)
          (insert "<script type=\"text/javascript\" src=\"/home/edo/Prog/javascript/pytex/solutions.js\"></script>"))
        (message "Head requiered.")))))

(defun pytex-library-css ()
  "Add links to library css and configure it."
  (interactive)
  (save-excursion
    (goto-char 0)
    (let ((pos (search-forward "<head>" nil t)))
      (if pos
        (progn
          (insert "\n")
          (indent-for-tab-command)
          (insert "<link rel=\"stylesheet\" type=\"text/css\" href=\"/home/edo/Prog/css/pytex/exercises.css\" />"))
        (message "Head requiered.")))))

(defun pytex-library-meta ()
  "Add links to library meta and configure it."
  (interactive)
  (save-excursion
    (goto-char 0)
    (let ((pos (search-forward "<head>" nil t)))
      (if pos
        (progn
          (insert "\n")
          (indent-for-tab-command)
          (insert "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />"))
        (message "Head requiered.")))))

(defun pytex-libraries ()
  "Add links to library mathjax and configure it."
  (interactive)
  (pytex-library-css)
  (pytex-library-jquery)
  (pytex-library-mathjax)
  (pytex-library-meta))


(provide 'python-integrated)
;;; python-integrated.el ends here
