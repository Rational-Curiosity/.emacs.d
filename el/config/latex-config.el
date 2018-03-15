;;; latex-config.el --- Configure latex

;;; Commentary:

;; Usage:
;; (require 'latex-config)

;;; Code:

(message "Importing latex-config")
;; Más botones para el modo latex
(defvar TeX-bar-LaTeX-button-alist
     (quote
      ((latex :image
              (lambda nil
                (if TeX-PDF-mode "pdftex" "tex"))
              :command
              (progn
                (TeX-save-document
                 (TeX-master-file))
                (TeX-command "LaTeX"
                             (quote TeX-master-file)
                             -1))
              :help
              (lambda
                (&rest ignored)
                (TeX-bar-help-from-command-list "LaTeX")))
       (pdfpslatex :image "pdftex" :command
                   (progn
                     (TeX-save-document
                      (TeX-master-file))
                     (TeX-command "PDFpsLaTeX"
                                  (quote TeX-master-file)
                                  -1))
                   :help
                   (lambda
                     (&rest ignored)
                     (TeX-bar-help-from-command-list "PDFpsLaTeX")))
       (evince :image "viewpdf" :command
               (TeX-command "Evince"
                            (quote TeX-master-file)
                            -1)
               :help
               (lambda
                 (&rest ignored)
                 (TeX-bar-help-from-command-list "Evince")))
       (clean :image "delete" :command
              (TeX-command "Clean"
                           (quote TeX-master-file)
                           -1)
              :help
              (lambda
                (&rest ignored)
                (TeX-bar-help-from-command-list "Clean")))
       (pdflatex :image "pdftex" :command
                 (progn
                   (TeX-save-document
                    (TeX-master-file))
                   (TeX-command "PDFLaTeX"
                                (quote TeX-master-file)
                                -1))
                 :help
                 (lambda
                   (&rest ignored)
                   (TeX-bar-help-from-command-list "PDFLaTeX")))
       (next-error :image "error" :command TeX-next-error :enable
                   (plist-get TeX-error-report-switches
                              (intern
                               (TeX-master-file)))
                   :visible
                   (plist-get TeX-error-report-switches
                              (intern
                               (TeX-master-file))))
       (view :image
             (lambda nil
               (if TeX-PDF-mode "viewpdf" "viewdvi"))
             :command
             (TeX-command "View"
                          (quote TeX-master-file)
                          -1)
             :help
             (lambda
               (&rest ignored)
               (TeX-bar-help-from-command-list "View")))
       (file :image "dvips" :command
             (TeX-command "File"
                          (quote TeX-master-file)
                          -1)
             :visible
             (not TeX-PDF-mode)
             :help
             (lambda
               (&rest ignored)
               (TeX-bar-help-from-command-list "File")))
       (bibtex :image "bibtex" :command
               (TeX-command "BibTeX"
                            (quote TeX-master-file)
                            -1)
               :help
               (lambda
                 (&rest ignored)
                 (TeX-bar-help-from-command-list "BibTeX")))
       (clean :image "delete" :command
              (TeX-command "Clean"
                           (quote TeX-master-file)
                           -1)
              :help
              (lambda
                (&rest ignored)
                (TeX-bar-help-from-command-list "Clean")))
       (latex-symbols-experimental :alias :eval-group LaTeX-symbols-toolbar-switch-contents LaTeX-symbols-toolbar-contents))))
;; 
(defvar TeX-bar-LaTeX-buttons
     (quote
      (new-file open-file dired kill-buffer save-buffer undo cut copy paste
                [separator nil]
                latex next-error view bibtex
                (pdfpslatex evince clean))))


(provide 'latex-config)
;;; latex-config.el ends here
