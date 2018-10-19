(when (bug-check-function-bytecode
       'org-beamer--format-frame
       "wgMIwwTEJcXGBYk7gxcAx8gDAyOCHQDJAUFAAyK2gomEJwDKgjkAy8wCIoM1AM0BziKCOQDNAc8isgHQBgaJO4NLAMfIAwMjglEAyQFBQAMitoLR0tPUyQYJ1SLWIgSFcgDUy9cGByKFcADY2QYHItYiBYWDAMvaBgcihoMAy9sGByI/hagA3AYMBgsi3d4C3+AZywMDAyMptoODogDhgqMA4gIisgFDIyLN48MGB4O4AOQEnYO8AAOCvwDkBELWI+UitoLmBgeJO4PVAMfIAwMjgtsAyQFBQAMitoLd5wKD7gACl+iag+4AyoIKAenqBguJO4P/AMfIAwMjggUByQFBQAMitoIGCSIisgHrBYQXAQYHgiEB7O3uBgqGIAHKI++wB4c=")
  (defun org-beamer--format-frame (headline contents info)
    "Format HEADLINE as a frame.
CONTENTS holds the contents of the headline.  INFO is a plist
used as a communication channel."
    (let ((fragilep
           ;; FRAGILEP is non-nil when HEADLINE contains an element
           ;; among `org-beamer-verbatim-elements'.
           (org-element-map headline org-beamer-verbatim-elements 'identity
                            info 'first-match)))
      (concat "\\begin{frame}"
              ;; Overlay specification, if any. When surrounded by
              ;; square brackets, consider it as a default
              ;; specification.
              (let ((action (org-element-property :BEAMER_ACT headline)))
                (cond
                 ((not action) "")
                 ((string-match "\\`\\[.*\\]\\'" action )
                  (org-beamer--normalize-argument action 'defaction))
                 (t (org-beamer--normalize-argument action 'action))))
              ;; Options, if any.
              (let* ((beamer-opt (org-element-property :BEAMER_OPT headline))
                     (options
                      ;; Collect nonempty options from default value and
                      ;; headline's properties.  Also add a label for
                      ;; links.
                      (cl-remove-if-not 'org-string-nw-p
                                        (append
                                         (org-split-string
                                          (plist-get info :beamer-frame-default-options) ",")
                                         (and beamer-opt
                                              (org-split-string
                                               ;; Remove square brackets if user provided
                                               ;; them.
                                               (and (string-match "^\\[?\\(.*\\)\\]?$" beamer-opt)
                                                    (match-string 1 beamer-opt))
                                               ","))
                                         ;; Provide an automatic label for the frame
                                         ;; unless the user specified one.  Also refrain
                                         ;; from labeling `allowframebreaks' frames; this
                                         ;; is not allowed by beamer.
                                         (unless (or (and beamer-opt
                                                          (or (string-match "\\(^\\|,\\)label=" beamer-opt)
                                                              (string-match "allowframebreaks" beamer-opt)))
                                                     (string-match "allowframebreaks" org-beamer-frame-default-options))
                                           (list
                                            (let ((label (org-beamer--get-label headline info)))
                                              ;; Labels containing colons need to be
                                              ;; wrapped within braces.
                                              (format (if (string-match-p ":" label)
                                                          "label={%s}"
                                                        "label=%s")
                                                      label))))))))
                ;; Change options list into a string.
                (org-beamer--normalize-argument
                 (mapconcat
                  'identity
                  (if (or (not fragilep) (member "fragile" options)) options
                    (cons "fragile" options))
                  ",")
                 'option))
              ;; Title.
              (let ((env (org-element-property :BEAMER_ENV headline)))
                (format "{%s}"
                        (if (and env (equal (downcase env) "fullframe")) ""
                          (org-export-data
                           (org-element-property :title headline) info))))
              "\n"
              ;; The following workaround is required in fragile frames
              ;; as Beamer will append "\par" to the beginning of the
              ;; contents.  So we need to make sure the command is
              ;; separated from the contents by at least one space.  If
              ;; it isn't, it will create "\parfirst-word" command and
              ;; remove the first word from the contents in the PDF
              ;; output.
              (if (not fragilep) contents
                (replace-regexp-in-string "\\`\n*" "\\& " (or contents "")))
              "\\end{frame}"))))


(provide 'ox-beamer-bug)