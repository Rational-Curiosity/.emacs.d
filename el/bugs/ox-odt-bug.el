(when (bug-check-function-bytecode
       'org-odt-verbatim
       "\300\301\302\303\304\211;\203\0\305\306#\266\202\202\0\307A@\"\266\202!#\207")
  (defun org-odt-verbatim (verbatim contents info)
    "Transcode a VERBATIM object from Org to ODT.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
    (format "<text:span text:style-name=\"%s\">%s</text:span>"
            "OrgVerbatim" (org-odt--encode-plain-text
                           (org-element-property :value verbatim)))))


(provide 'ox-odt-bug)