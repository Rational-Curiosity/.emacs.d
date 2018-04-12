(if (string-equal
     "\300\301\302\303\304\211;\203\0\305\306#\266\202\202\0\307A@\"\266\202!#\207"
     (aref (symbol-function 'org-odt-verbatim) 1))
    (defun org-odt-verbatim (verbatim contents info)
      "Transcode a VERBATIM object from Org to ODT.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
      (format "<text:span text:style-name=\"%s\">%s</text:span>"
              "OrgVerbatim" (org-odt--encode-plain-text
                             (org-element-property :value verbatim))))
  (message "WARN ox-odt-bug.el: bug fixed for different version of org-odt-verbatim"))




(provide 'ox-odt-bug)