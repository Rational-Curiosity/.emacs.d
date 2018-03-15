(defun org-odt-verbatim (verbatim contents info)
  "Transcode a VERBATIM object from Org to ODT.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (format "<text:span text:style-name=\"%s\">%s</text:span>"
	  "OrgVerbatim" (org-odt--encode-plain-text
		     (org-element-property :value verbatim))))


(provide 'ox-odt-bug)