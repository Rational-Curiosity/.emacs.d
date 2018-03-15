;;; plantuml-config.el --- Configure plantuml

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'plantuml-mode
;;   (require 'python-config))
;; or:
;; (with-eval-after-load 'plantuml-config
;;   )
;; never:
;; (require 'plantuml-config)

;; Do not include in this file:
;; (require 'plantuml-mode)


;; plantuml documentation:


;; Emphasized text

;; @startuml
;; Alice -> Bob : hello --there--
;; ... Some ~~long delay~~ ...
;; Bob -> Alice : ok
;; note left
;;   This is **bold**
;;   This is //italics//
;;   This is ""monospaced""
;;   This is --stroked--
;;   This is __underlined__
;;   This is ~~waved~~
;; end note
;; @enduml

;; bold italic monospaced stroke underline wave creole syntax
;; List

;; @startuml
;; object demo {
;;   * Bullet list
;;   * Second item
;; }
;; note left
;;   * Bullet list
;;   * Second item
;;   ** Sub item
;; end note

;; legend
;;   # Numbered list
;;   # Second item
;;   ## Sub item
;;   ## Another sub item
;;   # Third item
;; end legend
;; @enduml

;; list in creole
;; Escape character
;; You can use the tilde ~ to escape special creole characters.

;; @startuml
;; object demo {
;;   This is not ~___underscored__.
;;   This is not ~""monospaced"".
;; }
;; @enduml

;; escaping character in creole
;; Horizontal lines

;; @startuml
;; database DB1 as "
;; You can have horizontal line
;; ----
;; Or double line
;; ====
;; Or strong line
;; ____
;; Or dotted line
;; ..My title..
;; Enjoy!
;; "
;; note right
;;   This is working also in notes
;;   You can also add title in all these lines
;;   ==Title==
;;   --Another title--
;; end note

;; @enduml

;; separator in creole
;; Headings

;; @startuml
;; usecase UC1 as "
;; = Extra-large heading
;; Some text
;; == Large heading
;; Other text
;; === Medium heading
;; Information
;; ....
;; ==== Small heading"
;; @enduml

;; heading in creole
;; Legacy HTML
;; Some HTML tags are also working:

;;     <b> for bold text
;;     <u> or <u:#AAAAAA> or <u:colorName> for underline
;;     <i> for italic
;;     <s> or <s:#AAAAAA> or <s:colorName> for strike text
;;     <w> or <w:#AAAAAA> or <w:colorName> for wave underline text
;;     <color:#AAAAAA> or <color:colorName>
;;     <back:#AAAAAA> or <back:colorName> for background color
;;     <size:nn> to change font size
;;     <img:file> : the file must be accessible by the filesystem
;;     <img:http://url> : the URL must be available from the Internet

;; @startuml
;; :* You can change <color:red>text color</color> 
;; * You can change <back:cadetblue>background color</back> 
;; * You can change <size:18>size</size> 
;; * You use <u>legacy</u> <b>HTML <i>tag</i></b> 
;; * You use <u:red>color</u> <s:green>in HTML</s> <w:#0000FF>tag</w>
;; ----
;; * Use image : <img:sourceforge.jpg> 
;; ;

;; @enduml

;; some HTML tag are working with creole
;; Table
;; It is possible to build table.

;; @startuml
;; skinparam titleFontSize 14
;; title
;;   Example of simple table
;;   |= |= table |= header |
;;   | a | table | row |
;;   | b | table | row |
;; end title
;; [*] --> State1
;; @enduml

;; simple table in creole
;; You can specify background colors for cells and lines.

;; @startuml
;; start
;; :Here is the result
;; |= |= table |= header |
;; | a | table | row |
;; |<#FF8080> red |<#80FF80> green |<#8080FF> blue |
;; <#yellow>| b | table | row |;
;; @enduml

;; background color for table in creole
;; Tree
;; You can use |_ characters to build a tree.

;; @startuml
;; skinparam titleFontSize 14
;; title
;;   Example of Tree
;;   |_ First line
;;   |_ **Bom(Model)**
;;     |_ prop1
;;     |_ prop2
;;     |_ prop3
;;   |_ Last line
;; end title
;; [*] --> State1
;; @enduml

;; tree in creole
;; Special characters
;; It's possible to use any unicode characters with &# syntax or <U+XXXX>

;; usecase foo as "this is &#8734; long"
;; usecase bar as "this is also <U+221E> long"

;; unicode support in creole
;; OpenIconic
;; OpenIconic is an very nice open source icon set. Those icons have been integrated into the creole parser, so you can use them out-of-the-box.

;; You can use the following syntax: <&ICON_NAME>.

;; @startuml
;; title: <size:20><&heart>Use of OpenIconic<&heart></size>
;; class Wifi
;; note left
;;   Click on <&wifi>
;; end note
;; @enduml


;;; Code:

(message "Importing plantuml-mode")
(setq plantuml-jar-path (expand-file-name "~/.emacs.d/cache/java/plantuml.jar"))


(provide 'plantuml-config)
;;; plantuml-config.el ends here
