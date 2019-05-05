;;; rtags-config.el --- Configure rtags

;;; Commentary:

;; Usage:
;; (with-eval-after-load 'cc-mode
;;   (with-eval-after-load 'rtags
;;     (require 'rtags-config)))
;; or:
;; (with-eval-after-load 'rtags-config
;;   )
;; never:
;; (require 'rtags-config)

;; Do not include in this file:
;; (require 'cc-mode)
;; or:
;; (require 'rtags)


;; rdm &
;; cd /path/to/project/root
;; [
;; cmake . -DCMAKE_EXPORT_COMPILE_COMMANDS=1
;; rc -J .
;; <xor>
;; make clean
;; make -nk all | rc -c -
;; ]

;; [ <cygwin>
;; static const char *strcasestr (const char *haystack, const char *needle)
;; {
;;   int haypos;
;;   int needlepos;

;;   haypos = 0;
;;   while (haystack[haypos]) {
;;     if (tolower (haystack[haypos]) == tolower(needle[0])) {
;;       needlepos = 1;
;;       while ( (needle[needlepos]) &&
;;               (tolower(haystack[haypos + needlepos])
;;                == tolower(needle[needlepos])) )
;;           ++needlepos;
;;       if (! needle[needlepos]) return (haystack + haypos);
;;     }
;;     ++haypos;
;;   }
;;   return NULL;
;; }
;; ] <cygwin>

;;; Code:

(message "Importing rtags-config")
;; if you built rtags manually you might need to do this (change path
;; accordingly)
;(add-to-list 'exec-path (expand-file-name "~/prog/rtags/bin")) ;; path to rdm/rc
;(add-to-list 'load-path (expand-file-name "~/prog/rtags/src")) ;; path to rtags.el

;;(require 'rtags)

;; (require 'company-rtags)

;; (setq rtags-completions-enabled t)
;; (eval-after-load 'company
;;   '(add-to-list
;;     'company-backends 'company-rtags))
;; (setq rtags-autostart-diagnostics t)
;; (rtags-enable-standard-keybindings)
(setq ;; No marca errores, lo dejamos a flycheck
      rtags-spellcheck-enabled nil
      rtags-timeout 10000)

;; (advice-add 'rtags-start-process-unless-running :before
;;             (lambda () (message "Loading rdm")))

(defhydra hydra-rtags (:foreign-keys run :hint nil)
  "
^Find^             ^Actions^             ^Info
^^^^^^^^-----------------------------------------------
_._: symbol at.    _R_: rename symbol    _V_: p. enum at.
_,_: refs at.      _X_: fixit at.        _D_: diagnostics
_/_: all refs at.  _F_: fixit            _G_: guess fun at.
_v_: virtuals at.  _Y_: cycle overlays   _P_: p. dependencies
_>_: symbol        _-_: back pos         _M_: symbol info
_<_: refs          _+_: forward pos      _S_: summary
_;_: file          _p_: set project      _B_: show rtags buf
^ ^                _e_: reparse file     _T_: taglist
^ ^                _E_: preprocess file
^ ^                _O_: goto offset
^ ^                _I_: imenu
"
  ("." rtags-find-symbol-at-point)
  ("," rtags-find-references-at-point)
  ("v" rtags-find-virtuals-at-point)
  ("V" rtags-print-enum-value-at-point)
  ("/" rtags-find-all-references-at-point)
  ("Y" rtags-cycle-overlays-on-screen)
  (">" rtags-find-symbol)
  ("<" rtags-find-references)
  ("-" rtags-location-stack-back)
  ("+" rtags-location-stack-forward)
  ("D" rtags-diagnostics)
  ("G" rtags-guess-function-at-point)
  ("p" rtags-set-current-project)
  ("P" rtags-print-dependencies)
  ("e" rtags-reparse-file)
  ("E" rtags-preprocess-file)
  ("R" rtags-rename-symbol)
  ("M" rtags-symbol-info)
  ("S" rtags-display-summary)
  ("O" rtags-goto-offset)
  (";" rtags-find-file)
  ("F" rtags-fixit)
  ("X" rtags-fix-fixit-at-point)
  ("B" rtags-show-rtags-buffer)
  ("I" rtags-imenu)
  ("T" rtags-taglist)
  ("M-q" nil "quit"))
(bind-keys :map c-mode-base-map
           ("C-:" . hydra-rtags/body))

;;;;;;;;;;
;; Keys ;;
;;;;;;;;;;
(mapc (lambda (x)
        (define-key c-mode-base-map
          (kbd (concat "C-c r " (car x))) (cdr x)))
      '(("m" . hydra-rtags/body)
        ("." . rtags-find-symbol-at-point)
        ("," . rtags-find-references-at-point)
        ("v" . rtags-find-virtuals-at-point)
        ("V" . rtags-print-enum-value-at-point)
        ("/" . rtags-find-all-references-at-point)
        ("Y" . rtags-cycle-overlays-on-screen)
        (">" . rtags-find-symbol)
        ("<" . rtags-find-references)
        ("-" . rtags-location-stack-back)
        ("+" . rtags-location-stack-forward)
        ("D" . rtags-diagnostics)
        ("G" . rtags-guess-function-at-point)
        ("p" . rtags-set-current-project)
        ("P" . rtags-print-dependencies)
        ("e" . rtags-reparse-file)
        ("E" . rtags-preprocess-file)
        ("R" . rtags-rename-symbol)
        ("M" . rtags-symbol-info)
        ("S" . rtags-display-summary)
        ("O" . rtags-goto-offset)
        (";" . rtags-find-file)
        ("F" . rtags-fixit)
        ("X" . rtags-fix-fixit-at-point)
        ("B" . rtags-show-rtags-buffer)
        ("I" . rtags-imenu)
        ("T" . rtags-taglist)))


(provide 'rtags-config)
;;; rtags-config.el ends here
