;;; version-control-config.el --- Configure version control

;;; Commentary:

;; Usage:
;; (require 'version-control-config)

;;; Code:
(message "Importing version-control-config")
(require 'frames-windows-buffers-config)

;; Refresh mode line branch
(with-eval-after-load 'magit-mode
  (defun vc-refresh-buffers ()
    (interactive)
    (dolist (buffer (buffers-from-file))
      (with-current-buffer buffer
        (vc-refresh-state))))
  (advice-add 'magit-refresh :after 'vc-refresh-buffers))

(with-eval-after-load 'magit-status
  (define-key magit-status-mode-map (kbd "z") #'avy-goto-char-timer))
(with-eval-after-load 'magit-process
  (define-key magit-process-mode-map (kbd "z") #'avy-goto-char-timer))


;; SMerge hydra menu
(defhydra hydra-smerge
  (:foreign-keys run :hint nil :pre (smerge-mode 1))
  "
^Move^     ^Keep^     ^Diff^       ^Pair^
^^^^^^^^---------------------------------------------
_C-n_ext   _C-b_ase   _C-r_efine   _C-<_: base-upper
_C-p_rev   _C-u_pper  _C-e_diff    _C-=_: upper-lower
^   ^      _C-l_ower  _C-c_ombine  _C->_: base-lower
^   ^      _C-a_ll    _C-r_esolve
"
  ("C-RET" smerge-keep-current "current")
  ("C-c"   smerge-combine-with-next)
  ("C-e"   smerge-ediff)
  ("C-r"   smerge-refine)
  ("C-a"   smerge-keep-all)
  ("C-b"   smerge-keep-base)
  ("C-u"   smerge-keep-upper)
  ("C-n"   smerge-next)
  ("C-l"   smerge-keep-lower)
  ("C-p"   smerge-prev)
  ("C-r"   smerge-resolve)
  ("C-<"   smerge-diff-base-upper)
  ("C-="   smerge-diff-upper-lower)
  ("C->"   smerge-diff-base-lower)
  ("M-q" nil "quit"))
(bind-keys :map smerge-mode-map
           ("C-c m m" . hydra-smerge/body))


(provide 'version-control-config)
;;; version-control-config.el ends here
