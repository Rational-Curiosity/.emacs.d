;;; multi-magit-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads nil "multi-magit" "multi-magit.el" (0 0 0 0))
;;; Generated autoloads from multi-magit.el

(autoload 'multi-magit-insert-uncommitted-changes "multi-magit" "\
Insert a diffstat of changes in worktree relative to HEAD." nil nil)

(autoload 'multi-magit-insert-committed-changes "multi-magit" "\
Insert a diffstat and commit log of commits since the
merge-base betweenn HEAD and @{upstream}." nil nil)

(autoload 'multi-magit-select-repository "multi-magit" "\
Select DIRECTORY's repository.

\(fn &optional DIRECTORY)" t nil)

(autoload 'multi-magit-unselect-repository "multi-magit" "\
Unselect DIRECTORY's repository.

\(fn &optional DIRECTORY)" t nil)

(autoload 'multi-magit-checkout "multi-magit" "\
Checkout BRANCH for each selected repository.

\(fn BRANCH)" t nil)

(autoload 'multi-magit-branch-delete "multi-magit" "\
Delete BRANCH for each selected repository.

\(fn BRANCH)" t nil)

(autoload 'multi-magit-git-command "multi-magit" "\
Execute COMMAND asynchronously for each selected repository.

Interactively, prompt for COMMAND in the minibuffer. \"git \" is
used as initial input, but can be deleted to run another command.

COMMAND is run in the top-level directory of each repository.

\(fn COMMAND)" t nil)

(autoload 'multi-magit-shell-command "multi-magit" "\
Execute COMMAND asynchronously for each selected repository.

Interactively, prompt for COMMAND in the minibuffer. COMMAND is
run in the top-level directory of each repository.

\(fn COMMAND)" t nil)

(autoload 'multi-magit-list-repositories "multi-magit" "\
Display a list of repositories for selection.

Use the options `magit-repository-directories' and
`magit-repository-directories-depth' to control which
repositories are displayed." t nil)

(autoload 'multi-magit-list-branches "multi-magit" "\
Display a list of branches in all repositories, selected or unselected.

Use the options `magit-repository-directories' and
`magit-repository-directories-depth' to control which
repositories are displayed." t nil)

(autoload 'multi-magit-status "multi-magit" nil t nil)

(autoload 'multi-magit-insert-repos-overview "multi-magit" "\
Insert sections for all selected repositories." nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "multi-magit" '("magit-multi-magit-repo-section-map" "multi-magit-")))

;;;***

(provide 'multi-magit-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; multi-magit-autoloads.el ends here
