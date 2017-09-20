(defun projectile-project-vcs (&optional project-root)
  "Determine the VCS used by the project if any.
PROJECT-ROOT is the targeted directory.  If nil, use
`projectile-project-root'."
  (or project-root (setq project-root (projectile-project-root)))
  (cond
   ;; ((projectile-file-exists-p (expand-file-name ".git" project-root)) 'git)
   ;; ((projectile-file-exists-p (expand-file-name ".hg" project-root)) 'hg)
   ;; ((projectile-file-exists-p (expand-file-name ".fossil" project-root)) 'fossil)
   ;; ((projectile-file-exists-p (expand-file-name ".bzr" project-root)) 'bzr)
   ;; ((projectile-file-exists-p (expand-file-name "_darcs" project-root)) 'darcs)
   ;; ((projectile-file-exists-p (expand-file-name ".svn" project-root)) 'svn)
   ;; ((projectile-locate-dominating-file project-root ".git") 'git)
   ;; ((projectile-locate-dominating-file project-root ".hg") 'hg)
   ;; ((projectile-locate-dominating-file project-root ".fossil") 'fossil)
   ;; ((projectile-locate-dominating-file project-root ".bzr") 'bzr)
   ;; ((projectile-locate-dominating-file project-root "_darcs") 'darcs)
   ;; ((projectile-locate-dominating-file project-root ".svn") 'svn)
   (t 'none)))

(provide 'projectile-bug)