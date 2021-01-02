;;; projectile


;;;; projectile

(use-package projectile
  :init
  (when (file-directory-p "~/SRC/GITEA")
    (setq projectile-project-search-path '("~/SRC/GITEA")))
  ;; (setq projectile-known-projects-file (expand-file-name ".state/projectile-bookmarks.eld" user-emacs-directory))
  ;; (setq projectile-switch-project-action #'projectile-dired)
  (setq projectile-switch-project-action 'neotree-projectile-action)
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  )


;;;; counsel-projectile

(use-package counsel-projectile
  :config (counsel-projectile-mode))
