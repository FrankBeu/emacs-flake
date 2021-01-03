;;; packages


;;;; all-the-icons

(use-package all-the-icons)


;;;; avy

(use-package avy)


;;;; command-log

(use-package command-log-mode)


;;;; dired

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  )


;;;;; all-the-icons-dired

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))


;;;;; dired-git-info
;; TODO use after dired-k

;; (use-package dired-git-info
;;   :after dired
;;   :hook (dired-after-readin . dired-git-info-auto-enable)
;;   :config
;;   (setq dgi-auto-hide-details-p nil)
;;   )


;;;;; dired-hide-dotfiles

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "H" 'dired-hide-dotfiles-mode))


;;;;; dired-k
;; TODO fix colorscheme and icons

;; (use-package dired-k
;;   :after dired
;;   :hook ((dired-initial-position . dired-k)
;; 	 ;; (dired-after-readin     . #'dired-k-no-revert)
;; 	 )
;;   :config
;;   ;; (setq dired-k-style nil)
;;   (setq dired-k-style 'git)
;;   ;; (setq dired-k-human-readable nil)
;;   (setq dired-k-human-readable t)
;;   (setq dired-k-padding 1)
;;   )


;;;;; dired-rifle

(use-package dired-rifle
  :after dired
  )


;;;;; dired-single

(use-package dired-single)


;;;; fira-code-mode

(use-package fira-code-mode
  ;; :config (global-fira-code-mode) ;; will not work with orgmode headline-stars
  :hook prog-mode
  )


;;;; helpful

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key)
  )


;;;; imenu

(use-package imenu-list
  :config (imenu-list-minor-mode)
  )


;; ;;;; neotree

;; (use-package neotree
;;   :config
;;   (setq neo-smart-open t)
;;   (setq neo-autorefresh t)
;;   (setq neo-vc-integration '(face))
;;   )


;;;; no-littering

(use-package no-littering
  :init
  (setq no-littering-etc-directory (expand-file-name ".state/config/" user-emacs-directory)
        no-littering-var-directory (expand-file-name ".state/data/"   user-emacs-directory))
  :config
  (setq
   auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
   backup-by-copying t
   delete-old-versions t          ;;; delete excess backup versions silently
   kept-new-versions 128
   kept-old-versions 0
   make-backup-files t
   vc-follow-symlinks t           ;;; don't ask for confirmation when opening symlinked file under vc
   vc-make-backup-files t         ;;; make backups file even when in version controlled dir
   version-control t              ;;; use version control
   )
  )


;;;; rainbow-delimiters

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  )


;;;; ripgrep

(use-package ripgrep)


;;;; treemacs

(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemaca-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".state/data/treemacs/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-read-string-input             'from-child-frame
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         35
          treemacs-workspace-switch-cleanup      nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    ;; (treemacs-follow-mode nil)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag))
  ;; :general
  ;; (:keymaps 'treemacs-mode-map
  ;; :states 'treemacs
  ;; "l" 'nil)
  ;; :after general
  )

(use-package treemacs-all-the-icons
  :after treemacs all-the-icons
  :config
  (treemacs-load-theme "all-the-icons")
  )

(use-package treemacs-evil
  :after treemacs evil)

(use-package treemacs-projectile
  :after treemacs projectile)

(use-package treemacs-icons-dired
  :after treemacs dired
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit)

(use-package treemacs-persp ;;treemacs-persective if you use perspective.el vs. persp-mode
  :after treemacs persp-mode ;;or perspective vs. persp-mode
  :config (treemacs-set-scope-type 'Perspectives))


;;;; which-key

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq
   which-key-idle-delay 0.5
   which-key-max-description-length nil
   which-key-allow-imprecise-window-fit t
   ;; which-key-sort-order 'which-key-key-order-alpha
   which-key-sort-order 'which-key-description-order
   )
  )
