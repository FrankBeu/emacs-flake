;;; packages


;;;; all-the-icons

(use-package all-the-icons)


;;;; command-log

(use-package command-log-mode)


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


;;;; .nix

(use-package nix-mode
  :mode "\\.nix\\'")


;;;; rainbow-delimiters

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  )


;;;; which-key

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq
   which-key-idle-delay 0.5
   ;; which-key-sort-order 'which-key-key-order-alpha
   which-key-sort-order 'which-key-description-order
   )
  )
