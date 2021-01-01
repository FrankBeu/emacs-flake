;;; general.el

(use-package general
  :config
  (general-create-definer fb/leader-key-SPC
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  )
