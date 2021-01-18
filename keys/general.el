;;; general.el

(use-package general
  :config
  (general-create-definer fb/leader-key
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  ;; homerow
  (general-create-definer fb/local-leader-key
    :keymaps '(normal insert visual emacs)
    :prefix "SPC k"
    :global-prefix "C-SPC k")
  )
