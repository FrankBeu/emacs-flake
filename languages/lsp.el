;;; lsp



(defun fb/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

;;; lsp-mode

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . fb/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; or 'c-l', 's-l'
  :config
  (lsp-enable-which-key-integration t)
  )


;; (use-package lsp-ui :commands lsp-ui-mode)
;; (use-package lsp-ui
;;   :hook (lsp-mode . lsp-ui-mode)
;;   :commands lsp-ui-mode
;;   :custom
;;   (lsp-ui-doc-position 'bottom))


;;;; lsp-ivy

;; (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)


;;;; lsp-treemacs

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list
  :after lsp
  )
;; (use-package lsp-treemacs
;;   :after lsp)


;;;; lsp-treemacs

;; (use-package lsp-ivy)


;;;; dap-mode

;; (use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language
