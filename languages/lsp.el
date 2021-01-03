;;; lsp


;; (setq lsp-keymap-prefix "<SPC> l")
;;   (setq lsp-keymap-prefix "C-c l")
;;   :config
;;   ;; (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)

(defun fb/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

;;; lsp-mode
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . fb/lsp-mode-setup)
  ;;;; WORKING
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  ;;;; WORKING_END
  ;; (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  ;; :init
  ;; (setq lsp-keymap-prefix (kbd "<SPC> l"))  ;; Or 'C-l', 's-l'
  ;; (setq lsp-command-map (kbd "<SPC> l"))  ;; Or 'C-l', 's-l'

  ;;  ;; :init
  ;;  (general-define-key
  ;;  :prefix "SPC"
  ;;  "l" 'lsp-command-map)
    ;; (evil-define-key 'normal lsp-mode-map (kbd "SPC l") lsp-command-map)
  ;; (define-key lsp-mode-map (kbd "SPC l") lsp-command-map)
;;  :init
;;  (lsp-command-map "SPC" leader-map)
  ;; (fb/leader-key-SPC "l" '(lsp-command-map :which-key "lsp"))
  ;; (setq lsp-keymap-prefix (kbd "SPC l"))  ;; Or 'C-l', 's-l'
;;  :init
  ;;(with-eval-after-load 'evil
    ;;(evil-define-key '(normal visual) 'lsp-mode
      ;;(kbd "SPC l") lsp-command-map)
;;    (evil-normalize-keymaps))
  :config
  (lsp-enable-which-key-integration t)
  )


;;  (fb/leader-key-SPC "l" '(lsp-command-map :which-key "lsp"))
;;  (fb/leader-key-SPC "l" '('lsp-keymap-prefix :which-key "lsp"))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;; lsp-ui

;; (use-package lsp-ui :commands lsp-ui-mode)
;; (use-package lsp-ui
;;   :hook (lsp-mode . lsp-ui-mode)
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
