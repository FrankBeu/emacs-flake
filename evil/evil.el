;;; evil


;;;; evil-hook

(setq evil-want-keybinding nil)
(defun fb/evil-hook ()
  (dolist (mode '(
		  custom-mode
		  eshell-mode
		  git-rebase-mode
		  term-mode
		  ))
    (add-to-list 'evil-emacs-state-modes mode)))


;;;; evil

(use-package evil
  :init
  (setq evil-want-C-i-jump nil
	;; evil-want-C-u-scroll t     ;; TODO shadows C-u universal argument
	evil-want-Y-yank-to-eol t
	evil-want-integration t
	)
  :config
  (add-hook 'evil-mode-hook 'fb/evil-hook)
  (evil-mode 1)
  :custom
  (evil-undo-system 'undo-tree)
  )


;;;; evil-collection
;; TEST: quit helpful buffer with q

;; WORKING??
;; (defun fb/hjkl-rotation (_mode mode-keymaps &rest _rest)
;;  (evil-collection-translate-key 'normal mode-keymaps
;;    "j" "h"
;;    "k" "j"
;;    "l" "k"
;;    ";" "l"
;;    "h" ";"
;;    ))

(use-package evil-collection
  :after evil
  :init
  (setq evil-want-keybinding nil)
  :config
  ;; (add-hook 'evil-collection-setup-hook #'fb/hjkl-rotation)
  (evil-collection-init)
  )


;;;; evil-commentary
;; deprecated cf. evil-nerd-commenter

;; (use-package evil-commentary
;;   :config
;;   (evil-commentary-mode)
;;   )


;;;; evil-escape
;; use fast fd to escape everything

(use-package evil-escape
  :config
  (evil-escape-mode)
  )


;;;; evil-nerd-commenter

(use-package evil-nerd-commenter
  :config
  (setq evilnc-invert-comment-line-by-line t)
  ;; (evilnc-default-hotkeys)
  )


;;;; evil-numbers

(use-package evil-numbers)


;;;; evil-surround

(use-package evil-surround
  :config
  (global-evil-surround-mode 1)
  )


;;;; undo-tree

(use-package undo-tree
  :config
  (global-undo-tree-mode 1)
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t) 
  (fb/tetest ())
  )
