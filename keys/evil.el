;;; evil


;;;; evil-hook

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
	evil-want-keybinding nil
	)
  :config
  (add-hook 'evil-mode-hook 'fb/evil-hook)
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
  (define-key evil-normal-state-map (kbd "h") 'evil-repeat-find-char)


  ;; jkl;
  (define-key evil-motion-state-map "j" 'evil-backward-char)
  (define-key evil-motion-state-map "k" 'evil-previous-visual-line)
  (define-key evil-motion-state-map "l" 'evil-next-visual-line)
  (define-key evil-motion-state-map ";" 'evil-forward-char)
  ;; Also in visual mode
  (define-key evil-visual-state-map "k" 'evil-previous-visual-line)
  (define-key evil-visual-state-map "l" 'evil-next-visual-line)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
  (evil-global-set-key 'motion "l" 'evil-next-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
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
  ;; :config
  ;; (evilnc-default-hotkeys)
  )


;;;; evil-numbers

(use-package evil-numbers)

;;;; evil-surround

(use-package evil-surround
  :config
  (global-evil-surround-mode 1)
  )
