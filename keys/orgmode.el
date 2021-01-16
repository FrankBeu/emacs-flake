;;; orgmode-keybindings

;; prefix not working
(general-define-key
 :keymaps 'org-mode-map
 "C-'" 'nil
 )

;; (with-eval-after-load "org"
;;      (define-key org-mode-map (kbd "M-<return>") 'fb/org-meta-return)
;;      (define-key org-mode-map (kbd "M-S-<return>") 'org-insert-todo-subheading)
;;       )
