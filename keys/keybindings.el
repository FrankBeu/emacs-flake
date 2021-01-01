;;; keybindings

;;;; orgmode

;; SRC: https://orgmode.org/manual/Activation.html#Activation
;; (global-set-key (kbd "C-c l") 'org-store-link)
;; (global-set-key (kbd "C-c a") 'org-agenda)
;; (global-set-key (kbd "C-c c") 'org-capture)
(general-define-key
  :prefix "C-c"
  "l" 'org-store-link
  "a" 'org-agenda
  "c" 'org-capture
  )

;;;; ESC

;;;;; on prompts: use ESC like C-g
;; (global-set-key (kbd "<escape>") 'keyboard-escape-quit) ;; equivalent to the next expression
(general-define-key "<escape>" 'keyboard-escape-quit)


;;;; SPC

(fb/leader-key-SPC
  "t"  '(:ignore t :which-key "toggles")
  "tt" '(counsel-load-theme :which-key "choose theme")
  "r"  '(fb/reload-config :which-key "reload init.el")
  )


;;;; INFO

;;;;; unbind key
;; M-x global-unset-key

;;;;; general
;; https://github.com/noctuid/general.el#general-examples
