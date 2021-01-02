;;; keybindings


;;;; avy

(general-define-key
 "C-'" 'avy-goto-word-0
 )


;;;; orgmode

;; SRC: https://orgmode.org/manual/Activation.html#Activation ;; equivalent to the next expression
;; (global-set-key (kbd "C-c l") 'org-store-link)
;; (global-set-key (kbd "C-c a") 'org-agenda)
;; (global-set-key (kbd "C-c c") 'org-capture)
(general-define-key
 :prefix "C-c"
 "l" 'org-store-link
 "a" 'org-agenda
 "c" 'org-capture
 )


;;;; treemacs

(eval-after-load "treemacs-evil"
  '(progn
     (general-define-key
      :keymaps '(evil-treemacs-state-map treemacs-mode-map)
      "h" 'evil-forward-char
      "j" 'treemacs-root-up
      "k" 'treemacs-previous-line
      "l" 'treemacs-next-line
      ";" 'treemacs-root-down
      )

     (general-define-key
      :keymaps 'treemacs-mode-map
      :states 'treemacs
      "l" 'nil
      )

     (general-define-key
      :keymaps 'treemacs-mode-map
      :states 'treemacs
      "h" 'evil-forward-char
      "j" 'treemacs-root-up
      "k" 'treemacs-previous-line
      "l" 'treemacs-next-line
      ";" 'treemacs-root-down
      )))
;;;; ESC
;;   on prompts: use ESC like C-g

;;;;; on prompts: use ESC like C-g
;; (global-set-key (kbd "<escape>") 'keyboard-escape-quit) ;; equivalent to the next expression
(general-define-key
 "<escape>" 'keyboard-escape-quit
 )


;;;; SPC

(fb/leader-key-SPC
  "c"  '(:ignore t                                     :which-key "comment")
  "cc" '(evilnc-comment-operator                       :which-key "cmnt-operator")
  "ci" '(evilnc-toggle-invert-comment-line-by-line     :which-key "toggle-invert-cmnt-line-by-line")
  "cl" '(evilnc-comment-or-uncomment-lines             :which-key "cmmnt-or-uncmnt-lines")
  "cp" '(evilnc-comment-or-uncomment-paragraphs        :which-key "cmmnt-or-uncmnt-paragraphs")
  "cr" '(comment-or-uncomment-region                   :which-key "cmmnt-or-uncmnt-region")
  "ct" '(evilnc-quick-comment-or-uncomment-to-the-line :which-key "quick-cmmnt-or-uncmnt-to-the-line")
  "cy" '(evilnc-copy-and-comment-lines                 :which-key "cp-and-cmnt-lines")

  "d"  '(:ignore t                  :which-key "delete")
  "dw" '(delete-trailing-whitespace :which-key "trailing-wsp")

  "f"  '(:ignore t                  :which-key "fast")
  "fs" '(save-buffer                :which-key "save-buffer")

  "g"  '(:ignore t                  :which-key "git")
  "gs" '(magit-status               :which-key "status")

  "n"  '(:ignore t                  :which-key "numbers")
  "n=" '(evil-numbers/inc-at-pt     :which-key "add")
  "n+" '(evil-numbers/inc-at-pt     :which-key "add")
  "n-" '(evil-numbers/dec-at-pt     :which-key "sub")

  "p"  '(projectile-command-map     :which-key "projectile")

  "r"  '(fb/reload-config           :which-key "reload init.el")

  "t"  '(:ignore t                  :which-key "toggles")
  "T"  '(:ignore t                  :which-key "toggles")
  "tl" '(toggle-truncate-lines      :which-key "truncate-lines")
  "tn" '(neotree-toggle             :which-key "neotree-toggle")
  "TN" '(display-line-numbers-mode  :which-key "line-numbers")
  "tt" '(counsel-load-theme         :which-key "choose theme")
  "tw" '(whitespace-mode            :which-key "whitespace")

  )


;;;; INFO

;;;;; unbind key
;; M-x global-unset-key

;;;;; general
;; https://github.com/noctuid/general.el#general-examples
