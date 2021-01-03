;;; keybindings


;;;; avy

(general-define-key
 "C-'" 'avy-goto-word-0
 )


;;;; dired

(eval-after-load "dired-rifle"
  (evil-collection-define-key 'normal 'dired-mode-map
    "j"         'dired-single-up-directory
    ";"         'dired-single-buffer
    "r"         'dired-rifle
    (kbd "h d") 'epa-dired-do-decrypt 
    (kbd "h e") 'epa-dired-do-encrypt 
    (kbd "h s") 'epa-dired-do-sign
    (kbd "h v") 'epa-dired-do-verify
    ))


;;;; magit

(eval-after-load "evil-magit"
  '(progn

     (general-define-key
      :keymaps '(magit-mode-map)
      :states '(normal visual)
      "j" 'nil
      )

     (general-define-key
      :keymaps '(magit-status-mode-map)
      "j" 'nil
      )

     (general-define-key
      :keymaps '(magit-status-mode-map)
      :states '(normal visual)
      "h" 'magit-log
      )

     (general-define-key
      :keymaps 'magit-mode-map
      "h" 'magit-log
      "H" 'magit-log
      "j" 'evil-backward-char
      "k" 'evil-previous-visual-line
      "l" 'evil-next-visual-line
      ;; ";" 'evil-forward-char
      "J" 'magit-status-jump
      )))


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


;;;; ESC
;;   on prompts: use ESC like C-g

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

  "d"  '(:ignore t                       :which-key "delete")
  "dw" '(delete-trailing-whitespace      :which-key "trailing-wsp")

  "f"  '(:ignore t                       :which-key "fast")
  "ff" '(counsel-find-file               :which-key "files")
  "fs" '(save-buffer                     :which-key "save-buffer")

  "g"  '(:ignore t                       :which-key "git")
  "gs" '(magit-status                    :which-key "status")

  "j"  '(dired-jump                      :which-key "dired")

  "n"  '(:ignore t                       :which-key "numbers")
  "n=" '(evil-numbers/inc-at-pt          :which-key "add")
  "n+" '(evil-numbers/inc-at-pt          :which-key "add")
  "n-" '(evil-numbers/dec-at-pt          :which-key "sub")

  "p"  '(projectile-command-map          :which-key "projectile")

  "r"  '(fb/reload-config                :which-key "reload init.el")

  "t"  '(:ignore t                       :which-key "toggles")
  "T"  '(:ignore t                       :which-key "toggles")
  "ti" '(imenu-list-smart-toggle         :which-key "imenu")
  "tl" '(toggle-truncate-lines           :which-key "truncate-lines")
  ;; "tn" '(neotree-toggle                  :which-key "neotree-toggle")
  "tn" '(display-line-numbers-mode       :which-key "line-numbers")
  "tt" '(counsel-load-theme              :which-key "choose theme")
  "tw" '(whitespace-mode                 :which-key "whitespace")
  "TW" '(fb/toggle-which-key-sort-order  :which-key "whickKey-sort-order")
  )


;;;; INFO

;;;;; unbind key
;; M-x global-unset-key

;;;;; general
;; https://github.com/noctuid/general.el#general-examples
