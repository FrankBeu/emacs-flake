

(general-define-key
 :keymaps '(evil-normal-state-map)
 ;; :states  '(normal visual)
 "h"   'evil-repeat-find-char
 )

;; jkl;
(general-define-key
 :keymaps '(evil-motion-state-map)
 ;; :states  '(normal visual)
 "j" 'evil-backward-char
 "k" 'evil-previous-visual-line
 "l" 'evil-next-visual-line
 ";" 'evil-forward-char
 )

;; Also in visual mode
(general-define-key
 :keymaps '(evil-visual-state-map)
 "k" 'evil-previous-visual-line
 "l" 'evil-next-visual-line
 )

;; (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
;; (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)
;; (define-key evil-normal-state-map (kbd "h") 'evil-repeat-find-char)


;; jkl;
;; (define-key evil-motion-state-map "j" 'evil-backward-char)
;; (define-key evil-motion-state-map "k" 'evil-previous-visual-line)
;; (define-key evil-motion-state-map "l" 'evil-next-visual-line)
;; (define-key evil-motion-state-map ";" 'evil-forward-char)
;; ;; Also in visual mode
;; (define-key evil-visual-state-map "k" 'evil-previous-visual-line)
;; (define-key evil-visual-state-map "l" 'evil-next-visual-line)

;; Use visual line motions even outside of visual-line-mode buffers

;; `general-def' can be used instead for `evil-global-set-key'-like syntax
(general-def 'motion
  "k" 'evil-previous-visual-line
  "l" 'evil-next-visual-line
  )

;; (evil-global-set-key 'motion "k" 'evil-previous-visual-line)
;; (evil-global-set-key 'motion "l" 'evil-next-visual-line)

(evil-set-initial-state 'messages-buffer-mode 'normal)
(evil-set-initial-state 'dashboard-mode 'normal)

;;;; avy

(general-define-key
 "C-'"  'avy-goto-word-0
 "C-\"" 'avy-goto-line
 )


;;;; dired

(general-define-key
 :keymaps '(dired-mode-map)
 :states  '(normal visual)
 ;; "j" 'nil
 ";"      'nil
 )

(general-define-key
 :keymaps '(dired-mode-map)
 :states  '(normal visual)
 ;; "j" 'nil
 "H"      'dired-hide-dotfiles-mode
 ";"      'dired-find-file
 "j"      'dired-single-up-directory
 "r"      'dired-rifle
 )

(general-define-key
 :keymaps '(dired-mode-map)
 :states  '(normal visual)
 :prefix  "g"
 "R"      'dired-do-redisplay
 )
(general-define-key
 :keymaps '(dired-mode-map)
 :states  '(normal visual)
 :prefix  "h"
 "d"      'epa-dired-do-decrypt
 "e"      'epa-dired-do-encrypt
 "s"      'epa-dired-do-sign
 "v"      'epa-dired-do-verify
 )


;;;; magit

(general-define-key
 :keymaps '(magit-mode-map)
 :states  '(normal visual)
 "j" 'nil
 )

(general-define-key
 :keymaps '(magit-status-mode-map)
 "j" 'nil
 )

(general-define-key
 :keymaps '(magit-status-mode-map)
 :states  '(normal visual)
 "h" 'magit-log
 )

(general-define-key
 :keymaps 'magit-mode-map
 "h" 'magit-log
 "H" 'magit-log
 "j" 'evil-backward-char
 ;; "k" 'evil-previous-visual-line
 "l" 'evil-next-visual-line
 ;; ";" 'evil-forward-char
 "J" 'magit-status-jump
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


;;;; orgmode

(general-define-key
 :prefix "C-c"
 "L" 'org-store-link
 ;; "l" 'org-store-link
 "a" 'org-agenda
 "c" 'org-capture
 )


;;;; ESC
;;   on prompts: use ESC like C-g
(general-define-key
 "<escape>" 'keyboard-escape-quit
 )


;;;; UNDO-TREE TODO

;; (evil-make-overriding-map undo-tree-visualizer-mode-map 'normal)
;; asdfasdf asdfasdf qweqwer adsfasdf adsfasdf qeqwer

;;(add-hook undo-tree-visualizer-mode-hook (define-key undo-tree-visualizer-mode-map "k" 'undo-tree-visualize-undo))
;; (general-define-key
;;  :keymaps '(undo-tree-visualizer-mode
;; 	    ;; undo-tree
;; 	    )

;; ;; undo-tree-visualizer-mode-map <down>
;; ;; undo-tree-visualizer-mode-map C-n
;; ;; undo-tree-visualizer-mode-map n

;; ;; undo-tree-visualizer-mode-map <up>
;; ;; undo-tree-visualizer-mode-map C-p
;; ;; undo-tree-visualizer-mode-map p

;;  ;; "j" 'nil
;;  "k" 'nil
;;  ;; "l" 'nil
;;  ;; ";" 'nil
;;  ;; "j" 'undo-tree-visualize-switch-branch-left        ;;; working
;;  ;; "j" 'undo-tree-visualize-switch-branch-left        ;;; working
;;  "k" 'undo-tree-visualize-undo
;;  ;; "l" 'undo-tree-visualize-redo
;;  ;; ";" 'undo-tree-visualize-switch-branch-right       ;;; working
;;  ;; Ctr-{p,n} working
;;  )
;; )
;; https://emacs.stackexchange.com/questions/44431/how-to-suppress-a-minor-modes-key-binding-in-only-certain-major-modes
;; (add-hook 'typo-mode-hook
;;           (lambda ()
;;             (when (and typo-mode (derived-mode-p 'markdown-mode))
;;               (let ((map (make-sparse-keymap)))
;;                 (set-keymap-parent map typo-mode-map)
;;                 (define-key map (kbd "`") 'self-insert-command)
;;                 (push `(typo-mode . ,map)
;;                       minor-mode-overriding-map-alist)))))



;; evil-previous-visual-line
;;;; qwer qwer  adsf jkl;     qwerqwer  qwerqwer asdf asdfasdqwerqweradasdfd      dfffsadasd qewrqwer

;;;; SPC

(fb/leader-key
  "c"  '(                                                   :which-key "comment"                          :ignore t)
  "cc" '(evilnc-comment-operator                            :which-key "cmnt-operator"                    )
  "ci" '(evilnc-toggle-invert-comment-line-by-line          :which-key "toggle-invert-cmnt-line-by-line"  )
  "cl" '(evilnc-comment-or-uncomment-lines                  :which-key "cmmnt-or-uncmnt-lines"            )
  "cp" '(evilnc-comment-or-uncomment-paragraphs             :which-key "cmmnt-or-uncmnt-paragraphs"       )
  "cr" '(comment-or-uncomment-region                        :which-key "cmmnt-or-uncmnt-region"           )
  "ct" '(evilnc-quick-comment-or-uncomment-to-the-line      :which-key "quick-cmmnt-or-uncmnt-to-the-line")
  "cy" '(evilnc-copy-and-comment-lines                      :which-key "cp-and-cmnt-lines"                )

  "d"  '(                                                   :which-key "delete"                           :ignore t)
  "dw" '(delete-trailing-whitespace                         :which-key "trailing-wsp"                     )

  "f"  '(                                                   :which-key "fast/file"                        :ignore t)
  "fy" '(fb/yank-buffer-filename                            :which-key "files"                            )
  "ff" '(counsel-find-file                                  :which-key "files"                            )
  "fs" '(save-buffer                                        :which-key "save-buffer"                      )
  "fS" '(save-some-buffers                                  :which-key "save-some-buffer"                 )

  "g"  '(                                                   :which-key "git"                              :ignore t)
  "gs" '(magit-status                                       :which-key "status"                           )

  "j"  '(dired-jump                                         :which-key "dired"                            )

  "L"  '(lsp                                                :which-key "start lsp"                        )
  "l"  '(:keymap lsp-command-map :package lsp-mode          :which-key "lsp"                              )


  "n"  '(                                                   :which-key "numbers"                          :ignore t)
  "n=" '(evil-numbers/inc-at-pt                             :which-key "add"                              )
  "n+" '(evil-numbers/inc-at-pt                             :which-key "add"                              )
  "n-" '(evil-numbers/dec-at-pt                             :which-key "sub"                              )

  "p"  '(projectile-command-map                             :which-key "projectile"                       )

  "r"  '(                                                   :which-key "re-~"                             :ignore t)
  "rr" '(redraw-display                                     :which-key "redraw-display"                   )
  "rl" '(fb/reload-config                                   :which-key "reload init.el"                   )

  "t"  '(                                                   :which-key "toggles"                          :ignore t)
  "ti" '(imenu-list-smart-toggle                            :which-key "imenu"                            )
  "tl" '(toggle-truncate-lines                              :which-key "truncate-lines"                   )
  "tn" '(display-line-numbers-mode                          :which-key "line-numbers"                     )
  "tt" '(counsel-load-theme                                 :which-key "choose theme"                     )
  "tw" '(whitespace-mode                                    :which-key "whitespace"                       )
  "T"  '(                                                   :which-key "toggles"                          :ignore t)
  "TW" '(fb/toggle-which-key-sort-order                     :which-key "whickKey-sort-order"              )

  "y"  '(                                                   :which-key "yasnippets"                       :ignore t)
  "yy" '(yas-insert-snippet                                 :which-key "insert"                           )
  "yr" '(yas-reload-all                                     :which-key "reload-all"                       )

  "w" '(writeroom-mode                                     :which-key "writeroom-toggle"                  )
  )

;;; prefix will stack
;;; which-key not working
(general-define-key
 :keymaps '(writeroom-mode-map)
 "s-?"  'nil
 "M-m"   '(writeroom-toggle-mode-line :which-key "toggle-modeline")
 "C-M-<" 'writeroom-decrease-width
 "C-M->" 'writeroom-increase-width
 ;; "C-M-=" 'writeroom-adjust-width
 "C-M-=" '(writeroom-adjust-width :which-key "wr-with-=")
 )

;;; imenu
(general-define-key
 :keymaps '(imenu-list-major-mode-map)
 "<C-return>" 'imenu-list-display-entry
 "M-RET"      'imenu-list-display-entry
 )

;; ivy
(general-define-key
 :keymaps '(counsel-ag-map
	    counsel-git-grep-map
	    counsel-grep-map
	    counsel-imenu-map
	    )
 "C-l" 'nil
 "C-l" 'ivy-next-line
 "C-S-l" 'ivy-call-and-recenter
 )

;;;; INFO

;;;;; unbind key
;; M-x global-unset-key

;;;;; general
;; https://github.com/noctuid/general.el#general-examples
