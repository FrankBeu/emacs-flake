;;; orgmode-keybindings

;; prefix not working
(general-define-key
 :keymaps 'org-mode-map
 "C-'" 'nil
 )

(general-define-key
 :keymaps 'org-mode-map
 "C-'" 'nil
"M-<return>"   'fb/org-meta-return
"M-S-<return>" 'org-insert-todo-subheading
 )

;; (with-eval-after-load "org"
;;      (define-key org-mode-map (kbd "M-<return>") 'fb/org-meta-return)
;;      (define-key org-mode-map (kbd "M-S-<return>") 'org-insert-todo-subheading)
;;       )

(fb/local-leader-key
  :keymaps 'org-mode-map
  :states  '(normal visual)
  "t"      '(                                             :which-key "todo" :ignore t)
  "tc"     '(org-todo                                     :which-key "cycle")
  "t SPC"  '(org-todo                                     :which-key "cycle")
  "tt"     '((lambda () (interactive)(org-todo 'todo))    :which-key "todo" )
  "td"     '((lambda () (interactive)(org-todo 'done))    :which-key "done" )
  "tx"     '((lambda () (interactive)(org-todo 'none))    :which-key "none" )

  ;; cf.: rg org-emphasize ~/SRC/GITHUB/spacemacs
  "x"      '(                                             :which-key "text" :ignore t)
  "xb"     '((lambda () (interactive)(org-emphasize ?\*)) :which-key "bold"          )
  "xc"     '((lambda () (interactive)(org-emphasize ?\~)) :which-key "code"          )
  "xi"     '((lambda () (interactive)(org-emphasize ?\/)) :which-key "italic"        )
  "xr"     '((lambda () (interactive)(org-emphasize ?\ )) :which-key "clear"         )
  ;; TODO clearing not working just inserts " " arround region
  "xR"     '((lambda () (interactive)(org-emphasize ?\s)) :which-key "clear"         )
  "xs"     '((lambda () (interactive)(org-emphasize ?\+)) :which-key "strike-through")
  "xu"     '((lambda () (interactive)(org-emphasize ?\_)) :which-key "underline"     )
  "xv"     '((lambda () (interactive)(org-emphasize ?\=)) :which-key "verbatim"      )
  )


;; https://orgmode.org/manual/The-date_002ftime-prompt.html
;; minibuffer date
(general-define-key
 :keymaps 'org-read-date-minibuffer-local-map
 ;; "C-H"    'exit-minibuffer
 "C-j"    'nil

 "C-j"     '(lambda () (interactive) (org-eval-in-calendar '(calendar-backward-day   1)))
 "C-k"     '(lambda () (interactive) (org-eval-in-calendar '(calendar-backward-week  1)))
 "C-l"     '(lambda () (interactive) (org-eval-in-calendar '(calendar-forward-week   1)))
 "C-;"     '(lambda () (interactive) (org-eval-in-calendar '(calendar-forward-day    1)))

 ;; SHIFT or META are the same
 "C-S-j"   '(lambda () (interactive) (org-eval-in-calendar '(calendar-backward-month 1)))
 "C-S-k"   '(lambda () (interactive) (org-eval-in-calendar '(calendar-backward-year  1)))
 "C-S-l"   '(lambda () (interactive) (org-eval-in-calendar '(calendar-forward-year   1)))
 "C-:"     '(lambda () (interactive) (org-eval-in-calendar '(calendar-forward-month  1)))
 "C-M-j"   '(lambda () (interactive) (org-eval-in-calendar '(calendar-backward-month 1)))
 "C-M-k"   '(lambda () (interactive) (org-eval-in-calendar '(calendar-backward-year  1)))
 "C-M-l"   '(lambda () (interactive) (org-eval-in-calendar '(calendar-forward-year   1)))
 "C-M-;"   '(lambda () (interactive) (org-eval-in-calendar '(calendar-forward-month  1)))

 ;; scrolling with CTRL + SHIFT + META
 "C-M-S-j" '(lambda () (interactive) (org-eval-in-calendar '(calendar-scroll-right              1)))
 "C-M-S-k" '(lambda () (interactive) (org-eval-in-calendar '(calendar-scroll-right-three-months 1)))
 "C-M-S-l" '(lambda () (interactive) (org-eval-in-calendar '(calendar-scroll-left-three-months  1)))
 "C-M-:"   '(lambda () (interactive) (org-eval-in-calendar '(calendar-scroll-left               1)))
 )
