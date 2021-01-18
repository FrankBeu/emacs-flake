;;; orgmode-misc

;;;; font-size-heading
;; (custom-set-faces
;;  '(org-level-1 ((t (:inherit outline-1 :height 1.5))))
;;  '(org-level-2 ((t (:inherit outline-2 :height 1.25))))
;;  '(org-level-3 ((t (:inherit outline-3 :height 1.175))))
;;  '(org-level-4 ((t (:inherit outline-4 :height 1.1))))
;;  '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
;;  )

;;;; leading stars
(setq org-startup-indented t)

;;;; *BOLD*
(setq org-hide-emphasis-markers t)

(setq org-ellipsis
      ;; " ▾"
      ;; " ▽"
      "  ▼"
      ;; "  ◦◦◦"
      )

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars t)
  (org-superstar-headline-bullets-list
   ;; '("◉" "○" "●" "○" "●" "○" "●")
   ;; '("●" "◉" "○" "●" "◉" "○" "●")
   ;; '("●")
   ;; '("◉")
   '("○")
   )
  (org-superstar-item-bullet-alist
   '(
     (?- . ?•)
     (?+ . ?➤)
     ;; (?+ . ?▶)
     ;; (?+ . ?▷)
     ;; (?+ . ?▸)
     ;; (?+ . ?▹)
     ;; (?+ . ?►)
     ;; (?+ . ?▻)
     ;; (?+ . ?◉)
     ;; (?+ . ?○)
     ;; (?+ . ?◌)
     ;; (?+ . ?◍)
     ;; (?+ . ?◎)
     ;; (?+ . ?●)
     (?* . ?•)
     )
   )

  )

;; Increase the size of various headings
(set-face-attribute 'org-document-title nil :font "Roboto" :weight 'bold :height 2)
(dolist (face '((org-level-1 . 1.75)
                (org-level-2 . 1.5)
                (org-level-3 . 1.25)
                (org-level-4 . 1.175)
                (org-level-5 . 1.15)
                (org-level-6 . 1.1)
                (org-level-7 . 1.05)
                (org-level-8 . 1.05)))
  (set-face-attribute (car face) nil :font "Roboto" :weight 'regular :height (cdr face)))


;; Ensure that anything that should be fixed-pitch in Org files appears that way
(with-eval-after-load 'org-indent
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-table nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-formula nil  :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-indent nil :inherit '(org-hide fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  )

;; M-x describe-face org-

;;; agenda
(setq org-agenda-files '("~/NOTES"))
(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)


;;; structure-templates
(add-to-list 'org-structure-template-alist '("sh"   . "src sh"))
(add-to-list 'org-structure-template-alist '("el"   . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("sc"   . "src scheme"))
(add-to-list 'org-structure-template-alist '("ts"   . "src typescript"))
(add-to-list 'org-structure-template-alist '("py"   . "src python"))
(add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
(add-to-list 'org-structure-template-alist '("json" . "src json"))
(add-to-list 'org-structure-template-alist '("go"   . "src go"))
(add-to-list 'org-structure-template-alist '("rt"   . "src rust"))
(add-to-list 'org-structure-template-alist '("dt"   . "src dart"))
