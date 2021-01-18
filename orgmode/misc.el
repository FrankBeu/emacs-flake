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
                (org-level-5 . 1.5)
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


(setq org-agenda-files '("~/NOTES"))
