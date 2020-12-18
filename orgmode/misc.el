;;; orgmode-misc

;;;; font-size-heading
  (custom-set-faces
    '(org-level-1 ((t (:inherit outline-1 :height 1.75))))
    '(org-level-2 ((t (:inherit outline-2 :height 1.5))))
    '(org-level-3 ((t (:inherit outline-3 :height 1.25))))
    '(org-level-4 ((t (:inherit outline-4 :height 1.175))))
    '(org-level-5 ((t (:inherit outline-5 :height 1.0))))
  )

;;;; leading stars
  (setq org-startup-indented t)

;;;; *BOLD*
  (setq org-hide-emphasis-markers t)
