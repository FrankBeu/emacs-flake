;;; magit


;;;; magit

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  )


;;;; forge
(use-package forge)


;;;; orgit
;; TODO implement


;;;; orgit-forge
;; TODO implement


;; TODO integrated into evil-collection - remove after switch to 27.x
;; TODO fix jkl;
(use-package evil-magit
  :after magit
  )

