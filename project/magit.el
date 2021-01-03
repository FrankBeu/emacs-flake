;;; magit


;;;; magit

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  )


;;;;; evil-magit
;; TODO integrated into evil-collection - remove after switch to 27.x

(use-package evil-magit
  :after magit
  )


;;;; orgit

;; TODO implement


;;;; orgit-forge

;; TODO implement


;;;; forge

;; TODO currently gitea is only partially supported
;; https://github.com/magit/forge/issues/254
;; (use-package forge)


