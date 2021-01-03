;;; k8s


;;;; docker

(use-package docker
  :bind ("C-c d" . docker))


;;;; k8s

(use-package kubernetes
  :commands (kubernetes-overview))


;;;;; k8s-evil

(use-package kubernetes-evil
  :after kubernetes)


;;;;; k8s-helm

(use-package kubernetes-helm
  :after kubernetes)


;;;;; k8s-tramp

(use-package kubernetes-tramp
  :after kubernetes)


;;;;; yaml-mode

(use-package yaml-mode
  :hook (yaml-mode . lsp-deferred)
  )
