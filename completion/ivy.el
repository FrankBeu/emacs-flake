;;; ivy-counsel-swiper


;;;; ivy

(use-package ivy
  :diminish
  :bind (
         :map ivy-minibuffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-next-line)
         ("C-;" . ivy-alt-done)
         ("TAB" . ivy-alt-done)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-;" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill)
	 )
  :config (ivy-mode 1)
  )

;; (setq projectile-completion-system 'ivy)


;;;; ivy-avy
;; use avy in ivy-buffers (C-')

(use-package ivy-avy)


;;;; ivy-rich

(use-package all-the-icons-ivy-rich
  :init (all-the-icons-ivy-rich-mode 1)
  )

(use-package ivy-rich
  :init (ivy-rich-mode 1)
  )


;;;; counsel

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)
	 )
  :custom (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config (counsel-mode 1)
  )


;;;; swiper

(use-package swiper
  :bind (("C-s" . swiper)
         ;; ("C-r" . swiper)
         :map swiper-map
         ("C-l" . nil)
         ("C-l" . ivy-next-line)
         ("C-S-L" . swiper-recenter-top-bottom)
	 ))

;;;; INFO
;;;;; manual
;; https://oremacs.com/swiper/
