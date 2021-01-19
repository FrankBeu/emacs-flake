;;; misc


;; ;;;; company
;;
;; (use-package company
;;   :config
;;   ;; Optionally enable completion-as-you-type behavior.
;;   (setq company-idle-delay 0)
;;   (setq company-minimum-prefix-length 1)

;;   (progn
;;     (add-hook 'after-init-hook 'global-company-mode)))
;;   )


;; ;;;; yasnippets
;; https://joaotavora.github.io/yasnippet/snippet-expansion.html

(use-package yasnippet
  :init
  (yas-global-mode 1)
  :config
  (setq yas-snippet-dirs
	'(
          "~/.emacs.d/snippets"
	  ))
  (yas-reload-all)
  )

;;;; flycheck

;; (use-package flycheck
;;   :hook (prog-mode . flycheck-mode)
;;   ;; :config
;;   ;; (global-flycheck-mode)
;;   )


;;;; modes

;;;;; vimrc-mode

(use-package vimrc-mode
  :mode "\\.vim\\(rc\\)?\\'"
  )
