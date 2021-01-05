;;; auctex

(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(use-package auctex
  :init (setq TeX-view-program-selection '((output-pdf "Zathura")))
  :hook (LaTex-mode . lsp-deferred)
  )

(use-package auctex-latexmk
  :config (auctex-latexmk-setup)
  )

;; (TeX-source-correlate-mode)        ; activate forward/reverse search
;; (TeX-PDF-mode)
;; (add-to-list 'TeX-view-program-list '("zathura" zathura-forward-search))
;; (setq TeX-view-program-selection (quote ((output-pdf "zathura") (output-dvi "xdvi"))))
