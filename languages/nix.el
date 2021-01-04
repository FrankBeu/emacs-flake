;;; nix


(use-package nix-mode
  :mode "\\.nix\\'"
  )


(add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection '("rnix-lsp"))
                  :major-modes '(nix-mode)
                  :server-id 'nix))

;;;; INFO
;;;;; debug
;; bash -c "env RUST_LOG=trace rnix-lsp 2> /tmp/rnix-lsp.log"
