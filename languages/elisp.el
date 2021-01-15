;;; elisp

;;;; hooks
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            ;; (make-local-variable 'outline-regexp)
            ;; (setq outline-regexp "^;;; ")
            ;; (make-local-variable 'outline-heading-end-regexp)
            ;; (setq outline-heading-end-regexp ":\n")
            (outline-minor-mode 1)
            ))
