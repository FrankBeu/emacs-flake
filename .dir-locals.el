;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

(
 (org-mode
  (eval add-hook 'after-save-hook 'fb*literate-recompile-h)
  )
)

