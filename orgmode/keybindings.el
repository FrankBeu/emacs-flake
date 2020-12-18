;;; orgmode-keybindings

(defun fb/org-meta-return (&optional arg)
  "Insert a new heading or wrap a region in a table.
Calls `org-insert-SUBheading', `org-insert-item' or
`org-table-wrap-region', depending on context.  When called with
an argument, unconditionally call `org-insert-SUBheading'."
  (interactive "P")
  (org-check-before-invisible-edit 'insert)
  (or (run-hook-with-args-until-success 'org-metareturn-hook)
      (call-interactively (cond (arg #'org-insert-subheading)
				((org-at-table-p) #'org-table-wrap-region)
				((org-in-item-p) #'org-insert-item)
				(t #'org-insert-subheading)))))


(define-key org-mode-map (kbd "M-<return>") 'fb/org-meta-return)
(define-key org-mode-map (kbd "M-S-<return>") 'org-insert-todo-subheading)
