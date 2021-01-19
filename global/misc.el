;;; misc

;;;; serverMode

(server-start)

;;;; layout

;;;;; minimal interface
(menu-bar-mode -1)
(scroll-bar-mode -1)
(set-fringe-mode 10)
(tool-bar-mode -1)
(tooltip-mode -1)


;;;; font

(defvar fb/default-font-size 160)  ;; height/10 ≙ px
;; (set-frame-font "Roboto Mono 12" nil t)
;; (set-frame-font "Noto Sans Mono 12" nil t)
;; list all available fonts *scratch*: (font-family-list) C-j
;; (set-face-attribute 'default nil :height fb/default-font-size)
(set-face-attribute 'default nil :font "Roboto Mono" :height fb/default-font-size)
;; (set-face-attribute 'default nil :font "Noto Sans Mono" :height fb/default-font-size)
;; (set-face-attribute 'default nil :font "DejaVu Sans Mono" :height fb/default-font-size)
;; (set-face-attribute 'default nil :font "Iosevka Term" :height fb/default-font-size)
;; (set-face-attribute 'default nil :font "Hack" :height fb/default-font-size)
;; (set-face-attribute 'default nil :font "Fira Code" :height fb/default-font-size)


;;;;; fira-code-mode
;; view ./packages.el :fira-code-mode


;;; line-numbers

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(
		eshell-mode-hook
		helpful-mode-hook
		;; neotree-mode-hook
		org-mode-hook
		shell-mode-hook
		term-mode-hook
		treemacs-mode-hook
		)
	      )
  (add-hook mode (lambda () (display-line-numbers-mode 0)))
  )


;;;; parenthesis
(show-paren-mode 1)

;;;; window-splitting

(setq
 split-width-threshold 0
 split-height-threshold nil)


;;;; misc

;; (setq inhibit-startup-screen t )    ;;; inhibit startup screen
(setq inhibit-startup-message t )      ;;; inhibit startup message
(setq initial-scratch-message "")      ;;; print a default message in the empty scratch buffer opened at startup
;; (setq ring-bell-function 'ignore )     ;;; silent bell when you make a mistake
;; (setq visible-bell t)                  ;;; visible bell when you make a mistake - doom-modeline takes care
(setq coding-system-for-read 'utf-8 )  ;;; use utf-8 by default
(setq coding-system-for-write 'utf-8 )
(setq sentence-end-double-space nil)   ;;; sentence SHOULD end with only a point.
(setq fill-column 80)                  ;;; toggle wrapping text at the 80th character


;;;; yes-or-no

(defalias 'yes-or-no-p 'y-or-n-p)


;;;; commands

;;;;; reload-config
(defun fb/reload-config ()
  "reload ~/.emacs.d/init.el interactively"
  (interactive)
  (fb*reload-config))

;;;;; toggle-whichKey-sort-order
(defun fb/toggle-which-key-sort-order ()
  "Toggle whichKey-sort-order-alpha key - desc"
  (interactive)
  (setq which-key-sort-order
	(if (eq which-key-sort-order 'which-key-key-order-alpha) 'which-key-description-order 'which-key-key-order-alpha)))
;;;;; from doom-emacs
;;;###autoload
(defun fb/yank-buffer-filename ()
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (if-let (filename (or buffer-file-name (bound-and-true-p list-buffers-directory)))
      (message (kill-new (abbreviate-file-name filename)))
    (error "Couldn't find filename in current buffer")))
