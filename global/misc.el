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
;; (set-face-attribute 'default nil :font "Roboto Mono" :height fb/default-font-size)
;; (set-face-attribute 'default nil :font "Noto Sans Mono" :height fb/default-font-size)
;; (set-face-attribute 'default nil :font "DejaVu Sans Mono" :height fb/default-font-size)
;; (set-face-attribute 'default nil :font "Iosevka Term" :height fb/default-font-size)
;; (set-face-attribute 'default nil :font "Hack" :height fb/default-font-size)
(set-face-attribute 'default nil :font "Fira Code" :height fb/default-font-size)


;;;;; fira-code-mode
;; view ./modes.el:fira-code-mode


;;;; line-numbers

(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(
		eshell-mode-hook
		org-mode-hook
		shell-mode-hook
		term-mode-hook
		)
	      )
  (add-hook mode (lambda () (display-line-numbers-mode 0)))
  )


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
(setq version-control t )              ;;; use version control
(setq vc-follow-symlinks t )           ;;; don't ask for confirmation when opening symlinked file under vc
(setq vc-make-backup-files t )         ;;; make backups file even when in version controlled dir
(setq delete-old-versions -1 )         ;;; delete excess backup versions silently
(setq backup-directory-alist `(("." . "~/.emacs.d/BAK")) )                       ;;; which directory to put backups file
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) )  ;;; transform backups file name


;;;; functions

;;;;; reload-config
(defun fb/reload-config ()
  "reload ~/.emacs.d/init.el"
  (interactive)
  (load-file "~/.emacs.d/init.el"))
