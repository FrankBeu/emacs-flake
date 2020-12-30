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
;; (set-frame-font "Roboto Mono 12" nil t)
;; (set-frame-font "Noto Sans Mono 12" nil t)
;; list all available fonts *scratch*: (font-family-list) C-j
;; (set-face-attribute 'default nil :height 160)  ;;; height/10 =^= px
;; (set-face-attribute 'default nil :font "Roboto Mono" :height 160)  ;;; height/10 ≙ px
;; (set-face-attribute 'default nil :font "Noto Sans Mono" :height 160)  ;;; height/10 ≙ px
;; (set-face-attribute 'default nil :font "DejaVu Sans Mono" :height 160)  ;;; height/10 ≙ px
;; (set-face-attribute 'default nil :font "Iosevka Term" :height 160)  ;;; height/10 ≙ px
(set-face-attribute 'default nil :font "Hack" :height 160)  ;;; height/10 ≙ px


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
