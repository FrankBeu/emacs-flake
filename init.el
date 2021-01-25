(require 'package)

(setq package-archives nil)

(setq package-enable-at-startup nil)
;; (package-initialize 'no-activate)
(package-initialize)
(eval-when-compile
  (require 'use-package))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; tangling
;;;;
;;

(defmacro letf! (bindings &rest body)
  "Temporarily rebind function and macros in BODY.
Intended as a simpler version of `cl-letf' and `cl-macrolet'.

BINDINGS is either a) a list of, or a single, `defun' or `defmacro'-ish form, or
b) a list of (PLACE VALUE) bindings as `cl-letf*' would accept.

TYPE is either `defun' or `defmacro'. NAME is the name of the function. If an
original definition for NAME exists, it can be accessed as a lexical variable by
the same name, for use with `funcall' or `apply'. ARGLIST and BODY are as in
`defun'.

\(fn ((TYPE NAME ARGLIST &rest BODY) ...) BODY...)"
  (declare (indent defun))
  (setq body (macroexp-progn body))
  (when (memq (car bindings) '(defun defmacro))
    (setq bindings (list bindings)))
  (dolist (binding (reverse bindings) (macroexpand body))
    (let ((type (car binding))
          (rest (cdr binding)))
      (setq
       body (pcase type
              (`defmacro `(cl-macrolet ((,@rest)) ,body))
              (`defun `(cl-letf* ((,(car rest) (symbol-function #',(car rest)))
                                  ((symbol-function #',(car rest))
                                   (lambda ,(cadr rest) ,@(cddr rest))))
                         (ignore ,(car rest))
                         ,body))
              (_
               (when (eq (car-safe type) 'function)
                 (setq type (list 'symbol-function type)))
               (list 'cl-letf (list (cons type rest)) body)))))))

(defmacro print! (message &rest args)
  "Prints MESSAGE, formatted with ARGS, to stdout.

Returns non-nil if the message is a non-empty string.

Can be colored using (color ...) blocks:

  (print! \"Hello %s\" (bold (blue \"How are you?\")))
  (print! \"Hello %s\" (red \"World\"))
  (print! (green \"Great %s!\") \"success\")

Uses faces in interactive sessions and ANSI codes otherwise."
  `(doom--print (format! ,message ,@args)))

(defun doom--print (output)
  (unless (string-empty-p output)
    (princ output)
    (terpri)
    t))

(defmacro format! (message &rest args)
  "An alternative to `format' that understands (color ...) and converts them
into faces or ANSI codes depending on the type of sesssion we're in."
  `(doom--format (format ,@(doom--output-apply `(,message ,@args)))))

(defun doom--output-apply (forms &optional sub)
  "Replace color-name functions with calls to `doom--output-color'."
  (cond ((null forms) nil)
        ((listp forms)
         (append (cond ((not (symbolp (car forms)))
                        (list (doom--output-apply (car forms))))
                       (sub
                        (list (car forms)))
                       ((assq (car forms) doom-output-ansi-alist)
                        `(doom--output-color ',(car forms)))
                       ((assq (car forms) doom-output-class-alist)
                        `(doom--output-class ',(car forms)))
                       ((list (car forms))))
                 (doom--output-apply (cdr forms) t)
                 nil))
        (forms)))

(defvar doom-output-ansi-alist
  '(;; fx
    (bold       1 :weight bold)
    (dark       2)
    (italic     3 :slant italic)
    (underscore 4 :underline t)
    (blink      5)
    (rapid      6)
    (contrary   7)
    (concealed  8)
    (strike     9 :strike-through t)
    ;; fg
    (black      30 term-color-black)
    (red        31 term-color-red)
    (green      32 term-color-green)
    (yellow     33 term-color-yellow)
    (blue       34 term-color-blue)
    (magenta    35 term-color-magenta)
    (cyan       36 term-color-cyan)
    (white      37 term-color-white)
    ;; bg
    (on-black   40 term-color-black)
    (on-red     41 term-color-red)
    (on-green   42 term-color-green)
    (on-yellow  43 term-color-yellow)
    (on-blue    44 term-color-blue)
    (on-magenta 45 term-color-magenta)
    (on-cyan    46 term-color-cyan)
    (on-white   47 term-color-white))
  "An alist of fg/bg/fx names mapped to ansi codes and term-color-* variables.

This serves as the cipher for converting (COLOR ...) function calls in `print!'
and `format!' into colored output, where COLOR is any car of this list.")

(defvar doom-output-class-alist
  `((color . doom--output-color)
    (class . doom--output-class)
    (indent . doom--output-indent)
    (autofill . doom--output-autofill)

    (success . (lambda (str &rest args)
                 (apply #'doom--output-color 'green (format "✓ %s" str) args)))
    (warn    . (lambda (str &rest args)
                 (apply #'doom--output-color 'yellow (format "! %s" str) args)))
    (error   . (lambda (str &rest args)
                 (apply #'doom--output-color 'red (format "x %s" str) args)))
    (info    . (lambda (str &rest args)
                 (concat "- " (if args (apply #'format str args) str))))
    (start    . (lambda (str &rest args)
                  (concat "> " (if args (apply #'format str args) str))))
    (debug   . (lambda (str &rest args)
                 (if doom-debug-p
                     (apply #'doom--output-color 'dark
                            (format "- %s" str)
                            args)
                   "")))
    (path    . abbreviate-file-name)
    (symbol . symbol-name)
    (relpath . (lambda (str &optional dir)
                 (if (or (not str)
                         (not (stringp str))
                         (string-empty-p str))
                     str
                   (let ((dir (or dir (file-truename default-directory)))
                         (str (file-truename str)))
                     (if (file-in-directory-p str dir)
                         (file-relative-name str dir)
                       (abbreviate-file-name str))))))
    (filename . file-name-nondirectory)
    (dirname . (lambda (path)
                 (unless (file-directory-p path)
                   (setq path (file-name-directory path)))
                 (directory-file-name path))))
  "An alist of text classes that map to transformation functions.

Any of these classes can be called like functions from within `format!' and
`print!' calls, which will transform their input.")

(defun doom--format (output)
  (if (string-empty-p (string-trim output))
      ""
    (concat (make-string doom-output-indent 32)
            (replace-regexp-in-string
             "\n" (concat "\n" (make-string doom-output-indent 32))
             output t t))))

(defun doom--output-class (class format &rest args)
  "Apply CLASS to formatted format with ARGS.

CLASS is derived from `doom-output-class-alist', and can contain any arbitrary,
transformative logic."
  (let (fn)
    (cond ((setq fn (cdr (assq class doom-output-class-alist)))
           (if (functionp fn)
               (apply fn format args)
             (error "%s does not have a function" class)))
          (args (apply #'format format args))
          (format))))

(defvar doom-output-indent 0
  "Level to rigidly indent text returned by `format!' and `print!'.")

(defvar doom-output-indent-increment 2
  "Steps in which to increment `doom-output-indent' for consecutive levels.")

(defmacro print-group! (&rest body)
  "Indents any `print!' or `format!' output within BODY."
  `(let ((doom-output-indent (+ doom-output-indent-increment doom-output-indent)))
     ,@body))

(defvar fb*literate-config-file
  (expand-file-name "config.org" user-emacs-directory)
  "The file path of your literate config file.")

(defvar fb*literate-config-cache-file
  (expand-file-name ".local/cache/tangle/literate-last-compile" user-emacs-directory)
  "The file path that `fb*literate-config-file' will be tangled to, then
byte-compiled from.")

(defvar fb*literate-target-file
  (expand-file-name "init.el" user-emacs-directory)
  "The file path of your target config file.")

(defvar org-mode-hook)
(defvar org-inhibit-startup)

(defun fb*literate-tangle-h ()
  "Tangles `fb*literate-config-file' if it has changed."
  (and (not (getenv "__NOTANGLE"))
       (require 'ox nil t)
       (require 'ob-tangle nil t)
       (letf! ((default-directory user-emacs-directory)
               (target fb*literate-config-file)
               (cache fb*literate-config-cache-file)
               (dest fb*literate-target-file)
               ;; Operate on a copy because `org-babel-tangle' has
               ;; side-effects we need to undo immediately as not to
               ;; overwrite the user's config; it's bad ettiquite.
               (backup (make-temp-file (concat (file-name-nondirectory target) ".")))

               ;; HACK A hack to prevent ob-tangle from operating relative to
               ;;      the backup file and thus tangling to the wrong
               ;;      destinations.
               (defun org-babel-tangle-single-block (&rest args)
                 (let* ((spec (apply org-babel-tangle-single-block args))
                        (file (nth 1 spec))
                        (file (if (file-equal-p file backup) target file))
                        (file (if org-babel-tangle-use-relative-file-links
                                  (file-relative-name file)
                                file)))
                   (setf (nth 1 spec) file)
                   spec))
               ;; Ensure output conforms to the formatting of all doom CLIs
               (defun message (msg &rest args)
                 (when msg
                   (print! (info "%s") (apply #'format msg args)))))
         (print! (start "Compiling your literate config..."))
         (print-group!
          (unwind-protect
              (with-temp-file backup
                (insert-file-contents target)
                (let ((buffer-file-name backup)
                      ;; Prevent unwanted entries in recentf, or formatters, or
                      ;; anything that could be on these hooks, really. Nothing
                      ;; else should be touching these files (particularly in
                      ;; interactive sessions).
                      (write-file-functions nil)
                      (before-save-hook nil)
                      (after-save-hook nil)
                      ;; Prevent infinite recursion due to recompile-on-save
                      ;; hooks later, and speed up `org-mode' init.
                      (org-mode-hook nil)
                      (org-inhibit-startup t))
                  (org-mode)
                  (with-silent-modifications
                    ;; Tangling won't ordinarily expand #+INCLUDE directives,
                    ;; so I do it myself.
                    (org-export-expand-include-keyword)
                    (org-babel-tangle nil dest))))
            (ignore-errors (delete-file backup)))
          ;; Write an empty file to serve as our mtime cache
          (with-temp-file cache)
          ;; (if doom-interactive-p t
            ;; (message "Restarting..." )
            ;; (throw 'exit "__NOTANGLE=1 $@"))
          ))))

(defun fb/literate-recompile ()
  "Recompile literate config to `user-emacs-directory'"
  (interactive)
  (display-message-or-buffer "recompiling emacs config")
  (fb*literate-tangle-h)
  )

;; (setq debug-on-error t)
;; (setq debug-ignored-error t)
;;;; nixos-packages

(defun fb*getPathToConfigFile (filename)
  "Returns concatenation of \"HOME\" , \".emacs.d/\" and the passed \"filename\"."
  (expand-file-name filename (expand-file-name ".emacs.d" (getenv "HOME"))))
(defun fb*loadConfigFile (configFileName)
  "Load the config-file associated with the passed configFileName if it exists."
  (let ((pathToConfigFile (fb*getPathToConfigFile configFileName)))
    (if (file-readable-p pathToConfigFile) (load pathToConfigFile) (message "WARNING: CONFIG-FILE NOT FOUND: %s" pathToConfigFile))
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; completion
;;;;
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; completion-ivy
;;;;
;;

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

(use-package ivy-avy)

(use-package all-the-icons-ivy-rich
  :init (all-the-icons-ivy-rich-mode 1)
  )

(use-package ivy-rich
  :init (ivy-rich-mode 1)
  )

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)
	 )
  :custom (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config (counsel-mode 1)
  )

(use-package swiper
  :bind (("C-s" . swiper)
         ;; ("C-r" . swiper)
         :map swiper-map
         ("C-l" . nil)
         ("C-l" . ivy-next-line)
         ("C-S-L" . swiper-recenter-top-bottom)
	 ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; evil
;;;;
;;

(setq evil-want-keybinding nil)
(defun fb/evil-hook ()
  (dolist (mode '(
                  custom-mode
                  eshell-mode
                  git-rebase-mode
                  term-mode
                  ;; undo-tree-visualizer-mode
                  ))
    (add-to-list 'evil-emacs-state-modes mode)))

(use-package evil
  :init
  (setq evil-want-C-i-jump nil
        ;; evil-want-C-u-scroll t     ;; TODO shadows C-u universal argument
        evil-want-Y-yank-to-eol t
        evil-want-integration t
        )
  ;; (evil-set-initial-state 'undo-tree-visualizer-mode 'emacs)
  :config
  (add-hook 'evil-mode-hook 'fb/evil-hook)
  (evil-mode 1)
  :custom
  (evil-undo-system 'undo-tree)
  )

(use-package evil-collection
  :after evil
  :init
  (setq evil-want-keybinding nil)
  :config
  ;; (add-hook 'evil-collection-setup-hook #'fb/hjkl-rotation)
  (evil-collection-init)
  )

;; (use-package evil-commentary
;;   :config
;;   (evil-commentary-mode)
;;   )

(use-package evil-escape
  :config
  (evil-escape-mode)
  )

(use-package evil-nerd-commenter
  :config
  (setq evilnc-invert-comment-line-by-line t)
  ;; (evilnc-default-hotkeys)
  )

(use-package evil-numbers)

(use-package evil-surround
  :config
  (global-evil-surround-mode 1)
  )

(use-package undo-tree
  :config
  (global-undo-tree-mode 1)
  :custom
  (undo-tree-visualizer-diff t)
  (undo-tree-visualizer-timestamps t) 
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; global
;;;;
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; global-misc
;;;;
;;

(server-start)

(setq
 split-width-threshold 0
 split-height-threshold nil)

(defalias 'yes-or-no-p 'y-or-n-p)

;; (setq inhibit-startup-screen t )    ;;; inhibit startup screen
(setq inhibit-startup-message t )      ;;; inhibit startup message
(setq initial-scratch-message "")      ;;; print a default message in the empty scratch buffer opened at startup
;; (setq ring-bell-function 'ignore )     ;;; silent bell when you make a mistake
;; (setq visible-bell t)                  ;;; visible bell when you make a mistake - doom-modeline takes care
(setq coding-system-for-read 'utf-8 )  ;;; use utf-8 by default
(setq coding-system-for-write 'utf-8 )
(setq sentence-end-double-space nil)   ;;; sentence SHOULD end with only a point.
(setq fill-column 80)                  ;;; toggle wrapping text at the 80th character

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; global-packages
;;;;
;;

(use-package all-the-icons)

(use-package ansi-color
  :commands fb/display-ansi-colors
  :config
  (defun fb/display-ansi-colors ()
    (interactive)
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))
  )

(use-package avy)

(use-package command-log-mode)

(use-package dired
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  )

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

;; (use-package dired-git-info
;;   :after dired
;;   :hook (dired-after-readin . dired-git-info-auto-enable)
;;   :config
;;   (setq dgi-auto-hide-details-p nil)
;;   )

(use-package dired-hide-dotfiles
  :hook (dired-mode . dired-hide-dotfiles-mode)
  )

;; (use-package dired-k
;;   :after dired
;;   :hook ((dired-initial-position . dired-k)
;; 	 ;; (dired-after-readin     . #'dired-k-no-revert)
;; 	 )
;;   :config
;;   ;; (setq dired-k-style nil)
;;   (setq dired-k-style 'git)
;;   ;; (setq dired-k-human-readable nil)
;;   (setq dired-k-human-readable t)
;;   (setq dired-k-padding 1)
;;   )

(use-package dired-rifle
  :after dired
  )

(use-package dired-single)

(use-package fira-code-mode
  ;; :config (global-fira-code-mode) ;; will not work with orgmode headline-stars
  :hook prog-mode
  :disabled
  )

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key)
  )

(use-package imenu-list
  ;; TODO enable in each language-mode cf. spacemacs
  ;; :hook (prog-mode . imenu-list-minor-mode)
  ;; :disabled
  )

;; (use-package neotree
;;   :config
;;   (setq neo-smart-open t)
;;   (setq neo-autorefresh t)
;;   (setq neo-vc-integration '(face))
;;   )

(use-package no-littering
  :init
  (setq no-littering-etc-directory (expand-file-name ".state/config/" user-emacs-directory)
        no-littering-var-directory (expand-file-name ".state/data/"   user-emacs-directory))
  :config
  (setq
   auto-save-file-name-transforms `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))
   backup-by-copying t
   delete-old-versions t          ;;; delete excess backup versions silently
   kept-new-versions 128
   kept-old-versions 0
   make-backup-files t
   vc-follow-symlinks t           ;;; don't ask for confirmation when opening symlinked file under vc
   vc-make-backup-files t         ;;; make backups file even when in version controlled dir
   version-control t              ;;; use version control
   )
  )

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  )

(use-package rainbow-mode
  :hook ((prog-mode . rainbow-mode)
         (org-mode . rainbow-mode)
         )
  )

(use-package ripgrep)

(use-package treemacs
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-directory-name-transformer    #'identity
          treemacs-display-in-side-window        t
          treemacs-eldoc-display                 t
          treemacs-file-event-delay              5000
          treemacs-file-extension-regex          treemacs-last-period-regex-value
          treemacs-file-follow-delay             0.2
          treemacs-file-name-transformer         #'identity
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemaca-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-move-forward-on-expand        nil
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".state/data/treemacs/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-read-string-input             'from-child-frame
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-user-mode-line-format         nil
          treemacs-user-header-line-format       nil
          treemacs-width                         35
          treemacs-workspace-switch-cleanup      nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    ;; (treemacs-follow-mode nil)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag))
  ;; :general
  ;; (:keymaps 'treemacs-mode-map
  ;; :states 'treemacs
  ;; "l" 'nil)
  ;; :after general
  )

(use-package treemacs-all-the-icons
  :after treemacs all-the-icons
  :config
  (treemacs-load-theme "all-the-icons")
  )

(use-package treemacs-evil
  :after treemacs evil)

(use-package treemacs-icons-dired
  :after treemacs dired
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit)

(use-package treemacs-persp ;;treemacs-persective if you use perspective.el vs. persp-mode
  :after treemacs persp-mode ;;or perspective vs. persp-mode
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-projectile
  :after treemacs projectile)

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq
   which-key-idle-delay 0.5
   which-key-max-description-length nil
   which-key-allow-imprecise-window-fit t
   ;; which-key-sort-order 'which-key-key-order-alpha
   which-key-sort-order 'which-key-description-order
   )
  )

(use-package writeroom-mode
  :config
  (setq writeroom-mode-line-toggle-position 'mode-line-format)
  (setq writeroom-width 98)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; global-functions
;;;;
;;

(defun fb*reload-config ()
  "reload ~/.emacs.d/init.el"
  (load-file "~/.emacs.d/init.el"))

(defun fb*toggle-which-key-sort-order ()
  "Toggle whichKey-sort-order-alpha key - desc"
  (setq which-key-sort-order
	(if (eq which-key-sort-order 'which-key-key-order-alpha) 'which-key-description-order 'which-key-key-order-alpha)))

(defun fb*yank-buffer-filename ()
  "Copy the current buffer's path to the kill ring."
  (if-let (filename (or buffer-file-name (bound-and-true-p list-buffers-directory)))
      (message (kill-new (abbreviate-file-name filename)))
    (error "Couldn't find filename in current buffer")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; global-commands
;;;;
;;

(defun fb/yank-buffer-filename ()
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (fb*yank-buffer-filename))

(defun fb/break-here ()
  "break text at cursor"
  (interactive)
  (evil-backward-char)
  (insert "\n")
  (delete-char 1))

(defun fb/break-sentence ()
  "break text at end of sentence"
  (interactive)
  (evil-forward-sentence-begin)
  (insert "\n"))

(defun fb/break-sub-sentence ()
  "break text at end of sub-sentence"
  (interactive)
  (setq beforeBreakpoint
        (read-char "enter char before breakpoint: ,  ;  :  .  !  ?"))
  (pcase beforeBreakpoint
    (?,  (search-forward ",") (evil-forward-word-begin) (insert "\n"))
    (?:  (search-forward ":") (evil-forward-word-begin) (insert "\n"))
    (?\; (search-forward ";") (evil-forward-word-begin) (insert "\n"))
    (?.  (search-forward ".") (evil-forward-word-begin) (insert "\n"))
    (?!  (search-forward "!") (evil-forward-word-begin) (insert "\n"))
    (??  (search-forward "?") (evil-forward-word-begin) (insert "\n"))
    ))

(defun fb/reload-config ()
  "reload ~/.emacs.d/init.el interactively"
  (interactive)
  (fb*reload-config))

(defun fb/toggle-which-key-sort-order ()
  "Toggle whichKey-sort-order-alpha key - desc"
  (interactive)
  (fb*toggle-which-key-sort-order))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; languages
;;;;
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; languages-misc
;;;;
;;

(use-package company
  :after lsp-mode
  :hook (
         ;; (after-init-hook 'global-company-mode)
         (lsp-mode . company-mode)
       )
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
)

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package yasnippet
  :init
  (yas-global-mode 1)
  :config
  (setq yas-snippet-dirs
        '(
          "~/.emacs.d/snippets"
          )
        ))

(use-package yasnippet-snippets
:after yasnippet
:config
  (yasnippet-snippets-initialize)
  (yas-reload-all)
  )

;; (use-package flycheck
;;   :hook (prog-mode . flycheck-mode)
;;   ;; :config
;;   ;; (global-flycheck-mode)
;;   )

(use-package vimrc-mode
  :mode "\\.vim\\(rc\\)?\\'"
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; languages-lsp
;;;;
;;

(defun fb*lsp-mode-setup-h ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode . fb*lsp-mode-setup-h)
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; or 'c-l', 's-l'
  :config
  (lsp-enable-which-key-integration t)
  )

;; (use-package lsp-ui :commands lsp-ui-mode)
  (use-package lsp-ui
    :hook (lsp-mode . lsp-ui-mode)
    ;; :commands lsp-ui-mode
    :custom
    ;; (lsp-ui-sideline-show-diagnostics t)
    ;; (lsp-ui-sideline-show-hover t)
    ;; (lsp-ui-sideline-show-code-actions t)
    ;; (lsp-ui-sideline-update-mode t)
    (lsp-ui-sideline-delay 0)
    ;; (lsp-ui-doc-position 'bottom)
    (lsp-ui-doc-position 'at-point)

    )
;; (use-package lsp-ui)

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol
  )

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list
  :after lsp
  )

;; (use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; languages-elisp
;;;;
;;

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            ;; (make-local-variable 'outline-regexp)
            ;; (setq outline-regexp "^;;; ")
            ;; (make-local-variable 'outline-heading-end-regexp)
            ;; (setq outline-heading-end-regexp ":\n")
            (outline-minor-mode 1)
            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; languages-golang
;;;;
;;

(use-package go-mode
  :hook ((go-mode . lsp-deferred)
         ;; (go-mode . yas-minor-mode)
         )
  )

(defun fb/lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t)
  )
(add-hook 'go-mode-hook #'fb/lsp-go-install-save-hooks)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; languages-k8s
;;;;
;;

(use-package docker
  :bind ("C-c d" . docker))

(use-package kubernetes
  :commands (kubernetes-overview))

(use-package kubernetes-evil
  :after kubernetes)

(use-package kubernetes-helm
  :after kubernetes)

(use-package kubernetes-tramp
  :after kubernetes)

(use-package yaml-mode
  :hook (yaml-mode . lsp-deferred)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; languages-nix
;;;;
;;

(use-package nix-mode
  :mode "\\.nix\\'"
  )

;; (add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
;; (lsp-register-client
 ;; (make-lsp-client :new-connection (lsp-stdio-connection '("rnix-lsp"))
                  ;; :major-modes '(nix-mode)
                  ;; :server-id 'nix))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; languages-typeScript
;;;;
;;

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; modeline
;;;;
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; modeline-doom-modeline
;;;;
;;

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom (doom-modeline-height 50)
  :config (setq doom-modeline-icon t)  ;; needed for emacs-server
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; orgmode
;;;;
;;

(with-eval-after-load 'org

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; orgmode-misc
;;;;
;;

(setq org-hide-emphasis-markers t)

(setq org-ellipsis
      ;; " ▾"
      ;; " ▽"
      "  ▼"
      ;; "  ◦◦◦"
      )

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars t)
  (org-superstar-headline-bullets-list
   ;; '("◉" "○" "●" "○" "●" "○" "●")
   ;; '("●" "◉" "○" "●" "◉" "○" "●")
   ;; '("●")
   ;; '("◉")
   '("○")
   )
  (org-superstar-item-bullet-alist
   '(
     (?- . ?•)
     (?+ . ?➤)
     ;; (?+ . ?▶)
     ;; (?+ . ?▷)
     ;; (?+ . ?▸)
     ;; (?+ . ?▹)
     ;; (?+ . ?►)
     ;; (?+ . ?▻)
     ;; (?+ . ?◉)
     ;; (?+ . ?○)
     ;; (?+ . ?◌)
     ;; (?+ . ?◍)
     ;; (?+ . ?◎)
     ;; (?+ . ?●)
     (?* . ?•)
     )
   )
  )

(setq org-startup-indented t)

(use-package org-make-toc
  :hook (org-mode . org-make-toc-mode))

(setq org-agenda-files '("~/NOTES"))
(setq org-agenda-start-with-log-mode t)
(setq org-log-done 'time)
(setq org-log-into-drawer t)

(add-to-list 'org-structure-template-alist '("sh"   . "src sh"))
(add-to-list 'org-structure-template-alist '("el"   . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("sc"   . "src scheme"))
(add-to-list 'org-structure-template-alist '("ts"   . "src typescript"))
(add-to-list 'org-structure-template-alist '("py"   . "src python"))
(add-to-list 'org-structure-template-alist '("yaml" . "src yaml"))
(add-to-list 'org-structure-template-alist '("json" . "src json"))
(add-to-list 'org-structure-template-alist '("go"   . "src go"))
(add-to-list 'org-structure-template-alist '("rt"   . "src rust"))
(add-to-list 'org-structure-template-alist '("dt"   . "src dart"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; orgmode-functions
;;;;
;;

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

);;with-eval-end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; project
;;;;
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; project-magit
;;;;
;;

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  )

(use-package evil-magit
  :after magit
  )





;; (use-package forge)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; project-projectile
;;;;
;;

(use-package projectile
  :init
  (when (file-directory-p "~/SRC/GITEA")
    (setq projectile-project-search-path '("~/SRC/GITEA")))
  ;; (setq projectile-known-projects-file (expand-file-name ".state/projectile-bookmarks.eld" user-emacs-directory))
  ;; (setq projectile-switch-project-action #'projectile-dired)
  (setq projectile-switch-project-action 'neotree-projectile-action)
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  )

(use-package counsel-projectile
  :config (counsel-projectile-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; tex
;;;;
;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; themes
;;;;
;;

;; (load-theme 'deeper-blue)
;; (load-theme 'wombat)

;; (load-theme 'zerodark 'no-confirm)
;; (zerodark-setup-modeline-format)

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ;; if nil, bold is universally disabled
        doom-themes-enable-italic t) ;; if nil, italics is universally disabled
  ;; (load-theme 'doom-acario-dark t)
  ;; (load-theme 'doom-acario-light t)
  ;; (load-theme 'doom-challenger-deep t)
  ;; (load-theme 'doom-city-lights t)
  ;; (load-theme 'doom-dark+ t)                       ;; +
  (load-theme 'doom-dracula t)                     ;; +
  ;; (load-theme 'doom-ephemeral t)
  ;; (load-theme 'doom-fairy-floss t)
  ;; (load-theme 'doom-gruvbox-light t)
  ;; (load-theme 'doom-gruvbox t)                     ;; +
  ;; (load-theme 'doom-henna t)
  ;; (load-theme 'doom-horizon t)
  ;; (load-theme 'doom-Iosvkem t)
  ;; (load-theme 'doom-laserwave t)
  ;; (load-theme 'doom-manegarm t)
  ;; (load-theme 'doom-material t)                    ;; +
  ;; (load-theme 'doom-molokai t)
  ;; (load-theme 'doom-monokai-classic t)
  ;; (load-theme 'doom-monokai-pro t)
  ;; (load-theme 'doom-monokai-spectrum t)
  ;; (load-theme 'doom-moonlight t)
  ;; (load-theme 'doom-nord-light t)
  ;; (load-theme 'doom-nord t)
  ;; (load-theme 'doom-nova t)
  ;; (load-theme 'doom-oceanic-next t)
  ;; (load-theme 'doom-old-hope t)
  ;; (load-theme 'doom-one-light t)
  ;; (load-theme 'doom-one t)                         ;; +
  ;; (load-theme 'doom-opera-light t)
  ;; (load-theme 'doom-opera t)
  ;; (load-theme 'doom-outrun-electric t)
  ;; (load-theme 'doom-palenight t)
  ;; (load-theme 'doom-peacock t)
  ;; (load-theme 'doom-rouge t)
  ;; (load-theme 'doom-snazzy t)
  ;; (load-theme 'doom-solarized-dark t)              ;; +
  ;; (load-theme 'doom-solarized-light t)
  ;; (load-theme 'doom-sourcerer t)
  ;; (load-theme 'doom-spacegrey t)
  ;; (load-theme 'doom-tomorrow-day t)
  ;; (load-theme 'doom-tomorrow-night t)
  ;; (load-theme 'doom-vibrant t)
  ;; (load-theme 'doom-wilmersdorf t)
  ;; (load-theme 'doom-zenburn t)


  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)

  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ;; use the colorful treemacs theme
  (doom-themes-treemacs-config)

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; view
;;;;
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; view-font
;;;;
;;

(defvar fb-default-font-size 160) ;; height/10 ≙ px

(defvar fb-default-font-name-mono  "Roboto Mono")
(defvar fb-default-font-name-sans  "Roboto"     )
(defvar fb-default-font-name-serif "Noto Serif" )

(defvar fb-default-typeface fb-default-font-name-mono )
;; (defvar fb-default-typeface fb-default-font-name-sans )
;; (defvar fb-default-typeface fb-default-font-name-serif)

(defvar fb-default-typeface-fixed fb-default-font-name-mono )

(defvar fb-default-typeface-variable fb-default-font-name-sans )
;; (defvar fb-default-typeface-variable fb-default-font-name-serif)

;; (with-eval-after-load (or 'text-mode 'prog-mode)
(with-eval-after-load 'org
  (set-face-attribute 'default        nil :font fb-default-typeface          :height fb-default-font-size)
  (set-face-attribute 'fixed-pitch    nil :font fb-default-typeface-fixed    :foreground nil             )
  (set-face-attribute 'variable-pitch nil :font fb-default-typeface-variable :foreground nil             )
  )

(with-eval-after-load 'org
  (dolist (face '((org-level-1 . 1.75 )
                  (org-level-2 . 1.5  )
                  (org-level-3 . 1.25 )
                  (org-level-4 . 1.175)
                  (org-level-5 . 1.15 )
                  (org-level-6 . 1.1  )
                  (org-level-7 . 1.05 )
                  (org-level-8 . 1.05 )
                  ))
    (set-face-attribute (car face) nil :font fb-default-typeface-variable :weight 'regular :height (cdr face))))

(defun fb*org-font-faces-mono ()
  (set-face-attribute 'org-block            nil :inherit '(fixed-pitch         ) :foreground nil :height (- fb-default-font-size 10))
  (set-face-attribute 'org-checkbox         nil :inherit '(fixed-pitch         )                )
  (set-face-attribute 'org-code             nil :inherit '(fixed-pitch shadow  )                )
  (set-face-attribute 'org-date             nil :inherit '(fixed-pitch         )                )
  (set-face-attribute 'org-formula          nil :inherit '(fixed-pitch         )                )
  (set-face-attribute 'org-indent           nil :inherit '(fixed-pitch org-hide)                )   ;;;; fixes indentation
  (set-face-attribute 'org-link             nil :inherit '(fixed-pitch         ) :weight 'normal)
  (set-face-attribute 'org-meta-line        nil :inherit '(fixed-pitch font-lock-comment-face)  )
  (set-face-attribute 'org-special-keyword  nil :inherit '(fixed-pitch font-lock-comment-face)  )
  (set-face-attribute 'org-table            nil :inherit '(fixed-pitch         )                )
  (set-face-attribute 'org-verbatim         nil :inherit '(fixed-pitch shadow  )                )
  )

(defun fb*org-buffer-variable-pitch-h ()
  (variable-pitch-mode t)
  (fb*org-font-faces-mono))

(add-hook 'org-mode-hook 'fb*org-buffer-variable-pitch-h)

(setq line-spacing 2)

(setq org-fontify-quote-and-verse-blocks t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; view-misc
;;;;
;;

(menu-bar-mode -1)
(scroll-bar-mode -1)
(set-fringe-mode 10)
(tool-bar-mode -1)
(tooltip-mode -1)

(column-number-mode)
(global-display-line-numbers-mode t)

(dolist (mode '(
                eshell-mode-hook
                helpful-mode-hook
                org-mode-hook
                shell-mode-hook
                term-mode-hook
                treemacs-mode-hook
                ))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(show-paren-mode 1)

(setq org-startup-indented t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; keys
;;;;
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; keys-general
;;;;
;;

(use-package general
  :config
  (general-create-definer fb/leader-key
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")
  (general-create-definer fb/local-leader-key
    :keymaps '(normal insert visual emacs)
    :prefix "SPC k"
    :global-prefix "C-SPC k")
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; keys-hydra
;;;;
;;

(use-package hydra)

(defhydra hydra-text-scale (:timeout 5)
  "scale text"
  ("k" text-scale-increase "in")
  ("l" text-scale-decrease "out")
  ("q" nil "quit" :exit t))

(fb/leader-key
  "ts" '(hydra-text-scale/body :which-key "scale text"))

(defhydra hydra-evil-numbers (:timeout 5)
  "evil-numbers"
  ("k" evil-numbers/inc-at-pt "+")
  ("l" evil-numbers/dec-at-pt "-")
  ("q" nil "quit" :exit t))

(fb/leader-key
  "n." '(hydra-evil-numbers/body :which-key "transient"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; keys-keybindings
;;;;
;;

(general-define-key
 :keymaps '(evil-normal-state-map)
 ;; :states  '(normal visual)
 "h"   'evil-repeat-find-char
 )

(general-define-key
 :keymaps '(evil-motion-state-map)
 ;; :states  '(normal visual)
 "j" 'evil-backward-char
 "k" 'evil-previous-visual-line
 "l" 'evil-next-visual-line
 ";" 'evil-forward-char
 )

(general-define-key
 :keymaps '(evil-visual-state-map)
 "k" 'evil-previous-visual-line
 "l" 'evil-next-visual-line
 )

(general-def 'motion
  "k" 'evil-previous-visual-line
  "l" 'evil-next-visual-line
  )

(evil-set-initial-state 'messages-buffer-mode 'normal)
(evil-set-initial-state 'dashboard-mode 'normal)

(general-define-key
 "<escape>" 'keyboard-escape-quit
 )

(general-define-key
 "C-'"  'avy-goto-word-0
 "C-\"" 'avy-goto-line
 )

(general-define-key
 :keymaps '(company-active-map
            company-search-map
            )
 "C-k" 'nil
 "C-k" 'company-select-previous
 "C-l" 'nil
 "C-l" 'company-select-next
 "C-;" 'nil
 "C-;" 'company-complete
 )

(general-define-key
 :keymaps '(
            company-active-map
            )
 "C-j" 'nil
 "C-j" 'company-complete-selection
 )

(general-define-key
 :keymaps '(
            lsp-mode-map
            )
 "C-j" 'nil
 "C-j" 'company-indent-or-complete-common
 "C-k" 'nil
 "C-k" 'company-select-previous
 "C-l" 'nil
 "C-l" 'company-select-next
 "C-;" 'nil
 "C-;" 'company-complete
 "C-;" 'company-complete

 "C-L" 'lsp-ui-doc-focus-frame
 ;; "C-:" 'lsp-ui-doc-focus-frame
 )

(general-define-key
 :keymaps '(
           lsp-ui-doc-frame-mode-map
            )
 "C-K" 'lsp-ui-doc-unfocus-frame
 )

(general-define-key
 :keymaps '(dired-mode-map)
 :states  '(normal visual)
 ;; "j" 'nil
 ";"      'nil
 )

(general-define-key
 :keymaps '(dired-mode-map)
 :states  '(normal visual)
 ;; "j" 'nil
 "H"      'dired-hide-dotfiles-mode
 ";"      'dired-find-file
 "j"      'dired-single-up-directory
 "r"      'dired-rifle
 )

(general-define-key
 :keymaps '(dired-mode-map)
 :states  '(normal visual)
 :prefix  "g"
 "R"      'dired-do-redisplay
 )

(general-define-key
 :keymaps '(dired-mode-map)
 :states  '(normal visual)
 :prefix  "h"
 "d"      'epa-dired-do-decrypt
 "e"      'epa-dired-do-encrypt
 "s"      'epa-dired-do-sign
 "v"      'epa-dired-do-verify
 )

(general-define-key
 :keymaps '(imenu-list-major-mode-map)
 "<C-return>" 'imenu-list-display-entry
 "M-RET"      'imenu-list-display-entry
 )

(general-define-key
 :keymaps '(counsel-ag-map
            counsel-git-grep-map
            counsel-grep-map
            counsel-imenu-map
            )
 "C-l" 'nil
 "C-l" 'ivy-next-line
 "C-S-l" 'ivy-call-and-recenter
 )

(general-define-key
 :keymaps '(lsp-command-map)
 "i"  '(:which-key "ivy/imenu"    :ignore t)
 "ii"  'lsp-ivy-workspace-symbol
 "im"  'lsp-ui-imenu

 "t"  '(:which-key "treemacs"     :ignore t)
 "ts" 'lsp-treemacs-symbols
 )

(general-define-key
 :keymaps '(magit-mode-map)
 :states  '(normal visual)
 "j" 'nil
 )

(general-define-key
 :keymaps '(magit-status-mode-map)
 "j" 'nil
 )

(general-define-key
 :keymaps '(magit-status-mode-map)
 :states  '(normal visual)
 "h" 'magit-log
 )

(general-define-key
 :keymaps 'magit-mode-map
 "h" 'magit-log
 "H" 'magit-log
 "j" 'evil-backward-char
 ;; "k" 'evil-previous-visual-line
 "l" 'evil-next-visual-line
 ;; ";" 'evil-forward-char
 "J" 'magit-status-jump
 )

(general-define-key
 :prefix "C-c"
 "L" 'org-store-link
 ;; "l" 'org-store-link
 "a" 'org-agenda
 "c" 'org-capture
 )

(eval-after-load "treemacs-evil"
  '(progn
     (general-define-key
      :keymaps '(evil-treemacs-state-map treemacs-mode-map)
      "h" 'evil-forward-char
      "j" 'treemacs-root-up
      "k" 'treemacs-previous-line
      "l" 'treemacs-next-line
      ";" 'treemacs-root-down
      )

     (general-define-key
      :keymaps 'treemacs-mode-map
      :states 'treemacs
      "l" 'nil
      )

     (general-define-key
      :keymaps 'treemacs-mode-map
      :states 'treemacs
      "h" 'evil-forward-char
      "j" 'treemacs-root-up
      "k" 'treemacs-previous-line
      "l" 'treemacs-next-line
      ";" 'treemacs-root-down
      )))

;; (evil-make-overriding-map undo-tree-visualizer-mode-map 'normal)
;; asdfasdf asdfasdf qweqwer adsfasdf adsfasdf qeqwer

;;(add-hook undo-tree-visualizer-mode-hook (define-key undo-tree-visualizer-mode-map "k" 'undo-tree-visualize-undo))
;; (general-define-key
;;  :keymaps '(undo-tree-visualizer-mode
;; 	    ;; undo-tree
;; 	    )

;; ;; undo-tree-visualizer-mode-map <down>
;; ;; undo-tree-visualizer-mode-map C-n
;; ;; undo-tree-visualizer-mode-map n

;; ;; undo-tree-visualizer-mode-map <up>
;; ;; undo-tree-visualizer-mode-map C-p
;; ;; undo-tree-visualizer-mode-map p

;;  ;; "j" 'nil
;;  "k" 'nil
;;  ;; "l" 'nil
;;  ;; ";" 'nil
;;  ;; "j" 'undo-tree-visualize-switch-branch-left        ;;; working
;;  ;; "j" 'undo-tree-visualize-switch-branch-left        ;;; working
;;  "k" 'undo-tree-visualize-undo
;;  ;; "l" 'undo-tree-visualize-redo
;;  ;; ";" 'undo-tree-visualize-switch-branch-right       ;;; working
;;  ;; Ctr-{p,n} working
;;  )
;; )
;; https://emacs.stackexchange.com/questions/44431/how-to-suppress-a-minor-modes-key-binding-in-only-certain-major-modes
;; (add-hook 'typo-mode-hook
;;           (lambda ()
;;             (when (and typo-mode (derived-mode-p 'markdown-mode))
;;               (let ((map (make-sparse-keymap)))
;;                 (set-keymap-parent map typo-mode-map)
;;                 (define-key map (kbd "`") 'self-insert-command)
;;                 (push `(typo-mode . ,map)
;;                       minor-mode-overriding-map-alist)))))

(general-define-key
 :keymaps '(writeroom-mode-map)
 "s-?"  'nil
 "M-m"   '(writeroom-toggle-mode-line :which-key "toggle-modeline")
 "C-M-<" 'writeroom-decrease-width
 "C-M->" 'writeroom-increase-width
 ;; "C-M-=" 'writeroom-adjust-width
 "C-M-=" '(writeroom-adjust-width :which-key "wr-with-=")
 )

(fb/leader-key
  "c"   '(                                              :which-key "comment"                          :ignore t)
  "cc"  '(evilnc-comment-operator                       :which-key "cmnt-operator"                    )
  "ci"  '(evilnc-toggle-invert-comment-line-by-line     :which-key "toggle-invert-cmnt-line-by-line"  )
  "cl"  '(evilnc-comment-or-uncomment-lines             :which-key "cmmnt-or-uncmnt-lines"            )
  "cp"  '(evilnc-comment-or-uncomment-paragraphs        :which-key "cmmnt-or-uncmnt-paragraphs"       )
  "cr"  '(comment-or-uncomment-region                   :which-key "cmmnt-or-uncmnt-region"           )
  "ct"  '(evilnc-quick-comment-or-uncomment-to-the-line :which-key "quick-cmmnt-or-uncmnt-to-the-line")
  "cy"  '(evilnc-copy-and-comment-lines                 :which-key "cp-and-cmnt-lines"                )

  "C"   '(                                              :which-key "command-log-mode"                 :ignore t)
  "CC"  '(command-log-mode                              :which-key "toggle-local"                     )
  "CB"  '(clm/open-command-log-buffer                   :which-key "show-clm-buffer"                  )
  "CG"  '(global-command-log-mode                       :which-key "toggle-global"                    )

  "d"   '(                                              :which-key "delete"                           :ignore t)
  "dw"  '(delete-trailing-whitespace                    :which-key "trailing-wsp"                     )

  "f"   '(                                              :which-key "fast/file"                        :ignore t)
  "fy"  '(fb/yank-buffer-filename                       :which-key "files"                            )
  "ff"  '(counsel-find-file                             :which-key "files"                            )
  "fs"  '(save-buffer                                   :which-key "save-buffer"                      )
  "fS"  '(save-some-buffers                             :which-key "save-some-buffer"                 )

  "g"   '(                                              :which-key "git"                              :ignore t)
  "gs"  '(magit-status                                  :which-key "status"                           )

  "i"   '(                                              :which-key "imenu"                            :ignore t)
  "ii"  '(imenu-list                                    :which-key "imenulist"                        )

  "j"   '(dired-jump                                    :which-key "dired"                            )

  "L"   '(lsp                                           :which-key "start lsp"                        )
  "l"   '(:keymap lsp-command-map :package lsp-mode     :which-key "lsp"                              )

  "n"   '(                                              :which-key "numbers"                          :ignore t)
  "n="  '(evil-numbers/inc-at-pt                        :which-key "add"                              )
  "n+"  '(evil-numbers/inc-at-pt                        :which-key "add"                              )
  "n-"  '(evil-numbers/dec-at-pt                        :which-key "sub"                              )

  "p"   '(projectile-command-map                        :which-key "projectile"                       )

  "r"   '(                                              :which-key "re-~"                             :ignore t)
  "rc"  '(fb/literate-recompile                         :which-key "recompile-emacs.d"                )
  "rf"  '(                                              :which-key "reformat"                         :ignore t)
  "rfh" '(fb/break-here                                 :which-key "break-here"                       )
  "rfc" '(fb/break-sub-sentence                         :which-key "break-sub"                        )
  "rfs" '(fb/break-sentence                             :which-key "break-sentence"                   )

  "rr"  '(redraw-display                                :which-key "redraw-display"                   )
  "rl"  '(fb/reload-config                              :which-key "reload init.el"                   )

  "t"   '(                                              :which-key "toggles"                          :ignore t)
  "ti"  '(imenu-list-smart-toggle                       :which-key "imenu"                            )
  "tl"  '(toggle-truncate-lines                         :which-key "truncate-lines"                   )
  "tn"  '(display-line-numbers-mode                     :which-key "line-numbers"                     )
  "tt"  '(counsel-load-theme                            :which-key "choose theme"                     )
  "tw"  '(whitespace-mode                               :which-key "whitespace"                       )
  "T"   '(                                              :which-key "toggles"                          :ignore t)
  "TW"  '(fb/toggle-which-key-sort-order                :which-key "whickKey-sort-order"              )

  "y"   '(                                              :which-key "yasnippets"                       :ignore t)
  "yy"  '(yas-insert-snippet                            :which-key "insert"                           )
  "yr"  '(yas-reload-all                                :which-key "reload-all"                       )

  "w"   '(writeroom-mode                                :which-key "writeroom-toggle"                 )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; orgmode-keybindings
;;;;
;;

(general-define-key
 :keymaps 'org-mode-map
 "C-'" 'nil
 )

(general-define-key
 :keymaps 'org-mode-map
"M-<return>"   'fb/org-meta-return
"M-S-<return>" 'org-insert-todo-subheading
 )

(fb/local-leader-key
  :keymaps 'org-mode-map
  :states  '(normal visual insert)

  "S"      '(org-insert-structure-template 'elisp :which-key "struc-temp"            )
  "s"      '(                                             :which-key "subtree"   :ignore t)
  "sn"     '(org-narrow-to-subtree                        :which-key "narrow"        )
  "sw"     '(widen                                        :which-key "widen"         )

  "t"      '(                                             :which-key "todo"      :ignore t)
  "tc"     '(org-todo                                     :which-key "cycle"         )
  "t SPC"  '(org-todo                                     :which-key "cycle"         )
  "tt"     '((lambda () (interactive)(org-todo 'todo))    :which-key "todo"          )
  "td"     '((lambda () (interactive)(org-todo 'done))    :which-key "done"          )
  "tx"     '((lambda () (interactive)(org-todo 'none))    :which-key "none"          )

  "x"      '(                                             :which-key "text"      :ignore t)
  "xb"     '((lambda () (interactive)(org-emphasize ?\*)) :which-key "bold"          )
  "xc"     '((lambda () (interactive)(org-emphasize ?\~)) :which-key "code"          )
  "xi"     '((lambda () (interactive)(org-emphasize ?\/)) :which-key "italic"        )
  "xr"     '((lambda () (interactive)(org-emphasize ?\ )) :which-key "clear"         )
  "xR"     '((lambda () (interactive)(org-emphasize ?\s)) :which-key "clear"         )
  "xs"     '((lambda () (interactive)(org-emphasize ?\+)) :which-key "strike-through")
  "xu"     '((lambda () (interactive)(org-emphasize ?\_)) :which-key "underline"     )
  "xv"     '((lambda () (interactive)(org-emphasize ?\=)) :which-key "verbatim"      )
  )

(general-define-key
 :keymaps 'org-read-date-minibuffer-local-map

 ;; "C-H"    'exit-minibuffer
 "C-j"    'nil

 "C-j"     '(lambda () (interactive) (org-eval-in-calendar '(calendar-backward-day              1)))
 "C-k"     '(lambda () (interactive) (org-eval-in-calendar '(calendar-backward-week             1)))
 "C-l"     '(lambda () (interactive) (org-eval-in-calendar '(calendar-forward-week              1)))
 "C-;"     '(lambda () (interactive) (org-eval-in-calendar '(calendar-forward-day               1)))

 ;; SHIFT or META is the same
 "C-S-j"   '(lambda () (interactive) (org-eval-in-calendar '(calendar-backward-month            1)))
 "C-S-k"   '(lambda () (interactive) (org-eval-in-calendar '(calendar-backward-year             1)))
 "C-S-l"   '(lambda () (interactive) (org-eval-in-calendar '(calendar-forward-year              1)))
 "C-:"     '(lambda () (interactive) (org-eval-in-calendar '(calendar-forward-month             1)))
 "C-M-j"   '(lambda () (interactive) (org-eval-in-calendar '(calendar-backward-month            1)))
 "C-M-k"   '(lambda () (interactive) (org-eval-in-calendar '(calendar-backward-year             1)))
 "C-M-l"   '(lambda () (interactive) (org-eval-in-calendar '(calendar-forward-year              1)))
 "C-M-;"   '(lambda () (interactive) (org-eval-in-calendar '(calendar-forward-month             1)))

 ;; scrolling with CTRL + SHIFT + META
 "C-M-S-j" '(lambda () (interactive) (org-eval-in-calendar '(calendar-scroll-right              1)))
 "C-M-S-k" '(lambda () (interactive) (org-eval-in-calendar '(calendar-scroll-right-three-months 1)))
 "C-M-S-l" '(lambda () (interactive) (org-eval-in-calendar '(calendar-scroll-left-three-months  1)))
 "C-M-:"   '(lambda () (interactive) (org-eval-in-calendar '(calendar-scroll-left               1)))
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; keybindings-outline
;;;;
;;

(define-prefix-command 'cm-map nil "Outline-")

(define-key cm-map "\M-q" 'outline-hide-sublevels)            ;;; Hide everything but the top-level headings
(define-key cm-map "\M-t" 'outline-hide-body)                 ;;; Hide everything but headings (all body lines)
(define-key cm-map "\M-o" 'outline-hide-other)                ;;; Hide other branches
(define-key cm-map "\M-c" 'outline-hide-entry)                ;;; Hide this entry's body
(define-key cm-map "\M-l" 'outline-hide-leaves)               ;;; Hide body lines in this entry and sub-entries
(define-key cm-map "\M-d" 'outline-hide-subtree)              ;;; Hide everything in this entry and sub-entries

(define-key cm-map "\M-a" 'outline-show-all)                  ;;; Show (expand) everything
(define-key cm-map "\M-e" 'outline-show-entry)                ;;; Show this heading's body
(define-key cm-map "\M-i" 'outline-show-children)             ;;; Show this heading's immediate child sub-headings
(define-key cm-map "\M-k" 'outline-show-branches)             ;;; Show all sub-headings under this heading
(define-key cm-map "\M-s" 'outline-show-subtree)              ;;; Show (expand) everything in this heading & below

(define-key cm-map "\M-u" 'outline-up-heading)                ;;; Up
(define-key cm-map "\M-n" 'outline-next-visible-heading)      ;;; Next
(define-key cm-map "\M-p" 'outline-previous-visible-heading)  ;;; Previous
(define-key cm-map "\M-f" 'outline-forward-same-level)        ;;; Forward - same level
(define-key cm-map "\M-b" 'outline-backward-same-level)       ;;; Backward - same level

(global-set-key "\M-o" cm-map)

(setq lsp-gopls-codelens nil)
