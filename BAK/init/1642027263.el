(require 'package)

(setq comp-deferred-compilation nil)

(setq package-archives nil)

(setq package-enable-at-startup nil)
(package-initialize 'no-activate)
(eval-when-compile
  (require 'use-package))

(let ((gc-cons-threshold (* 50 1000 1000))
      (gc-cons-percentage 0.6)
      (file-name-handler-alist nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; tangling
;;;;
;;

(use-package ox)
(use-package ob-tangle)

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
                ;;;; make a bakup of the current initFile
                (copy-file user-init-file (concat user-emacs-directory "/BAK/init/" (format-time-string "%s") ".el") t)
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
  :custom (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config (counsel-mode 1)
  )

(use-package swiper)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; themes
;;;;
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; themes-colors
;;;;
;;

(defvar fb-doom-dracula-colors
  ;; name          default   256       16
  '(:bg         '("#282a36" "#262626"  nil           )
    :bg-alt     '("#1E2029" "#1c1c1c"  nil           )
    :base0      '("#1E2029" "#1c1c1c" "black"        )
    :base1      '("#282a36" "#1e1e1e" "brightblack"  )
    :base2      '("#373844" "#2e2e2e" "brightblack"  )
    :base3      '("#44475a" "#262626" "brightblack"  )
    :base4      '("#565761" "#3f3f3f" "brightblack"  )
    :base5      '("#6272a4" "#525252" "brightblack"  )
    :base6      '("#b6b6b2" "#bbbbbb" "brightblack"  )
    :base7      '("#ccccc7" "#cccccc" "brightblack"  )
    :base8      '("#f8f8f2" "#dfdfdf" "white"        )
    :fg         '("#f8f8f2" "#ffffff" "white"        )
    :fg-alt     '("#e2e2dc" "#bfbfbf" "brightwhite"  )

    :grey       '("#565761" "#3f3f3f" "brightblack"  )
    :red        '("#ff5555" "#ff6655" "red"          )
    :orange     '("#ffb86c" "#ffbb66" "brightred"    )
    :green      '("#50fa7b" "#55ff77" "green"        )
    :teal       '("#0189cc" "#0088cc" "brightgreen"  )
    :yellow     '("#f1fa8c" "#ffff88" "yellow"       )
    :blue       '("#61bfff" "#66bbff" "brightblue"   )
    :dark-blue  '("#0189cc" "#0088cc" "blue"         )
    :magenta    '("#ff79c6" "#ff77cc" "magenta"      )
    :violet     '("#bd93f9" "#bb99ff" "brightmagenta")
    :cyan       '("#8be9fd" "#88eeff" "brightcyan"   )
    :dark-cyan  '("#8be9fd" "#88eeff" "cyan"         )
   ) "Definition of all dracula-colors.")

(setq fb-doom-colors fb-doom-dracula-colors)

(defun fb*getDefaultColorValue (color)
  "get the hex color value from currenty used colorscheme"
            (nth 0 (nth 1 (plist-get fb-doom-colors color))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; themes-colorschemes
;;;;
;;

;; (load-theme 'deeper-blue)
;; (load-theme 'wombat)

(use-package solaire-mode
  :hook (after-init . solaire-global-mode))

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
 ;; (load-theme 'doom-dracula t)                     ;; +
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

(add-to-list 'custom-theme-load-path (expand-file-name "themes/themes" user-emacs-directory))
;; (setq doom-theme 'fb-doom)
(load-theme 'fb-doom t)

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

(setq
       evil-search-module 'evil-search
       ;; evil-magic 'very-magic
     )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; global
;;;;
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; global-packages-local
;;;;
;;

;; (add-to-list 'load-path "~/.emacs.d/global/packages-local")
(add-to-list 'load-path (expand-file-name "global/packages-local" user-emacs-directory))

(require 'core-funcs)

(require 'core-toggle)

(use-package dart-snippets)

(use-package ox-ipynb
  ;; :straight (:host github :repo "jkitchin/ox-ipynb")
  :after ox)

(require 'core-transient-state)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; global-misc
;;;;
;;

(setq calendar-week-start-day 1)

(copy-face font-lock-constant-face 'calendar-iso-week-face)
(set-face-attribute 'calendar-iso-week-face nil :height 1.0)
(setq calendar-intermonth-text
      '(propertize
        (format "%2d"
                (car
                 (calendar-iso-from-absolute
                  (calendar-absolute-from-gregorian (list month day year)))))
        'font-lock-face 'calendar-iso-week-face))

(copy-face 'default 'calendar-iso-week-header-face)
(set-face-attribute 'calendar-iso-week-header-face nil :height 1.0)
(setq calendar-intermonth-header
      (propertize "W"
                  'font-lock-face 'calendar-iso-week-header-face))

(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(setq ediff-split-window-function (quote split-window-horizontally))

;; (setq inhibit-startup-screen t )    ;;; inhibit startup screen
(setq inhibit-startup-message t )      ;;; inhibit startup message
(setq initial-scratch-message "")      ;;; print a default message in the empty scratch buffer opened at startup
;; (setq ring-bell-function 'ignore )     ;;; silent bell when you make a mistake
;; (setq visible-bell t)                  ;;; visible bell when you make a mistake - doom-modeline takes care
(setq coding-system-for-read 'utf-8 )  ;;; use utf-8 by default
(setq coding-system-for-write 'utf-8 )
(setq sentence-end-double-space nil)   ;;; sentence SHOULD end with only a point.
(setq fill-column 80)                  ;;; toggle wrapping text at the 80th character

(defvar fb/domainName
"thesym.site"
  "my domain")

(setq enable-local-variables :safe)

(setf safe-local-variable-values (cl-list*
                                  '(org-content . 2)
                                  '(org-content . 3)
                                  '(org-export-with-section-numbers . nil)
                                  '(org-export-with-toc . nil)
                                  '(org-fb-re-reveal-talk-url . "https://keynotes.thesym.site/dartflutter")
                                  '(org-fb-re-reveal-talk-url-show-anchor-on-title-slide . nil)
                                  '(org-fb-re-reveal-talk-url-show-qrCode-and-anchor-on-last-slide . t)
                                  '(org-fb-re-reveal-talk-url-show-qrCode-on-title-slide . nil)
                                  '(org-re-reveal-single-file . t)
                                  '(org-re-reveal-title-slide . "~/.emacs.d/orgmode/revealjs/revealjs.titleSlide.html")
                                  safe-local-variable-values))

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

(server-start)

(defalias 'yes-or-no-p 'y-or-n-p)

(use-package wrap-region
  :config
  (wrap-region-global-mode t)
  )

;; (setq
;;  split-width-threshold 0
;;  split-height-threshold nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; global-packages
;;;;
;;

(use-package align)

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

(use-package calfw
  :config
  (custom-set-faces
   `(cfw:face-title              ((t :foreground ,(fb*getDefaultColorValue :magenta)            :height 2.0                           :weight bold :inherit variable-pitch    )))
   `(cfw:face-header             ((t :foreground ,(fb*getDefaultColorValue :base6)                                                    :weight bold                            )))
   `(cfw:face-sunday             ((t :foreground ,(fb*getDefaultColorValue :base6)  :background ,(fb*getDefaultColorValue :bg)        :weight bold                            )))
   `(cfw:face-saturday           ((t :foreground ,(fb*getDefaultColorValue :base6)  :background ,(fb*getDefaultColorValue :bg)        :weight bold                            )))
   `(cfw:face-holiday            ((t :foreground ,(fb*getDefaultColorValue :base2)  :background ,(fb*getDefaultColorValue :orange)    :weight bold                            )))
   `(cfw:face-grid               ((t :foreground ,(fb*getDefaultColorValue :fg)                                                                                               )))
   `(cfw:face-default-content    ((t :foreground ,(fb*getDefaultColorValue :red)                                                                                              )))
   `(cfw:face-periods            ((t :foreground ,(fb*getDefaultColorValue :red)                                                                                              )))
   `(cfw:face-day-title          ((t                                                :background ,(fb*getDefaultColorValue :bg)                                                )))
   `(cfw:face-default-day        ((t                                                                                                  :weight bold :inherit cfw:face-day-title)))
   `(cfw:face-annotation         ((t :foreground ,(fb*getDefaultColorValue :bg-alt)                                                                :inherit cfw:face-day-title)))
   `(cfw:face-disable            ((t :foreground ,(fb*getDefaultColorValue :blue)                                                                  :inherit cfw:face-day-title)))
   `(cfw:face-today-title        ((t                                                :background ,(fb*getDefaultColorValue :dark-blue) :weight bold                            )))
   `(cfw:face-today              ((t                                                :background ,(fb*getDefaultColorValue :bg)        :weight bold                            )))
   `(cfw:face-select             ((t                                                :background ,(fb*getDefaultColorValue :violet)                                            )))
   `(cfw:face-toolbar            ((t :foreground ,(fb*getDefaultColorValue :bg)     :background ,(fb*getDefaultColorValue :bg)                                                )))
   `(cfw:face-toolbar-button-off ((t :foreground ,(fb*getDefaultColorValue :bg)                                                       :weight bold                            )))
   `(cfw:face-toolbar-button-on  ((t :foreground ,(fb*getDefaultColorValue :bg)                                                       :weight bold                            ))))
  (setq
        ;; cfw:fchar-junction ?╋
        cfw:fchar-junction ?\u254B

        ;; cfw:fchar-vertical-line ?┃
        cfw:fchar-vertical-line ?\u2503

        ;; cfw:fchar-horizontal-line ?━
        cfw:fchar-horizontal-line ?\u2501

        ;; cfw:fchar-left-junction ?┣
        cfw:fchar-left-junction ?\u2523

        ;; cfw:fchar-right-junction ?┫
        cfw:fchar-right-junction ?\u252B

        ;; cfw:fchar-top-junction ?┳
        cfw:fchar-top-junction ?\u2533

        ;; cfw:fchar-top-left-corner ?┏
        cfw:fchar-top-left-corner ?\u250F

        ;; cfw:fchar-top-right-corner ?┓
        cfw:fchar-top-right-corner ?\u2513
        )
  )

(defun fb/open-calendar ()
  (interactive)
  (cfw:open-calendar-buffer
   :contents-sources
   (list
    (cfw:org-create-source                                  (fb*getDefaultColorValue :base8)) ;; orgmode source
    ;; (cfw:howm-create-source                                 (fb*getDefaultColorValue :base7)) ;; howm source
    ;; (cfw:cal-create-source                                  (fb*getDefaultColorValue :base6)) ;; diary source
    ;; (cfw:ical-create-source "Moon" "~/moon.ics"             (fb*getDefaultColorValue :base5)) ;; ICS source1
    ;; (cfw:ical-create-source "gcal" "https://..../basic.ics" (fb*getDefaultColorValue :base4  )) ;; google calendar ICS
    )))

(use-package command-log-mode)

(use-package dired
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :custom ((dired-listing-switches "-agho --group-directories-first"))
  :config (setq dired-dwim-target t)
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

(use-package string-inflection)

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

(use-package origami
  ;; :hook
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
          treemacs-width                         45
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
    which-key-allow-imprecise-window-fit nil
    ;; which-key-popup-type 'minibuffer
    which-key-popup-type 'side-window
    ;; which-key-popup-type 'frame
    which-key-separator " "
    ;; which-key-use-C-h-commands nil
    which-key-use-C-h-commands t
    which-key-sort-order 'which-key-description-order
  ))

;; which-key-sort-order 'which-key-key-order-alpha

;; which-key-allow-imprecise-window-fit t

;; which-key-popup-type 'side-window
;; which-key-popup-type 'frame
;; which-key-popup-type 'custom
;; which-key-custom-show-popup-function

;; which-key-side-window-max-height
;; which-key-min-display-lines

;; which-key-use-C-h-commands t

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

;; modified function from http://emacswiki.org/emacs/AlignCommands
(defun spacemacs/align-repeat (start end regexp &optional justify-right after)
  "Repeat alignment with respect to the given regular expression.
If JUSTIFY-RIGHT is non nil justify to the right instead of the
left. If AFTER is non-nil, add whitespace to the left instead of
the right."
  (interactive "r\nsAlign regexp: ")
  (let* ((ws-regexp (if (string-empty-p regexp)
                        "\\(\\s-+\\)"
                      "\\(\\s-*\\)"))
         (complete-regexp (if after
                              (concat regexp ws-regexp)
                            (concat ws-regexp regexp)))
         (group (if justify-right -1 1)))

    (unless (use-region-p)
      (save-excursion
        (while (and
                (string-match-p complete-regexp (thing-at-point 'line))
                (= 0 (forward-line -1)))
          (setq start (point-at-bol))))
      (save-excursion
        (while (and
                (string-match-p complete-regexp (thing-at-point 'line))
                (= 0 (forward-line 1)))
          (setq end (point-at-eol)))))

    (align-regexp start end complete-regexp group 1 t)))

;; Modified answer from http://emacs.stackexchange.com/questions/47/align-vertical-columns-of-numbers-on-the-decimal-point
(defun spacemacs/align-repeat-decimal (start end)
  "Align a table of numbers on decimal points and dollar signs (both optional)"
  (interactive "r")
  (require 'align)
  (align-region start end nil
                '((nil (regexp . "\\([\t ]*\\)\\$?\\([\t ]+[0-9]+\\)\\.?")
                       (repeat . t)
                       (group 1 2)
                       (spacing 1 1)
                       (justify nil t)))
                nil))

(defmacro spacemacs|create-align-repeat-x (name regexp &optional justify-right default-after)
  (let* ((new-func (intern (concat "spacemacs/align-repeat-" name)))
         (new-func-defn
          `(defun ,new-func (start end switch)
             (interactive "r\nP")
             (let ((after (not (eq (if switch t nil) (if ,default-after t nil)))))
               (spacemacs/align-repeat start end ,regexp ,justify-right after)))))
    (put new-func 'function-documentation "Created by `spacemacs|create-align-repeat-x'.")
    new-func-defn))

(spacemacs|create-align-repeat-x "comma"              ","        nil  t )
(spacemacs|create-align-repeat-x "semicolon"          ";"        nil  t )
(spacemacs|create-align-repeat-x "colon"              ":"        nil  t )
(spacemacs|create-align-repeat-x "equal"              "="               )
(spacemacs|create-align-repeat-x "math-oper"          "[+\\-*/]"        )
(spacemacs|create-align-repeat-x "percent"            "%"               )
(spacemacs|create-align-repeat-x "ampersand"          "&"               )
(spacemacs|create-align-repeat-x "bar"                "|"               )
(spacemacs|create-align-repeat-x "left-paren"         "("               )
(spacemacs|create-align-repeat-x "right-paren"        ")"             t )
(spacemacs|create-align-repeat-x "left-curly-brace"   "{"               )
(spacemacs|create-align-repeat-x "right-curly-brace"  "}"             t )
(spacemacs|create-align-repeat-x "left-square-brace"  "\\["             )
(spacemacs|create-align-repeat-x "right-square-brace" "\\]"           t )
(spacemacs|create-align-repeat-x "backslash"          "\\\\"            )

(defun spacemacs/uniquify-lines ()
  "Remove duplicate adjacent lines in a region or the current buffer"
  (interactive)
  (save-excursion
    (save-restriction
      (let* ((region-active (or (region-active-p) (evil-visual-state-p)))
             (beg (if region-active (region-beginning) (point-min)))
             (end (if region-active (region-end) (point-max))))
        (goto-char beg)
        (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
          (replace-match "\\1"))))))

(defun spacemacs/sort-lines (&optional reverse)
  "Sort lines in a region or the current buffer.
A non-nil argument sorts in reverse order."
  (interactive "P")
  (let* ((region-active (or (region-active-p) (evil-visual-state-p)))
         (beg (if region-active (region-beginning) (point-min)))
         (end (if region-active (region-end) (point-max))))
    (sort-lines reverse beg end)))

(defun spacemacs/sort-lines-reverse ()
  "Sort lines in reverse order, in a region or the current buffer."
  (interactive)
  (spacemacs/sort-lines -1))

(defun spacemacs/sort-lines-by-column (&optional reverse)
  "Sort lines by the selected column,
using a visual block/rectangle selection.
A non-nil argument sorts in REVERSE order."
  (interactive "P")
  (if (and
       ;; is there an active selection
       (or (region-active-p) (evil-visual-state-p))
       ;; is it a block or rectangle selection
       (or (eq evil-visual-selection 'block) (eq rectangle-mark-mode t))
       ;; is the selection height 2 or more lines
       (>= (1+ (- (line-number-at-pos (region-end))
                  (line-number-at-pos (region-beginning)))) 2))
      (sort-columns reverse (region-beginning) (region-end))
    (error
     "Sorting by column requires a block/rect selection on 2 or more lines.")))

(defun spacemacs/sort-lines-by-column-reverse ()
  "Sort lines by the selected column in reverse order,
using a visual block/rectangle selection."
  (interactive)
  (spacemacs/sort-lines-by-column -1))

(defun fb*transform-square-brackets-to-round-ones(string-to-transform)
  "Transforms [ into ( and ] into ), other chars left unchanged."
  (concat
  (mapcar #'(lambda (c) (if (equal c ?\[) ?\( (if (equal c ?\]) ?\) c))) string-to-transform))
  )

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

(defun fb/reload-dir-locals-current-buffer ()
  "reload dir-locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defun fb/reload-dir-locals-all-directory-buffer ()
  "For every buffer with the same `default-directory` as the
current buffer's, reload dir-locals."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir))
        (my-reload-dir-locals-for-current-buffer)))))

(defun fb/titlecase-word ()
  (interactive)
  (progn
    (evil-backward-word-begin)
    (capitalize-word 1)
    ))

(defun fb/downcase-word ()
  (interactive)
  (progn
    (evil-backward-word-begin)
    (downcase-word 1)
    ))

(defun fb/upcase-word ()
  (interactive)
  (progn
    (evil-backward-word-begin)
    (upcase-word 1)
    ))

(defun fb/describe-last-function()
  (interactive)
  (describe-function last-command))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; languages
;;;;
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; languages-misc
;;;;
;;

(use-package company
  :after lsp-mode
  :hook (
         (after-init . global-company-mode)
       )
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
)

(defun fb*default-company-backends-h ()
  "set default company-backends"
  (set (make-local-variable 'company-backends)
       '((company-files company-capf company-yasnippet)
         (company-dabbrev-code company-keywords)
          company-dabbrev
          )))

(use-package company-box
  :init
  (setq company-box-icons-alist 'company-box-icons-all-the-icons)
  :hook (company-mode . company-box-mode)
  :config (setq company-box-doc-enable t)
   )

(kbd "M-h") #'company-box-doc-manually

(defun fb/company-complete-selection ()
  "Insert the selected candidate or the first if no one is selected."
  (interactive)
  (if company-selection
      (company-complete-selection)
    (company-complete-number 1)))

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

(add-hook 'yas-minor-mode-hook
          (lambda ()
            (yas-activate-extra-mode 'fundamental-mode)))

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  ;; :defer t
  ;; :commands flycheck-list-errors
  :after lsp-mode
  ;; :init
  ;; (global-flycheck-mode)
  )

;; toggle flycheck window
(defun fb/toggle-flycheck-error-buffer ()
  "toggle a flycheck error buffer."
  (interactive)
  (if (string-match-p "Flycheck errors" (format "%s" (window-list)))
      (dolist (w (window-list))
        (when (string-match-p "*Flycheck errors*" (buffer-name (window-buffer w)))
          (delete-window w)
          ))
    (flycheck-list-errors)
    )
  )
(defun spacemacs/goto-flycheck-error-list ()
  "Open and go to the error list buffer."
  (interactive)
  (if (flycheck-get-error-list-window)
      (switch-to-buffer flycheck-error-list-buffer)
    (progn
      (flycheck-list-errors)
      (switch-to-buffer-other-window flycheck-error-list-buffer))))

(defvar-local fb*flycheck-local-cache nil)

(defun fb*flycheck-checker-get (fn checker property)
  (or (alist-get property (alist-get checker fb*flycheck-local-cache))
      (funcall fn checker property)))

(advice-add 'flycheck-checker-get :around 'fb*flycheck-checker-get)

(defadvice flycheck-error-list-refresh (around shrink-error-list activate)
  ad-do-it
  (-when-let (window (flycheck-get-error-list-window t))
    (with-selected-window window
      (fit-window-to-buffer window 30))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; languages-lsp
;;;;
;;

(defun fb*lsp-mode-setup-h ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (
         (lsp-mode              . fb*lsp-mode-setup-h)
         (lsp-mode              . lsp-enable-which-key-integration)
         (lsp-before-initialize . (lambda () (setq lsp-enable-snippet t)))
         )
  ;; :init
  ;; (setq lsp-keymap-prefix "C-c l")  ;; or 'c-l', 's-l'
  ;; (setq lsp-keymap-prefix "SPC l")  ;; shows named-prefixes; prevent usage of "SPC"
  :config
  (setq lsp-prefer-flymake nil)
  (setq lsp-completion-provider :none)
  ;; (setq lsp-eldoc-enble-hover t)
  )

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  ;; :commands lsp-ui-mode
  :custom
  ;; (lsp-ui-sideline-show-diagnostics t)
  ;; (lsp-ui-sideline-show-hover t)
  ;; (lsp-ui-sideline-show-code-actions t)
  ;; (lsp-ui-sideline-update-mode t)
  (lsp-ui-sideline-delay 0)

  ;; (lsp-ui-doc-position 'top)
  ;; (lsp-ui-doc-position 'bottom)
  (lsp-ui-doc-position 'at-point)

  ;; (lsp-ui-flycheck-enable t)
  ;; (lsp-ui-flycheck-list-position 'right)
  ;; (lsp-ui-flycheck-live-reporting t)

  ;; (lsp-ui-doc-border "white")
  ;; (lsp-ui-doc-border (fb*getDefaultColorValue :orange))
  (lsp-ui-doc-border (fb*getDefaultColorValue :base5))
  (lsp-ui-doc-max-width 150)
  (lsp-ui-doc-max-height 13)
  (lsp-ui-doc-use-childframe t)
  (lsp-ui-doc-use-webkit t)
  )

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol
  )

(use-package lsp-origami
  :hook (lsp-after-open . lsp-origami-try-enable)
  )
;; (add-hook 'lsp-after-open-hook #'lsp-origami-try-enable)

(use-package lsp-treemacs
  :commands lsp-treemacs-errors-list
  :after lsp
  ;; :config (lsp-treemacs-sync-mode 1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; languages-dap
;;;;
;;

(use-package dap-mode
;;   :straight t
  :custom
  ;;;;;;
  ;; (lsp-enable-dap-auto-configure t)
  ;; (dap-auto-configure-features '(locals sessions tooltip))
  ;; ;; (dap-auto-configure-features '(
  ;;                                ;; breakpoints
  ;;                                ;; controls
  ;;                                ;; debugging
  ;;                                ;; expressions
  ;;                                ;; locals
  ;;                                ;; repl
  ;;                                ;; sessions
  ;;                                ;; tooltip
  ;;                                ;; ))
  ;;;;;;
  (lsp-enable-dap-auto-configure nil) ;;;; needs :config dap-ui-mode 1
  :config
  (dap-ui-mode 1)                     ;;;; needs :custom lsp-enable.. nil
  ;; (dap-tooltip-mode 1)
  )

(add-hook 'dap-stopped-hook
          (lambda (arg) (call-interactively #'dap-hydra)))

(defvar fb*dap-enable-mouse-support t
  "If non-nil, enable `dap-mode''s mouse support.")

(spacemacs|add-toggle dap-mouse
  :status dap-tooltip-mode
  :on (progn (dap-tooltip-mode)
             (tooltip-mode))
  :off (progn (dap-tooltip-mode -1)
              (tooltip-mode -1))
  :documentation "Enable mouse support in DAP mode.")

(when fb*dap-enable-mouse-support
  (spacemacs/toggle-dap-mouse-on))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; languages-c#
;;;;
;;

(use-package csharp-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; languages-elisp
;;;;
;;

(dolist (fn '(
              outline-minor-mode
              fb*default-company-backends-h
	          company-mode
              ))
  (add-hook 'emacs-lisp-mode-hook fn))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; languages-dart
;;;;
;;

(use-package dart-mode
  :hook (
         (dart-mode . flutter-test-mode             )
         (dart-mode . company-mode                  )
         (dart-mode . lsp-deferred                  )
         (dart-mode . fb*default-company-backends-h )
         (dart-mode . fb*set-flutter-sdk-dir        )
         (dart-mode . fb|lsp-dart-install-save-hooks)
         )
  :config (setq
           lsp-dart-dap-flutter-hot-reload-on-save  t
           lsp-dart-dap-flutter-hot-restart-on-save t
           )
  )

(defun fb*set-flutter-sdk-dir ()
  (direnv-update-environment)
  (setq lsp-dart-flutter-sdk-dir
        (string-trim-right (shell-command-to-string "echo $FLUTTER_SDK")))
  )

(defun fb|lsp-dart-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t))

(use-package flutter
  :after dart-mode
  :bind (:map dart-mode-map
              ("C-M-x" . #'flutter-run-or-hot-reload))
  ;; :custom
  ;; (flutter-sdk-path "/run/current-system/sw/bin/flutter")
  )

(use-package lsp-dart
  :after lsp
  :hook (dart-mode . lsp)
  :config (setq
           lsp-dart-dap-flutter-hot-reload-on-save t
           lsp-dart-show-todos t                      ;;;; Whether to show TODOs in the Problems list.
           )
  )

;; (with-eval-after-load 'lsp-dart
;;   (dap-dart-setup))

(use-package hover)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; languages-dockerfile
;;;;
;;

(use-package dockerfile-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; languages-golang
;;;;
;;

(use-package go-mode
  :hook (
         (go-mode         . company-mode)
         (go-mode         . lsp-deferred)
         (go-mode         . fb|lsp-go-install-save-hooks)
         (go-mode         . fb*default-company-backends-h)
         (go-dot-mod-mode . fb*default-company-backends-h)
         )
  )

(defun fb|lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(use-package flycheck-golangci-lint
  :hook (go-mode . flycheck-golangci-lint-setup)
  )

(add-hook 'lsp-managed-mode-hook
          (lambda ()
            (when (derived-mode-p 'go-mode)
              (setq fb*flycheck-local-cache '((lsp . ((next-checkers . (golangci-lint)))))))))

(load-file (expand-file-name "languages/golang/config.el" user-emacs-directory))
(load-file (expand-file-name "languages/golang/funcs.el" user-emacs-directory))

(defun fb/golangDashUnused ()
  "copy word under cursor and prepend '_ = ' - yanking has to be done MANUALLY afterwards"
    (interactive)
    (message (kill-new (concat "_ = " (thing-at-point 'word 'no-properties))))
    )

(use-package go-gen-test)

(use-package gotest)

(spacemacs|add-toggle go-test-testify-for-testing
  :documentation "Enable testify-test."
  :status go-use-testify-for-testing
  :on  (setq go-use-testify-for-testing t)
  :off (setq go-use-testify-for-testing nil)
  )

(spacemacs|add-toggle go-test-verbose
  :documentation "Enable verbose test output."
  :status go-test-verbose
  :on (setq go-test-verbose t)
  :off (setq go-test-verbose nil)
  )

(defvar fb*go-test-benchmark-p nil
"Provide the status of go-test-Benchmark.")
(spacemacs|add-toggle go-test-benchmark
  :documentation "Enable benchmark-tests."
  :status fb*go-test-benchmark-p
  :on  (progn (setq go-use-test-args "-bench=.") (setq fb*go-test-benchmark-p t  ))
  :off (progn (setq go-use-test-args ""        ) (setq fb*go-test-benchmark-p nil))
  )

(defvar fb*go-test-coverage-p nil
"Provide the status of go-test-coverage.")
(spacemacs|add-toggle go-test-coverage
  :documentation "Enable test coverage."
  :status fb*go-test-coverage-p
  :on  (progn (setq go-use-test-args "-cover") (setq fb*go-test-coverage-p t  ))
  :off (progn (setq go-use-test-args ""      ) (setq fb*go-test-coverage-p nil))
  )

(with-eval-after-load 'lsp-mode
   (lsp-register-custom-settings '(
      ("gopls.completeUnimported" t t)
      ("gopls.staticcheck" t t)
      ("gopls.gofumpt" t t)
      )))

(setq lsp-gopls-codelens nil)

(use-package dap-go
  ;; :after dap
  :config
  (dap-go-setup)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; graphviz-mermaid
;;;;
;;

(use-package graphviz-dot-mode
  )

(use-package company-graphviz-dot
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; languages-json
;;;;
;;

(dolist (fn '(
              fb*default-company-backends-h
              company-mode
              lsp-deferred
              ))
  (progn
    (add-hook 'json-mode-hook fn)
    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; languages-jsonnet
;;;;
;;

(use-package jsonnet-mode )

(dolist (fn '(
              fb*default-company-backends-h
              company-mode
              ;; lsp-deferred
              ))
  (progn
    (add-hook 'jsonnet-mode-hook fn)
    ))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; languages-kotlin
;;;;
;;

(use-package kotlin-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; languages-mermaid
;;;;
;;

(use-package mermaid-mode
  :mode "\\.mmd\\'"
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; languages-nix
;;;;
;;

(use-package nix-mode
  :mode "\\.nix\\'"
  :hook (
         (nix-mode . company-mode)
         (nix-mode . lsp-deferred)
         (nix-mode . fb*default-company-backends-h)
         )
  )

(add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
(lsp-register-client
 (make-lsp-client :new-connection (lsp-stdio-connection '("rnix-lsp"))
                  :major-modes '(nix-mode)
                  :server-id 'nix)
 )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; languages-protobuf
;;;;
;;

(use-package protobuf-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; languages-python
;;;;
;;

(use-package python-mode
  :config
  (setq python-indent-guess-indent-offset t
        python-indent-guess-indent-offset-verbose nil ;;;; prevent warning on opening a python-buffer
        )
  )

(use-package lsp-pyright
  :custom
  (lsp-pyright-auto-import-completions nil)
  :config
  (lsp-pyright-auto-search-paths t)
  ;; (lsp-pyright-diagnostic-mode "openFilesOnly")
  (lsp-pyright-diagnostic-mode "workspace")
  (lsp-pyright-disable-language-services nil)
  (lsp-pyright-disable-organize-imports nil)
  ;; (lsp-pyright-extra-paths)
  ;; (lsp-pyright-log-level)
  (lsp-pyright-typechecking-mode "off")
  ;; (lsp-pyright-typechecking-mode "basic")
  ;; (lsp-pyright-typechecking-mode "strict")
  ;; (lsp-pyright-typeshed-paths)
  (lsp-pyright-use-library-code-for-types nil)
  ;; (lsp-pyright-venv-path)

  :hook (
         (python-mode . company-mode)
         (python-mode . (lambda () (require 'lsp-pyright) (lsp-deferred)))
         (python-mode . fb*default-company-backends-h)
         )
  ;; :config (setq lsp-pyright-use-library-code-for-types t)
  )

(use-package dap-python
  ;; :after dap
  :custom (dap-python-debugger 'debugpy)
  )

(use-package blacken
  :hook (
         (python-mode . blacken-mode)
         )
  :config (setq
           blacken-only-if-project-is-blackened t  ;;;; only blacken if pyproject.toml contains [tool.black]
           blacken-allow-py36                   t  ;;;; Allow using Python 3.6-only syntax on all input files.
           blacken-line-length               80 ;;;; Max line length enforced by blacken.
           ;; blacken-skip-string-normalization    ;;;; Don't normalize string quotes or prefixes.
           ;; blacken-fast-unsafe                  ;;;; Skips temporary sanity checks.
           )
  )

(use-package python-pytest
  :after python
  )

(use-package vyper-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; languages-rust
;;;;
;;

(use-package rustic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; languages-solidity
;;;;
;;

(use-package solidity-mode
  :hook
         (solidity-mode         . fb*solidity-company-backends-h)
  )

(use-package company-solidity)

(defun fb*solidity-company-backends-h ()
  "set solidity company-backends"
  (set (make-local-variable 'company-backends)
       '((company-solidity company-files company-capf company-yasnippet)
         (company-dabbrev-code company-keywords)
          company-dabbrev
          )))

(use-package solidity-flycheck
  :config
  (setq solidity-flycheck-solc-checker-active t
        ;; solidity-flycheck-solium-checker-active t
        )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; languages-swift
;;;;
;;

(use-package swift-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; languages-typeScript
;;;;
;;

(use-package typescript-mode
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2)
  ;; (require 'dap-node)
  (use-package dap-node)
  (dap-node-setup)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; languages-web
;;;;
;;

(dolist (fn '(
              fb*default-company-backends-h
              company-mode
              lsp-deferred
              ))
  (progn
    (add-hook 'css-mode-hook   fn)
    (add-hook 'mhtml-mode-hook fn)
    (add-hook 'web-mode-hook   fn)
    ))

(use-package sass-mode)

(use-package web-mode
   :mode (
          "\\.html\\'"
          "\\.tpl\\'"                                     ;;;; gotemplate
          )
  :init (emmet-mode)
  :config (setq
           web-mode-enable-auto-expanding t               ;;;; enable d/ expanding
           web-mode-engines-alist '(("go" . "\\.tpl\\'")) ;;;; enable go rendering in tpl
           )
  )

(defun fb|web-mode-hook ()
  "Hooks for Web mode. Add a local hook which set the engine to the one specified by
   `fb-web-mode-engine` local variable."
  (add-hook
   'hack-local-variables-hook
   (lambda ()
     (if (boundp 'fb-web-mode-engine)
         (progn
           (message "web-mode-engine is %s" fb-web-mode-engine)
           (web-mode-set-engine fb-web-mode-engine))
       (progn
         (message "no web-mode-engine settled")
         (web-mode-set-engine "none"))))))
(add-hook 'web-mode-hook  'fb|web-mode-hook nil nil)

(use-package emmet-mode
  :hook ((html-mode sgml-mode css-mode web-mode) . emmet-preview-mode)
  :commands (emmet-mode emmet-expand-line yas/insert-snippet yas-insert-snippet company-complete)
  :config
  (setq emmet-move-cursor-between-quotes t
        emmet-preview-default nil
        ;; emmet-preview-default t
        )
  (add-hook 'rjsx-mode-hook
            (lambda ()
              (setq-local emmet-expand-jsx-className? t)))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; orgmode-org
;;;;
;;

(use-package org
  ;; :hook
  ;; :config
  ;; :custom
  )

(use-package evil-org
  :after org
  :init
  (setq evil-org-use-additional-insert t
  evil-org-key-theme '(
                            ;; additional
                            ;; calendar
                            ;; heading
                            ;; insert
                            ;; navigation
                            ;; return
                            ;; shift
                            textobjects
                            ;; todo
                            )
  )
  ;; :hook (org-mode . (lambda () evil-org-mode))
  :hook (org-mode . evil-org-mode)
  :config
  (evil-org-set-key-theme)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; orgmode-misc
;;;;
;;

(setq org-hide-emphasis-markers t)

(defvar fb/notesDir
(expand-file-name "~/NOTES/")
  "The file path to the org-notes-files")

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
     ;; (?- . ?•)
     (?\u002D . ?\u2022)

     ;; (?+ . ?➤)
     (?\u002B . ?\u27A4)
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

     ;; (?* . ?•)
     (?\u002A . ?\u2022)
     )
   )
  )

(setq org-global-properties '(
                              ("Effort_ALL". " 0:05 0:10 0:15 0:30 1:00 1:30 3:00 5:00 7:30 13:00 21:00")
                           ;;;; pomodoro buildingBlocks 1    2    4    6    10   16   26
                           ;; ("Effort_ALL". " 0:05 0:10 0:15 0:30 1:00 2:00 3:00 5:00 8:00 13:00")

                              ("COOKIE_DATA". "todo recursive")                         ;;;; use todo instead of list; count all subtree entries
                              ;; ("ORDERED". "t")
                              ))

(setq org-startup-indented t)

(setq org-image-actual-width (list 650))

(use-package org-make-toc
  :hook (org-mode . org-make-toc-mode))

(use-package calfw-org
  :config
  (setq cfw:org-face-agenda-item-foreground-color (fb*getDefaultColorValue :base8))
  )

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

(use-package org-wild-notifier
  :hook (org-mode . org-wild-notifier-mode)
  :config
  (setq
    alert-default-style 'libnotify
    org-wild-notifier-alert-times-property 'NOTIFY
  )
)

(defun fb*org-mode-h ()
  (fb*default-company-backends-h)
  (company-mode)
  )

(add-hook 'org-mode-hook 'fb*org-mode-h)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; orgmode-export
;;;;
;;

(defun fb/ox/tags-preventing-export-of-subtree-according-to-export-back-end (backend)
  "Remove all subtreees with the tags specified below.
     BACKEND is the export back-end being used, as a symbol."
  (cond
   ((org-export-derived-backend-p backend 'md) (setq  org-export-exclude-tags '("noexport" "mdignore")))
   ((org-export-derived-backend-p backend 're-reveal) (setq  org-export-exclude-tags '("noexport" "revealignore") ))))

(add-hook 'org-export-before-parsing-hook 'fb/ox/tags-preventing-export-of-subtree-according-to-export-back-end)

(setq org-export-global-macros '(
;;;; REVEAL
                                 ("toc-on-export" .
                                  "(eval (format \"%s\"
                                    (cond
                                      ((org-export-derived-backend-p org-export-current-backend 'md) \"#+OPTIONS: toc:1\")
                                      ((org-export-derived-backend-p org-export-current-backend 're-reveal) \"#+OPTIONS: toc:nil\")
                                      )))")
                                 ("tags-on-export" .
                                  "(eval (format \"%s\"
                                    (cond
                                      ((org-export-derived-backend-p org-export-current-backend 'md) \"#+OPTIONS: tags:1\")
                                      ((org-export-derived-backend-p org-export-current-backend 're-reveal) \"#+OPTIONS: tags:nil\")
                                      )))")
                                 ("show-url-on-title-slide" .
                                  "(eval (format \"%s\"
                                    (cond
                                      ((not (bound-and-true-p org-fb-re-reveal-talk-url))                                                                                                                                         (concat \"                                                        #+HTML_HEAD: <style>#talkURL__titleSlide{display:none}</style>                                                \\n #+HTML_HEAD: <style>#qrCode__titleSlide{display:none}</style> \")) ;;;; ______ __                                                                                                                                                                                                                                                       ;;;; no URL
                                      ((and (boundp 'org-fb-re-reveal-talk-url) (bound-and-true-p org-fb-re-reveal-talk-url-show-anchor-on-title-slide) (bound-and-true-p org-fb-re-reveal-talk-url-show-qrCode-on-title-slide) ) (concat \"#+REVEAL_TALK_URL: \" org-fb-re-reveal-talk-url  \"                                                                    \\n #+REVEAL_TALK_QR_CODE: ./qrCodeTalkURL.png                                                                   \")) ;;;; Anchor QR
                                      ((and (boundp 'org-fb-re-reveal-talk-url) (bound-and-true-p org-fb-re-reveal-talk-url-show-anchor-on-title-slide)                                                                         ) (concat \"#+REVEAL_TALK_URL: \" org-fb-re-reveal-talk-url  \"                                                                                                                   \\n #+HTML_HEAD: <style>#qrCode__titleSlide{display:none}</style> \")) ;;;; Anchor
                                      ((and (boundp 'org-fb-re-reveal-talk-url)                                                                         (bound-and-true-p org-fb-re-reveal-talk-url-show-qrCode-on-title-slide) ) (concat \"#+REVEAL_TALK_URL: \" org-fb-re-reveal-talk-url  \" \\n #+HTML_HEAD: <style>#talkURL__titleSlide{display:none}</style> \\n #+REVEAL_TALK_QR_CODE: ./qrCodeTalkURL.png                                                                   \")) ;;;;        QR
                                      (     (boundp 'org-fb-re-reveal-talk-url)                                                                                                                                                   (concat \"#+REVEAL_TALK_URL: \" org-fb-re-reveal-talk-url  \" \\n #+HTML_HEAD: <style>#talkURL__titleSlide{display:none}</style>                                                \\n #+HTML_HEAD: <style>#qrCode__titleSlide{display:none}</style> \")) ;;;; ______ __
                                      ))))")
                                 ("end-of-talk" .
                                  "(eval (format \"%s\"
                                    (cond
                                      ((not (bound-and-true-p org-fb-re-reveal-talk-url)) \"#+REVEAL: split \n#+REVEAL_HTML: <h2>END</h2> \n\")                                                                                                                                                                                                                                                                                                                                                   ;;;; END
                                      ((and (boundp 'org-fb-re-reveal-talk-url) (bound-and-true-p org-fb-re-reveal-talk-url-show-qrCode-and-anchor-on-last-slide)) (concat \"#+REVEAL: split \n#+REVEAL_HTML: <h2>END</h2> \n#+REVEAL: split \n#+REVEAL_HTML: <div id='talkURL__titleSlide__svgContainer'> \n#+ATTR_HTML: :id qrCode__titleSlide__svg :alt qrCode :style width:25vh; height:25vh; \n[[file:qrCodeTalkURL.svg]] \n#+REVEAL_HTML: </div> \n#+REVEAL_HTML: <a id='talkURL__titleSlide__End' href='\" org-fb-re-reveal-talk-url \"'>\" org-fb-re-reveal-talk-url \"</a>\")) ;;;; END + QR and Anchor
                                      (t \"#+REVEAL: split \n#+REVEAL_HTML: <h2>END</h2> \n\")                                                                                                                                                                                                                                                                                                                                                                                                    ;;;; END
                                      )))")
                                 ))

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

(defvar fb/org-screenshot-dir
(concat (file-name-as-directory fb/notesDir) "TMP")
;; (expand-file-name (concat (file-name-as-directory fb/notesDir) "TMP"))
  "Path to dir where all org-screenshots are stored"
  )

(defun fb/org-screenshot-plus-click ()
  "Take a screenshot into a time stamped unique-named file in the
screenshotDir and insert a link to this file."
  (interactive)
  (setq filename
        (concat
         (make-temp-name
          ;; (concat (buffer-file-name) ;;;; current filename
          (concat (file-name-as-directory fb/org-screenshot-dir)
                  (format-time-string "%Y%m%d_%H%M%S_")) ) ".png"))
  (call-process "import" nil nil nil filename)                      ;;;; takes screenshot and creates file
  (insert (concat "[[" filename "]]"))
  (org-display-inline-images))

(org-add-link-type "epub"  (lambda (path) (browse-url-xdg-open path)  ))
(org-add-link-type "pdf"   (lambda (path) (browse-url-xdg-open path)  ))
(org-add-link-type "video" (lambda (path) (browse-url-xdg-open path)  ))
(org-add-link-type "rifle" (lambda (path) (browse-url-rifle-open path)))

(defun browse-url-rifle-open (url &optional ignored)
  "Pass the specified URL to the \"rifle\" command.
rifle is a desktop utility that calls your preferred program.
Configuration in ~/.config/ranger/rifle.conf
rifle can use mimetype like xdg-open and additionally extensions (regexes)
The optional argument IGNORED is not used."
  (interactive (browse-url-interactive-arg "URL: "))
  (call-process "rifle" nil 0 nil url))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; orgmode-agenda
;;;;
;;

(setq org-agenda-files '(
                         "~/NOTES"
                         "~/NOTES/〇"
                         ;; "~/NOTES/PROJECTS"
                         ))

(setq org-deadline-warning-days 14)

(setq org-agenda-time-grid
      '((daily today require-timed)
        (0000 0200 0400 0600 0800 1000 1200 1400 1600 1800 2000 2200)
        "······" "────────────────"))

(setq org-agenda-start-with-log-mode nil)

(setq org-log-into-drawer t)

(setq org-log-done 'note)

(setq org-priority-faces
  `(
    (?A :foreground ,(fb*getDefaultColorValue :red)    :background ,(fb*getDefaultColorValue :bg))
    (?B :foreground ,(fb*getDefaultColorValue :orange) :background ,(fb*getDefaultColorValue :bg))
    (?C :foreground ,(fb*getDefaultColorValue :yellow) :background ,(fb*getDefaultColorValue :bg))
    ))

(setq org-todo-keywords
  '(
    (sequence "TODO(t!)"   "PENDING(p!)"   "NEXT(n!)"   "WIP(w!)"                  "|" "DONE(d@/!)" "CANCELLED(c@/!)" "DEPRECATED(e@/!)" "ARCHIVED(a)")
    (sequence "CRASH(C)"   "BUG(B)"        "REQUEST(R)" "TEST(E)"                  "|" "FIXED(F)"                                                     )
    (sequence "BACKLOG(O)" "KONZEPTION(K)" "BEREIT(T)"  "UMSETZUNG(U)" "ABNAHME(A)" "LIVE(L)" "|" "ERLEDIGT(D)"                                       )
    (sequence " (N)") ;;;; placeholder -> last line is not shown in buffer properly
    )
  )

(setq org-todo-keyword-faces
      (list
        `("TODO"       . (:foreground ,(fb*getDefaultColorValue :orange   ) :weight bold :background ,(fb*getDefaultColorValue :bg-alt)))
        `("PENDING"    . (:foreground ,(fb*getDefaultColorValue :yellow   ) :weight bold :background ,(fb*getDefaultColorValue :bg-alt)))
        `("NEXT"       . (:foreground ,(fb*getDefaultColorValue :red      ) :weight bold :background ,(fb*getDefaultColorValue :bg-alt)))
        `("WIP"        . (:foreground ,(fb*getDefaultColorValue :dark-blue) :weight bold :background ,(fb*getDefaultColorValue :bg-alt)))
        `("DONE"       . (:foreground ,(fb*getDefaultColorValue :green    ) :weight bold :background ,(fb*getDefaultColorValue :bg-alt)))
        `("CANCELLED"  . (:foreground ,(fb*getDefaultColorValue :base5    ) :weight bold :background ,(fb*getDefaultColorValue :bg-alt)))
        `("DEPRECATED" . (:foreground ,(fb*getDefaultColorValue :base5    ) :weight bold :background ,(fb*getDefaultColorValue :bg-alt)))
        `("ARCHIVED"   . (:foreground ,(fb*getDefaultColorValue :base5    ) :weight bold :background ,(fb*getDefaultColorValue :bg-alt)))

        `("CRASH"      . (:foreground ,(fb*getDefaultColorValue :red      ) :weight bold :background ,(fb*getDefaultColorValue :bg-alt)))
        `("BUG"        . (:foreground ,(fb*getDefaultColorValue :orange   ) :weight bold :background ,(fb*getDefaultColorValue :bg-alt)))
        `("REQUEST"    . (:foreground ,(fb*getDefaultColorValue :cyan     ) :weight bold :background ,(fb*getDefaultColorValue :bg-alt)))
        `("TEST"       . (:foreground ,(fb*getDefaultColorValue :blue     ) :weight bold :background ,(fb*getDefaultColorValue :bg-alt)))
        `("FIXED"      . (:foreground ,(fb*getDefaultColorValue :green    ) :weight bold :background ,(fb*getDefaultColorValue :bg-alt)))

        `("BACKLOG"    . (:foreground ,(fb*getDefaultColorValue :cyan     ) :weight bold :background ,(fb*getDefaultColorValue :bg-alt)))
        `("KONZEPTION" . (:foreground ,(fb*getDefaultColorValue :orange   ) :weight bold :background ,(fb*getDefaultColorValue :bg-alt)))
        `("BEREIT"     . (:foreground ,(fb*getDefaultColorValue :red      ) :weight bold :background ,(fb*getDefaultColorValue :bg-alt)))
        `("UMSETZUNG"  . (:foreground ,(fb*getDefaultColorValue :dark-blue) :weight bold :background ,(fb*getDefaultColorValue :bg-alt)))
        `("ABNAHME"    . (:foreground ,(fb*getDefaultColorValue :yellow   ) :weight bold :background ,(fb*getDefaultColorValue :bg-alt)))
        `("LIVE"       . (:foreground ,(fb*getDefaultColorValue :red      ) :weight bold :background ,(fb*getDefaultColorValue :bg-alt)))
        `("ERLEDIGT"   . (:foreground ,(fb*getDefaultColorValue :green    ) :weight bold :background ,(fb*getDefaultColorValue :bg-alt)))
        )
      )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; orgmode-babel
;;;;
;;

(setq org-src-tab-acts-natively t)

(use-package ob-arduino
  :config
  (add-to-list 'org-babel-load-languages  '(arduino . t))
  )

(use-package ob-browser
  :config
  (add-to-list 'org-babel-load-languages  '(browser . t))
  )

(use-package ob-csharp
  :config
  (add-to-list 'org-babel-load-languages  '(csharp . t))
  )

(use-package ob-C
  :config
  (add-to-list 'org-babel-load-languages  '(cpp . t))
  )

(use-package ob-css
  :config
  (add-to-list 'org-babel-load-languages  '(css . t))
  )

(use-package ob-dart
  :config
  (add-to-list 'org-babel-load-languages  '(dart . t))
  )

(use-package ein
  :config
  (add-to-list 'org-babel-load-languages  '(ein . t))
  )

(use-package ein-notebook)
(use-package ein-subpackages)

(use-package ob-go)
(add-to-list 'org-babel-load-languages  '(go . t))

(use-package ob-graphql
  :config
  (add-to-list 'org-babel-load-languages  '(graphql . t))
  )

(use-package ob-dot
  :config
  (add-to-list 'org-babel-load-languages  '(dot . t))
  )

(use-package ob-haskell
  :config
  (add-to-list 'org-babel-load-languages  '(haskell . t))
  )

(use-package ob-http
  :config
  (add-to-list 'org-babel-load-languages  '(http  . t))
  )

(use-package ob-java
  :config
  (add-to-list 'org-babel-load-languages  '(java . t))
  )

(use-package ob-js
  :config
  (add-to-list 'org-babel-load-languages  '(js . t))
  )

(use-package ob-kotlin
  :config
  (add-to-list 'org-babel-load-languages  '(kotlin . t))
  )

(use-package ob-latex
  :config
  (add-to-list 'org-babel-load-languages  '(latex . t))
  )

(use-package
  :config
  (add-to-list 'org-babel-load-languages  '(make . t))
  )

(use-package ob-mermaid
  :config
  (add-to-list 'org-babel-load-languages  '(mermaid . t))
  )

(use-package ob-mongo
  :config
  (add-to-list 'org-babel-load-languages  '(mongo  . t))
  )

(use-package ob-plantuml
  :config
  (setq org-plantuml-jar-path "/run/current-system/sw/lib/plantuml.jar")
  (add-to-list 'org-babel-load-languages  '(plantuml . t))
  )

(use-package ob-python
  :config
  (add-to-list 'org-babel-load-languages  '(python . t))
  )

(use-package ob-redis
  :config
  (add-to-list 'org-babel-load-languages  '(redis . t))
  )

(use-package ob-restclient
  :config
  (add-to-list 'org-babel-load-languages  '(restclient . t))
  :mode (("\\.http\\'" . restclient-mode))
  )

(use-package ob-rust
  :config
  (add-to-list 'org-babel-load-languages  '(rust . t))
  )

(use-package ob-sass
  :config
  (add-to-list 'org-babel-load-languages  '(sass . t))
  )

(use-package ob-swift
  :config
  (add-to-list 'org-babel-load-languages  '(swift . t))
  )

(use-package ob-shell
  :config
  (add-to-list 'org-babel-load-languages  '(shell . t))
  )

(use-package ob-sqlite
  :config
  (add-to-list 'org-babel-load-languages  '(sqlite . t))
  )

(use-package ob-translate
  :config
  (add-to-list 'org-babel-load-languages  '(translate  . t))
  )
(use-package google-translate
  :config
  (setq google-translate-backend-method 'curl)
  (defun google-translate--search-tkk () "Search TKK." (list 430675 2721866130))
  )

(use-package ob-typescript
  :config
  (add-to-list 'org-babel-load-languages  '(typescript  . t))
  )

(wrap-region-add-wrapper "#+BEGIN_SRC dart :results output\n"  "#+END_SRC\n"     "D" 'org-mode)
(wrap-region-add-wrapper "#+BEGIN_EXAMPLE\n"                   "#+END_EXAMPLE\n" "E" 'org-mode)
(wrap-region-add-wrapper "#+BEGIN_SRC go :results drawer\n"    "#+END_SRC\n"     "G" 'org-mode)
(wrap-region-add-wrapper "#+BEGIN_SRC shell :results drawer\n" "#+END_SRC\n"     "S" 'org-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; orgmode-capture
;;;;
;;

(use-package org-protocol)

(add-hook 'org-capture-mode-hook 'evil-insert-state)

(defvar fb*noteFile "~/NOTES/AKTUELLES.org" "Target file for org-capture")

(setq org-capture-templates '())

(setq webSelection-templates '(
  ("P" "Protocol Link" entry (file+olp fb*noteFile "〇" "1  UNSORTIERTES")
    "* %?\n[[%:link][%(fb\\*transform-square-brackets-to-round-ones \"%:description\")]]\n#+BEGIN_QUOTE\n%i\n#+END_QUOTE")
  ))
(setq org-capture-templates (append org-capture-templates webSelection-templates))

(setq webWithoutSelection-templates '(
  ("L" "Protocol Link" entry (file+olp fb*noteFile "〇" "1  UNSORTIERTES")
    "* %? [[%:link][%(fb\\*transform-square-brackets-to-round-ones \"%:description\")]]\n")
  ))
(setq org-capture-templates (append org-capture-templates webWithoutSelection-templates))

(setq aktuelles-templates '(
                            ("ak" "AKTUELLES"      entry (file+headline fb*noteFile "AKTUELLES") "* %i%?\n")
                            ("ad" "AKTUELLES TODO" entry (file+headline fb*noteFile "AKTUELLES") "* TODO %i%?\n")
                            ))
(setq org-capture-templates (append org-capture-templates aktuelles-templates))

(setq project-templates '(
    ("a" "AKTUELLES AI Anthroposophie Archlinux Art Astronomy")
    ("ai" "AI"               entry (file+olp fb*noteFile "PROJECTS" "AI"               ) "* %i%?\n")
    ("as" "Anthroposophie"   entry (file+olp fb*noteFile "PROJECTS" "Anthroposophie"   ) "* %i%?\n")
    ("al" "Archlinux"        entry (file+olp fb*noteFile "PROJECTS" "Archlinux"        ) "* %i%?\n")
    ("ar" "Art"              entry (file+olp fb*noteFile "PROJECTS" "Art"              ) "* %i%?\n")
    ("at" "Astronomy"        entry (file+olp fb*noteFile "PROJECTS" "Astronomy"        ) "* %i%?\n")

    ("b" "Berufliches BigData Browser")
    ("br" "Berufliches"      entry (file+olp fb*noteFile "PROJECTS" "Berufliches"      ) "* %i%?\n")
    ("bd" "BigData"          entry (file+olp fb*noteFile "PROJECTS" "BigData"          ) "* %i%?\n")
    ("bs" "Browser"          entry (file+olp fb*noteFile "PROJECTS" "Browser"          ) "* %i%?\n")

    ("c" "Computer Consoles Container+Cloud CSS")
    ("cp" "Computer"         entry (file+olp fb*noteFile "PROJECTS" "Computer"         ) "* %i%?\n")
    ("cl" "Consoles"         entry (file+olp fb*noteFile "PROJECTS" "Consoles"         ) "* %i%?\n")
    ("cc" "Container+Cloud"  entry (file+olp fb*noteFile "PROJECTS" "Container+Cloud"  ) "* %i%?\n")
    ("cs" "CSS"              entry (file+olp fb*noteFile "PROJECTS" "CSS"              ) "* %i%?\n")

    ("d" "Dart DB Debugging")
    ("da" "Dart"             entry (file+olp fb*noteFile "PROJECTS" "Dart"             ) "* %i%?\n")
    ("db" "DataBases"        entry (file+olp fb*noteFile "PROJECTS" "DataBases"        ) "* %i%?\n")
    ("dg" "Debugging"        entry (file+olp fb*noteFile "PROJECTS" "Debugging" "EVENTS") "%i**** %^{EVENT}
\***** SITUATION/SETUP
  %^{SITUATION}
\***** MESSAGE
  %x
\***** PROBLEM / CAUSE
  %^{PROBLEM}
\***** SOLUTION
  %?
  ")
    ("e" "Editors emacs Embedded Energy Ernährung")
    ("em" "emacs"            entry (file+olp fb*noteFile "PROJECTS" "emacs"            ) "* %i%?\n")
    ("ed" "Editors"          entry (file+olp fb*noteFile "PROJECTS" "Editors"          ) "* %i%?\n")
    ("eb" "embedded"         entry (file+olp fb*noteFile "PROJECTS" "embedded"         ) "* %i%?\n")
    ("eg" "Energy"           entry (file+olp fb*noteFile "PROJECTS" "Energy"           ) "* %i%?\n")
    ("en" "Ernährung"        entry (file+olp fb*noteFile "PROJECTS" "Ernährung"        ) "* %i%?\n")

    ("g" "Geographie Git Golang")
    ("gg" "Geographie"       entry (file+olp fb*noteFile "PROJECTS" "Geographie"       ) "* %i%?\n")
    ("gt" "Git"              entry (file+olp fb*noteFile "PROJECTS" "Git"              ) "* %i%?\n")
    ("go" "Golang"           entry (file+olp fb*noteFile "PROJECTS" "Golang"           ) "* %i%?\n")

    ("h" "Hardware Haushalt")
    ("hw" "Hardware"         entry (file+olp fb*noteFile "PROJECTS" "Hardware"         ) "* %i%?\n")
    ("hh" "Haushalt"         entry (file+olp fb*noteFile "PROJECTS" "Haushalt"         ) "* %i%?\n")

    ("i" "Infrastructure Installationen IoT")
    ("is" "Infrastructure"   entry (file+olp fb*noteFile "PROJECTS" "Infrastructure"   ) "* %i%?\n")
    ("il" "Installation"     entry (file+olp fb*noteFile "PROJECTS" "Installation"     ) "* %i%?\n")
    ("it" "IoT"              entry (file+olp fb*noteFile "PROJECTS" "IoT"              ) "* %i%?\n")

    ("k" "Keyboard Klassifikation Körper")
    ("kb" "Keyboard"         entry (file+olp fb*noteFile "PROJECTS" "Keyboard"         ) "* %i%?\n")
    ("kk" "Klassifikation"   entry (file+olp fb*noteFile "PROJECTS" "Klassifikation"   ) "* %i%?\n")
    ("kp" "Körper"           entry (file+olp fb*noteFile "PROJECTS" "Körper"           ) "* %i%?\n")

    ("l" "Lisp")
    ("li" "Lisp"             entry (file+olp fb*noteFile "PROJECTS" "Lisp"             ) "* %i%?\n")

    ("m" "Maker Mathematik MeinLeben Mobile Music")
    ("ma" "Maker"            entry (file+olp fb*noteFile "PROJECTS" "Maker"            ) "* %i%?\n")
    ("mm" "Mathematik"       entry (file+olp fb*noteFile "PROJECTS" "Mathematik"       ) "* %i%?\n")
    ("ml" "MeinLeben"        entry (file+olp fb*noteFile "PROJECTS" "MeinLeben"        ) "* %i%?\n")
    ("mb" "Mobile"           entry (file+olp fb*noteFile "PROJECTS" "Mobile"           ) "* %i%?\n")
    ("mu" "Music"            entry (file+olp fb*noteFile "PROJECTS" "Music"            ) "* %i%?\n")

    ("n" "Network NixOS")
    ("nw" "Network"          entry (file+olp fb*noteFile "PROJECTS" "Network"          ) "* %i%?\n")
    ("nx" "NixOS"            entry (file+olp fb*noteFile "PROJECTS" "NixOS"            ) "* %i%?\n")

    ("o" "OperatingSystems OrgMode")
    ("os" "OperatingSystems" entry (file+olp fb*noteFile "PROJECTS" "OperatingSystems" ) "* %i%?\n")
    ("om" "OrgMode"          entry (file+olp fb*noteFile "PROJECTS" "OrgMode"          ) "* %i%?\n")

    ("p" "Personal Pflanzen Planning Programming Projects Psychologie Python")
    ("ps" "Personal"         entry (file+olp fb*noteFile "PROJECTS" "Personal"         ) "* %i%?\n")
    ("pf" "Pflanzen"         entry (file+olp fb*noteFile "PROJECTS" "Pflanzen"         ) "* %i%?\n")
    ("pl" "Planning"         entry (file+olp fb*noteFile "PROJECTS" "Planning"         ) "* %i%?\n")
    ("pg" "Programming"      entry (file+olp fb*noteFile "PROJECTS" "Programming"      ) "* %i%?\n")
    ("pj" "Projects"         entry (file+olp fb*noteFile "PROJECTS" "Projects"         ) "* %i%?\n")
    ("pi" "Psychologie"      entry (file+olp fb*noteFile "PROJECTS" "Psychologie"      ) "* %i%?\n")
    ("py" "Python"           entry (file+olp fb*noteFile "PROJECTS" "Python"           ) "* %i%?\n")

    ("r" "ReadTheDocs Religion Rust")
    ("rd" "ReadTheDocs"      entry (file+olp fb*noteFile "PROJECTS" "ReadTheDocs"      ) "* %i%?\n")
    ("rl" "Religion"         entry (file+olp fb*noteFile "PROJECTS" "Religion"         ) "* %i%?\n")
    ("ru" "Rust"             entry (file+olp fb*noteFile "PROJECTS" "Rust"             ) "* %i%?\n")

    ("s" "Schrift Search Security Sprachen")
    ("st" "Schrift"          entry (file+olp fb*noteFile "PROJECTS" "Schrift"          ) "* %i%?\n")
    ("se" "Search"           entry (file+olp fb*noteFile "PROJECTS" "Search"           ) "* %i%?\n")
    ("sc" "Security"         entry (file+olp fb*noteFile "PROJECTS" "Security"         ) "* %i%?\n")
    ("sp" "Sprachen"         entry (file+olp fb*noteFile "PROJECTS" "Sprachen"         ) "* %i%?\n")

    ("t" "Technology Testing Tools")
    ("tc" "Technology"       entry (file+olp fb*noteFile "PROJECTS" "Technology"       ) "* %i%?\n")
    ("te" "Testing"          entry (file+olp fb*noteFile "PROJECTS" "Testing"          ) "* %i%?\n")
    ("to" "Tools"            entry (file+olp fb*noteFile "PROJECTS" "Tools"            ) "* %i%?\n")

    ("v" "Virtualisierung VirtualReality")
    ("vi" "Virtualisierung"  entry (file+olp fb*noteFile "PROJECTS" "Virtualisierung"  ) "* %i%?\n")
    ("vr" "VirtualReality"   entry (file+olp fb*noteFile "PROJECTS" "VirtualReality"   ) "* %i%?\n")

    ("w" "Web Welt")
    ("we" "Web"              entry (file+olp fb*noteFile "PROJECTS" "Web"              ) "* %i%?\n")
    ("wl" "Welt"             entry (file+olp fb*noteFile "PROJECTS" "Welt"             ) "* %i%?\n")

    ("y" "Yoga")
    ("yg" "Yoga"             entry (file+olp fb*noteFile "PROJECTS" "Yoga"             ) "* %i%?\n")

    ("z" "Zukunft")
    ("zk" "Zukunft"          entry (file+olp fb*noteFile "PROJECTS" "Zukunft"          ) "* %i%?\n")
    ))
    (setq org-capture-templates (append org-capture-templates project-templates))

(setq clock-template '(("ck" "clock" entry (clock) "* %i%?\n")))
(setq org-capture-templates (append org-capture-templates clock-template))

(setq 〇-templates '(
  ("1" "UNSORTIERTES"   entry (file+olp fb*noteFile "〇" "1  UNSORTIERTES"                  ) "* %i%?\n")
  ("2" "IDEEN"          entry (file+olp fb*noteFile "〇" "2  IDEEN"                         ) "* %i%?\n")
  ("3" "FRAGEN"         entry (file+olp fb*noteFile "〇" "3  FRAGEN"                        ) "* %i%?\n")
  ("4" "RECHERCHE"      entry (file+olp fb*noteFile "〇" "4  RECHERCHE"                     ) "* %i%?\n")
  ("5" "BIBLIO~"        entry (file+olp fb*noteFile "〇" "5  BIBLIO~"                       ) "* %i%?\n")
  ("6" "I"              entry (file+olp fb*noteFile "〇" "6  I"                             ) "* %i%?\n")
  ("7" "ToDO"           entry (file+olp fb*noteFile "〇" "7  ToDO"                          ) "* TODO %i%?\n")
  ("A" "ANSCHAFFUNGEN"  entry (file+olp fb*noteFile "〇" "7a ANSCHAFFUNGEN"                 ) "* %i%?\n")
  ("8" "INSTALLATIONEN" entry (file+olp fb*noteFile "〇" "8  INSTALLATIONEN"                ) "* %i%?\n")
  ("9" "ROUTINEN"       entry (file+olp fb*noteFile "〇" "9  ROUTINEN + EVENTS + TRIGGERED" ) "* %i%?\n")
  ("0" "ERKENNTNISSE"   entry (file+olp fb*noteFile "〇" "10 ERKENNTNISSE"                  ) "* %i%?\n")
  ("E" "ERLEDIGTES"     entry (file+olp fb*noteFile "〇" "11 ERLEDIGTES"                    ) "* %i%?\n")
  ))
  (setq org-capture-templates (append org-capture-templates 〇-templates))

;;;; TODO: ask for headline location
  ;; ("p" "projects" entry
  ;; (file+function fb*noteFile org-ask-location)
  ;; "\n\n** %?\n<%<%Y-%m-%d %a %T>>"
  ;; :empty-lines 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; orgmode-drill
;;;;
;;

(use-package org-drill
  :config
  (setq org-drill-scope 'file)
  ;; (setq org-drill-scope 'tree)
  ;; (setq org-drill-scope 'agenda)
  ;; (setq org-drill-scope 'directory)
  )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; orgmode-refile
;;;;
;;

(defmacro fb/make-org-refile-hydra (hydraname keyAndHeadline)
  "Make a hydra named HYDRANAME with refile targets to FILE."
  `(defhydra ,hydraname (:color blue :columns 6) "org-Refile-Hydra"
     ,@(cl-loop for kv in keyAndHeadline
                collect (list (car kv) (list 'fb/refile (cdr kv)) (f-base(cdr kv))))
     ("q" nil "cancel"))
  )

(defun fb/refile (file &optional headline arg)
  (unless headline (setq headline "1  UNSORTIERTES"))
  ;; (message headline)
  (let ((pos (save-excursion
               (find-file file)
               (org-find-exact-headline-in-buffer headline))))
    (org-refile arg nil (list headline file nil pos)))
  (switch-to-buffer (current-buffer)))

(defun fb*getDayForErledigtes ()
  (pcase (format-time-string "%w" (current-time) )
    ("0" "SO")
    ("1" "MO")
    ("2" "DI")
    ("3" "MI")
    ("4" "DO")
    ("5" "FR")
    ("6" "SA")
    )
  )

(defhydra fb/org-refile-hydra-grouped (:foreign-keys run :columns 2 :column "Horizontal")
  "Org-Refile"
  ("a" fb/org-refile-hydra-a/body "AI Anthroposophie Archlinux Art Astronomy"                          :exit t)
  ("b" fb/org-refile-hydra-b/body "Berufliches BigData Browser"                                        :exit t)
  ("c" fb/org-refile-hydra-c/body "Computer Consoles Container+Cloud CSS"                              :exit t)
  ("d" fb/org-refile-hydra-d/body "Dart DB Debugging"                                                  :exit t)
  ("e" fb/org-refile-hydra-e/body "Editors emacs Embedded Energy Ernährung"                            :exit t)
  ;; ("f" fb/org-refile-hydra-f/body "" :exit t)
  ("g" fb/org-refile-hydra-g/body "Geographie Git Golang"                                              :exit t)
  ("h" fb/org-refile-hydra-h/body "Hardware Haushalt"                                                  :exit t)
  ("i" fb/org-refile-hydra-i/body "Infrastructure Installationen IoT"                                  :exit t)
  ;; ("j" fb/org-refile-hydra-j/body "" :exit t)
  ("k" fb/org-refile-hydra-k/body "Keyboard Klassifikation Körper"                                     :exit t)
  ("l" fb/org-refile-hydra-l/body "Lisp"                                                               :exit t)
  ("m" fb/org-refile-hydra-m/body "Maker Mathematik MeinLeben Mobile Music"                            :exit t)
  ("n" fb/org-refile-hydra-n/body "Network NixOS"                                                      :exit t)
  ("o" fb/org-refile-hydra-o/body "OperatingSystems OrgMode"                                           :exit t)
  ("p" fb/org-refile-hydra-p/body "Personal Pflanzen Planning Programming Projects Psychologie Python" :exit t)
  ;; ("q" fb/org-refile-hydra-q/body "" :exit t)
  ("r" fb/org-refile-hydra-r/body "ReadTheDocs Religion Rust"                                          :exit t)
  ("s" fb/org-refile-hydra-s/body "Schrift Search Security Sprachen"                                   :exit t)
  ("t" fb/org-refile-hydra-t/body "Technology Testing Tools"                                           :exit t)
  ;; ("u" fb/org-refile-hydra-u/body "" :exit t)
  ("v" fb/org-refile-hydra-v/body "Virtualisierung VirtualReality"                                     :exit t)
  ("w" fb/org-refile-hydra-w/body "Web Welt"                                                           :exit t)
  ;; ("x" fb/org-refile-hydra-x/body "" :exit t)
  ("y" fb/org-refile-hydra-y/body "Yoga"                                                               :exit t)
  ("z" fb/org-refile-hydra-z/body "Zukunft"                                                            :exit t)

  ("E" (fb/refile "AKTUELLES.org" (fb*getDayForErledigtes) ) "Erledigtes->aktueller Wochentag"         :exit t)

  ("0" fb/org-refile-hydra-0/body "〇"                                                                 :exit t)

  ("j" org-refile-goto-last-stored "Jump to last refile"                                               :exit t)
  ("q" nil "cancel")
  )

(fb/make-org-refile-hydra fb/org-refile-hydra-a
                          (
                           ("i" . "PROJECTS/AI.org"            )
                           ("s" . "PROJECTS/Anthroposophie.org")
                           ("l" . "PROJECTS/Archlinux.org"     )
                           ("r" . "PROJECTS/Art.org"           )
                           ("t" . "PROJECTS/Astronomy.org"     )
                           ))
(fb/make-org-refile-hydra fb/org-refile-hydra-b
                          (
                           ("r" . "PROJECTS/Berufliches.org")
                           ("d" . "PROJECTS/BigData.org"    )
                           ("s" . "PROJECTS/Browser.org"    )
                           ))
(fb/make-org-refile-hydra fb/org-refile-hydra-c
                          (
                           ("p" . "PROJECTS/Computer.org"      )
                           ("l" . "PROJECTS/Consoles.org"      )
                           ("c" . "PROJECTS/ContainerCloud.org")
                           ("s" . "PROJECTS/CSS.org"           )
                           ))
(fb/make-org-refile-hydra fb/org-refile-hydra-d
                          (
                           ("a" . "PROJECTS/Dart.org"     )
                           ("b" . "PROJECTS/DataBases.org")
                           ("g" . "PROJECTS/Debugging.org")
                           ))
(fb/make-org-refile-hydra fb/org-refile-hydra-e
                          (
                           ("m" . "PROJECTS/emacs.org"    )
                           ("d" . "PROJECTS/Editors.org"  )
                           ("b" . "PROJECTS/Embedded.org" )
                           ("g" . "PROJECTS/Energy.org"   )
                           ("n" . "PROJECTS/Ernährung.org")
                           ))
(fb/make-org-refile-hydra fb/org-refile-hydra-g
                          (
                           ("g" . "PROJECTS/Geographie.org")
                           ("t" . "PROJECTS/Git.org"       )
                           ("o" . "PROJECTS/Golang.org"    )
                           ))
(fb/make-org-refile-hydra fb/org-refile-hydra-h
                          (
                           ("w" . "PROJECTS/Hardware.org")
                           ("h" . "PROJECTS/Haushalt.org")
                           ))
(fb/make-org-refile-hydra fb/org-refile-hydra-i
                          (
                           ("s" . "PROJECTS/Infrastructure.org")
                           ("l" . "PROJECTS/Installation.org"  )
                           ("t" . "PROJECTS/IoT.org"           )
                           ))
(fb/make-org-refile-hydra fb/org-refile-hydra-k
                          (
                           ("b" . "PROJECTS/Keyboard.org"      )
                           ("k" . "PROJECTS/Klassifikation.org")
                           ("p" . "PROJECTS/Körper.org"        )
                           ))
(fb/make-org-refile-hydra fb/org-refile-hydra-l
                          (
                           ("i" . "PROJECTS/Lisp.org")
                           ))
(fb/make-org-refile-hydra fb/org-refile-hydra-m
                          (
                           ("a" . "PROJECTS/Maker.org"     )
                           ("m" . "PROJECTS/Mathematik.org")
                           ("l" . "PROJECTS/MeinLeben.org" )
                           ("b" . "PROJECTS/Mobile.org"    )
                           ("u" . "PROJECTS/Music.org"     )
                           )
                          )
(fb/make-org-refile-hydra fb/org-refile-hydra-n
                          (
                           ("w" . "PROJECTS/Network.org")
                           ("x" . "PROJECTS/NixOS.org"  )
                           ))
(fb/make-org-refile-hydra fb/org-refile-hydra-o
                          (
                           ("s" . "PROJECTS/OperatingSystems.org")
                           ("m" . "PROJECTS/OrgMode.org"         )
                           ))
(fb/make-org-refile-hydra fb/org-refile-hydra-p
                          (
                           ("s" . "PROJECTS/Personal.org"   )
                           ("f" . "PROJECTS/Pflanzen.org"   )
                           ("l" . "PROJECTS/Planning.org"   )
                           ("g" . "PROJECTS/Programming.org")
                           ("j" . "PROJECTS/Projects.org"   )
                           ("i" . "PROJECTS/Psychologie.org")
                           ("y" . "PROJECTS/Python.org"     )
                           ))
(fb/make-org-refile-hydra fb/org-refile-hydra-r
                          (
                           ("d" . "PROJECTS/ReadTheDocs.org")
                           ("l" . "PROJECTS/Religion.org"   )
                           ("u" . "PROJECTS/Rust.org"       )
                           ))
(fb/make-org-refile-hydra fb/org-refile-hydra-s
                          (
                           ("t" . "PROJECTS/Schrift.org" )
                           ("e" . "PROJECTS/Search.org"  )
                           ("c" . "PROJECTS/Security.org")
                           ("p" . "PROJECTS/Sprachen.org")
                           ))
(fb/make-org-refile-hydra fb/org-refile-hydra-t
                          (
                           ("c" . "PROJECTS/Technology.org")
                           ("e" . "PROJECTS/Testing.org"   )
                           ("o" . "PROJECTS/Tools.org"     )
                           ))
(fb/make-org-refile-hydra fb/org-refile-hydra-v
                          (
                           ("i" . "PROJECTS/Virtualisierung.org")
                           ("r" . "PROJECTS/VirtualReality.org" )
                           ))
(fb/make-org-refile-hydra fb/org-refile-hydra-w
                          (
                           ("e" . "PROJECTS/Web.org" )
                           ("l" . "PROJECTS/Welt.org")
                           ))
(fb/make-org-refile-hydra fb/org-refile-hydra-y
                          (
                           ("g" . "PROJECTS/Yoga.org")
                           ))
(fb/make-org-refile-hydra fb/org-refile-hydra-z
                          (
                           ("k" . "PROJECTS/Zukunft.org")
                           ))
(fb/make-org-refile-hydra fb/org-refile-hydra-0
                          (
                           ("1" . "〇/1  UNSORTIERTES.org"  )
                           ("2" . "〇/2  IDEEN.org"         )
                           ("3" . "〇/3  FRAGEN.org"        )
                           ("4" . "〇/4  RECHERCHE.org"     )
                           ("5" . "〇/5  BIBLIO~.org"       )
                           ("6" . "〇/6  I.org"             )
                           ("7" . "〇/7  ToDO.org"          )
                           ("a" . "〇/7a ANSCHAFFUNGEN.org" )
                           ("8" . "〇/8  INSTALLATIONEN.org")
                           ("9" . "〇/9  ROUTINEN.org"      )
                           ("0" . "〇/10 ERKENNTNISSE.org"  )
                           ("e" . "〇/11 ERLEDIGTES.org"    )
                           ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; orgmode-reveal
;;;;
;;

(use-package org-re-reveal)

(setq org-re-reveal-revealjs-version "4")

(setq org-re-reveal-root (concat "file://" "/run/current-system/sw/share/" "revealjs"))
;; (setq org-re-reveal-root (concat "file://" "/home/frank/SRC/GITHUB/MISC/reveal.js"))

(setq org-reveal-hlevel 2)
(setq org-reveal-previewLinks t)

;; (setq org-reveal-theme "white")
(setq org-reveal-theme "black")
;; (setq org-reveal-theme "customTheme")

(setq org-re-reveal-script-files '("js/reveal.js" "js/revealjs.keybindings.js"))

(defun fb/org-re-reveal/insert (insertion)
  "enable fragments on current slide"
  (interactive)
  (evil-collection-unimpaired-insert-newline-below 1)
  (evil-next-line)
  (insert insertion)
  )

(defun fb/org-re-reveal/fragments ()
  "enable fragments on current slide"
  (interactive)
  (fb/org-re-reveal/insert "#+ATTR_REVEAL: :frag (appear)")
  )

(defun fb/org-re-reveal/insert-slide-split ()
  "split the current slide"
  (interactive)
  (fb/org-re-reveal/insert "#+REVEAL: split")
  )

(defun fb/org-re-reveal/fragments ()
  "enable fragments on current slide"
  (interactive)
  (evil-collection-unimpaired-insert-newline-below 1)
  (evil-next-line)
  (insert "#+ATTR_REVEAL: :frag (appear)")
  )

(defun fb/org-re-reveal/insert-slide-split ()
  "split the current slide"
  (interactive)
  (evil-collection-unimpaired-insert-newline-below 1)
  (evil-next-line)
  (insert "#+REVEAL: split")
  )

(defun fb/org-re-reveal/insert-line-break ()
  "insert a line break"
  (interactive)
  (end-of-line)
  (insert " \\\\")
  )

(defun fb/org-re-reveal/add-html ()
  "add raw html to slide"
  (interactive)
  (evil-collection-unimpaired-insert-newline-below 1)
  (evil-next-line)
  (insert "#+REVEAL_HTML: ")
  (evil-org-append-line 1)
  )

(defun fb/org-re-reveal/extra-attribut (attribute)
  "Add an attribute like a class.
   The attribute has to be set at binding definition."
  (interactive)
  (evil-collection-unimpaired-insert-newline-below 1)
  (evil-next-line)
  (insert (concat "#+REVEAL_EXTRA_ATTR: " attribute))
  (evil-backward-char)
  (evil-forward-char)
  (evil-insert 1)
  )

(defun fb/org-re-reveal/insert-end-of-talk ()
  "remove previous set of end-of-talk and insert below cursor"
  (interactive)
  (setq-local current-cursor-position (point))
  (let ((case-fold-search t)) ;; or nil
    (goto-char (point-min))
    (while (search-forward "{{{end-of-talk}}}\n" nil t)
      (replace-match ""))
    )
  (goto-char current-cursor-position)
  (evil-end-of-line)
  (evil-collection-unimpaired-insert-newline-below 1)
  (evil-next-line)
  (insert "{{{end-of-talk}}}")
  )

(defun fb|org-re-reveal|create-qr-code (backend)
  "create qrCode on org-export, if org-fb-re-reveal-talk-url is set"
  (cond
   ((boundp 'org-fb-re-reveal-talk-url) (shell-command (concat "qr --factory=svg-path " org-fb-re-reveal-talk-url " > qrCodeTalkURL.svg")))))
(add-hook 'org-export-before-processing-hook 'fb|org-re-reveal|create-qr-code)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; orgmode-padmā
;;;;
;;

(defun fb/store-as-successor ()
  "store current element-id as successor"
  (interactive)
  (evil-set-register ?s (org-id-get-create))
  (message "choose predecessor and call fb/insert-successor-relation")
  )

(defun fb/insert-successor-relation ()
  "insert a successor for current element"
  (interactive)
  (org-set-property "SUCCESSOR" (evil-get-register ?s))
  (evil-set-register ?p (org-id-get-create))
  (org-id-goto (evil-get-register ?s))
  (org-set-property "PREDECESSOR" (evil-get-register ?p))
  )

(defun fb/remove-predecessor-relation ()
  "remove the predecessor relation from current element"
  (interactive)
  (evil-set-register ?s (org-id-get-create))

  (org-id-goto  (org-entry-get (point) "PREDECESSOR"))
  (org-entry-delete (point) "SUCCESSOR")

  (org-id-goto  (evil-get-register ?s))
  (org-entry-delete (point) "PREDECESSOR")
  (evil-set-register ?s nil)
  )

(defun fb/goto-predecessor ()
  "go to the element specified by an org-id specified as value of the org-property: PREDECESSOR"
  (interactive)
  (org-id-goto  (org-entry-get (point) "PREDECESSOR"))
  )

(defun fb/store-as-predecessor ()
  "store current element-id as predecessor"
  (interactive)
  (evil-set-register ?p (org-id-get-create))
  (message "choose successor and call fb/insert-predecessor-relation")
  )

(defun fb/insert-predecessor-relation ()
  "insert a predecessor for current element"
  (interactive)
  (org-set-property "PREDECESSOR" (evil-get-register ?p))
  (evil-set-register ?s (org-id-get-create))
  (org-id-goto (evil-get-register ?p))
  (org-set-property "SUCCESSOR" (evil-get-register ?s))
  )

(defun fb/remove-successor-relation ()
  "remove the successor relation from current element"
  (interactive)
  (evil-set-register ?p (org-id-get-create))

  (org-id-goto  (org-entry-get (point) "SUCCESSOR"))
  (org-entry-delete (point) "PREDECESSOR")

  (org-id-goto  (evil-get-register ?p))
  (org-entry-delete (point) "SUCCESSOR")
  (evil-set-register ?p nil)
  )

(defun fb/goto-successor ()
  "go to the element specified by an org-id specified as value of the org-property: SUCCESSOR"
  (interactive)
  (org-id-goto  (org-entry-get (point) "SUCCESSOR"))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; orgmode-pomodoro
;;;;
;;

(use-package org-pomodoro
   :config
   (setq
    org-pomodoro-manual-break t
    org-pomodoro-ticking-sound-p nil

    org-pomodoro-length 25
    ;; org-pomodoro-length 1
    org-pomodoro-start-sound-p t
    org-pomodoro-start-sound (expand-file-name "ASSETS/sounds/ringHigh.wav" user-emacs-directory)
    org-pomodoro-overtime-sound-p t
    org-pomodoro-overtime-sound (expand-file-name "ASSETS/sounds/chimeTriple.wav" user-emacs-directory)
    org-pomodoro-finished-sound-p t
    org-pomodoro-finished-sound (expand-file-name "ASSETS/sounds/ringMiddle.wav" user-emacs-directory)                 ;;;; same as break-sound

    org-pomodoro-finished-before-long-break-sound-p t
    org-pomodoro-finished-before-long-break-sound (expand-file-name "ASSETS/sounds/chimeLow.wav" user-emacs-directory) ;;;; same as long-break-sound
    org-pomodoro-clock-break t                                                                                         ;;;; clock breaks, too
    org-pomodoro-short-break-length 5
    ;; org-pomodoro-short-break-length 1
    org-pomodoro-short-break-sound-p t
    org-pomodoro-short-break-sound (expand-file-name "ASSETS/sounds/ringMiddle.wav" user-emacs-directory)

    org-pomodoro-long-break-frequency 4
    org-pomodoro-long-break-length 35
;; org-pomodoro-long-break-length 1
    org-pomodoro-long-break-sound-p t
    org-pomodoro-long-break-sound (expand-file-name "ASSETS/sounds/chimeLow.wav" user-emacs-directory)

  ;;;; modeline
    org-pomodoro-time-format "%.2m:%.2s"
    org-pomodoro-format "P %s"
    ;; org-pomodoro-format " %s"
    org-pomodoro-overtime-format "+ %s"
    org-pomodoro-short-break-format "S %s"
    org-pomodoro-long-break-format "L %s"
    )
   )

);;with-eval-end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; project
;;;;
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; project-direnv
;;;;
;;

(use-package envrc
  :config
  (envrc-global-mode)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; project-magit
;;;;
;;

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  )

;; (add-hook 'magit-mode-hook (lambda () (magit-delta-mode +1)))



(use-package magit-todos
  :after magit
  :config (setq magit-todo-mode t)
  )



;; (use-package forge)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; project-misc
;;;;
;;

(use-package hl-todo
  :config
  (global-hl-todo-mode t)
  (setq hl-todo-keyword-faces
    '(("TODO"   . "#FF0000")
      ("FIXME"  . "#FF0000")
      ("DEBUG"  . "#A020F0")
      ("GOTCHA" . "#FF4500")
      ("STUB"   . "#1E90FF")))
  )

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; view-layout
;;;;
;;

(use-package ace-window
  :config
  (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l ?\;))
  (set-face-attribute 'aw-leading-char-face nil  :weight 'bold  :height 2.0     :foreground "deep sky blue")
  (set-face-attribute 'aw-mode-line-face    nil  :inherit 'mode-line-buffer-id  :foreground "lawn green")
  (ace-window-display-mode t)
  ;; (setq aw-dispatch-always t)
  (setq winner-mode 1)
  (setq aw-dispatch-alist
    '((?x aw-delete-window "delete")
      (?c aw-swap-window "swap")
      (?n aw-flip-window "flip")
      (?v aw-split-window-vert "split -")
      (?h aw-split-window-horz "split |")
      (?m delete-other-windows "maximize")
      (?g delete-other-windows)
      (?b balance-windows "=")
      ))
)

(defun fb/aw-split-window-horz ()
  "interactive aw-split-window-horz"
(interactive)
(aw-select "split |" 'aw-split-window-horz))
  ;; "av" '((lambda () (interactive) (ace-window ?h))      :which-key "split vert"                       )

(defun fb/aw-split-window-vert ()
  "interactive aw-split-window-vert"
  (interactive)
  (aw-select "split -" 'aw-split-window-vert))

(defun fb/winner-redo ()
  "interactive winner-redo"
  (interactive)
  (winner-redo))

(defun fb/winner-undo ()
  "interactive winner-undo"
  (interactive)
  (progn
    (winner-undo)
    (setq this-command 'winner-undo)))

(use-package persp-mode)

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
                cfw:calendar-mode-hook
                calendar-mode-hook
                eshell-mode-hook
                helpful-mode-hook
                org-mode-hook
                shell-mode-hook
                term-mode-hook
                treemacs-mode-hook
                ))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(spacemacs|add-toggle absolute-line-numbers
      :status (and (featurep 'display-line-numbers)
                   display-line-numbers-mode
                   (eq display-line-numbers t))
      :on (prog1 (display-line-numbers-mode)
            (setq display-line-numbers t))
      :off (display-line-numbers-mode -1)
      :on-message "Absolute line numbers enabled."
      :off-message "Line numbers disabled."
      :documentation "Show the line numbers."
      )

;; (spacemacs|add-toggle line-numbers
  ;; :status (and (featurep 'display-line-numbers)
               ;; display-line-numbers-mode
               ;; (eq display-line-numbers t))
  ;; :on (prog1 (display-line-numbers-mode)
        ;; (setq display-line-numbers t))
  ;; :off (display-line-numbers-mode -1)
  ;; :(or )n-message "Absolute line numbers enabled."
  ;; :off-message "Line numbers disabled."
  ;; :documentation "Show the line numbers.")

(spacemacs|add-toggle visual-line-numbers
  :status (and (featurep 'display-line-numbers)
               display-line-numbers-mode
               (eq display-line-numbers 'visual))
  :on (prog1 (display-line-numbers-mode)
        (setq display-line-numbers 'visual))
  :off (display-line-numbers-mode -1)
  :documentation "Show relative visual line numbers."
  :on-message "Visual line numbers enabled."
  :off-message "Line numbers disabled."
  ;; :evil-leader "tnv"
  )

(spacemacs|add-toggle relative-line-numbers
  :status (and (featurep 'display-line-numbers)
               display-line-numbers-mode
               (eq display-line-numbers 'relative))
  :on (prog1 (display-line-numbers-mode)
        (setq display-line-numbers 'relative))
  :off (display-line-numbers-mode -1)
  :documentation "Show relative line numbers."
  :on-message "Relative line numbers enabled."
  :off-message "Line numbers disabled."
  )

(show-paren-mode 1)

(global-hl-line-mode 1)

(setq org-startup-indented t)

(setq
      evil-emacs-state-cursor   `( box      ,(fb*getDefaultColorValue :orange ))
      evil-insert-state-cursor  `((bar . 3) ,(fb*getDefaultColorValue :magenta))
      evil-motion-state-cursor  `( box      ,(fb*getDefaultColorValue :base5  ))
      evil-normal-state-cursor  `( box      ,(fb*getDefaultColorValue :violet ))
      evil-replace-state-cursor `( box      ,(fb*getDefaultColorValue :red    ))
      evil-visual-state-cursor  `( box      ,(fb*getDefaultColorValue :yellow )))
      ;; evil-normal-state-cursor  `( box      ,(fb*getDefaultColorValue :green  ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; keys
;;;;
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; keys-evil
;;;;
;;

(general-define-key
 :keymaps  'evil-window-map
 "H" 'nil
 "J" 'evil-window-move-far-left
 "K" 'evil-window-move-very-top
 "L" 'evil-window-move-very-bottom
 ":" 'evil-window-move-far-right

 "h" 'nil
 "j" 'evil-window-left
 "k" 'evil-window-down
 "l" 'evil-window-up
 ";" 'evil-window-right
 )

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

(defhydra hydra-window-frame (:color red)
  "Frame"
  ("k" delete-frame "delete frame")
  ("l" make-frame   "new frame"   ))

(defhydra hydra-window-size (:color red)
  "Windows size"
  ("j" shrink-window-horizontally "horizontal -")
  ("k" shrink-window "vertical -")
  ("l" enlarge-window "vertical +")
  (";" enlarge-window-horizontally "horizontal +"))

(defun fb*scroll-other-window()
  (interactive)
  (scroll-other-window 1))
(defun fb*scroll-other-window-down ()
  (interactive)
  (scroll-other-window-down 1))
(defhydra hydra-window-scroll (:color red)
  "Scroll other window"
  ("k" fb*scroll-other-window "scroll")
  ("l" fb*scroll-other-window-down "scroll down"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; keys-transient
;;;;
;;

(spacemacs|define-transient-state evil-numbers
  :title "Evil Numbers Transient State"
  :doc
  "\n[_+_/_=_/_k_] increase number  [_-_/___/_l_] decrease  [0..9] prefix  [_q_] quit"
  :foreign-keys run
  :bindings
  ("+" evil-numbers/inc-at-pt)
  ("=" evil-numbers/inc-at-pt)
  ("k" evil-numbers/inc-at-pt)
  ("-" evil-numbers/dec-at-pt)
  ("_" evil-numbers/dec-at-pt)
  ("l" evil-numbers/dec-at-pt)
  ("q" nil :exit t)
  :evil-leader "xi."
  )

(defun fb/inc-at-pt ()
  (interactive)
  (spacemacs/evil-numbers-transient-state/evil-numbers/inc-at-pt))
(defun fb/dec-at-pt ()
  (interactive)
  (spacemacs/evil-numbers-transient-state/evil-numbers/dec-at-pt))

(defun fb/string-inflection-all-cycle ()
"cycle inflectionsand enter fold-transient-state"
  (interactive)
  (spacemacs/string-inflection-transient-state/string-inflection-all-cycle))

(defun fb/open-fold ()
"open fold and enter fold-transient-state"
  (interactive)
  (spacemacs/fold-transient-state/origami-open-node))
(defun fb/close-fold ()
"close fold and enter fold-transient-state"
  (interactive)
  (spacemacs/fold-transient-state/origami-close-node))

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
 :keymaps '(
            bookmark-bmenu-mode-map
            )
 :states  '(normal visual)
 "j" 'evil-backward-char
 "k" 'evil-previous-visual-line
 "l" 'evil-next-visual-line

 "C-;" 'bookmark-bmenu-this-window
 )

(general-define-key
 :keymaps '(
            cfw:calendar-mode-map
            )
 ";"     'nil

 "j"     'cfw:navi-previous-day-command
 "k"     'cfw:navi-previous-week-command
 "l"     'cfw:navi-next-week-command
 ";"     'cfw:navi-next-day-command

 "K"     'cfw:navi-previous-month-command
 "L"     'cfw:navi-next-month-command

 "S-TAB" 'cfw:navi-prev-item-command
 "TAB"   'cfw:navi-next-item-command

 "d"     'cfw:change-view-day
 "w"     'cfw:change-view-week
 "f"     'cfw:change-view-two-weeks
 "m"     'cfw:change-view-month

 "r"     'cfw:refresh-calendar-buffer
 "h"     'cfw:org-goto-date
 "x"     'cfw:org-clean-exit


 "SPC"   'cfw:show-details-command            ;; show in agenda
 "RET"   'cfw:org-onclick                     ;; jump
 )

(general-define-key
 :keymaps '(
            company-mode-map
            )
 "C-j" 'nil
 "C-j" 'company-indent-or-complete-common
 "C-k" 'nil
 "C-K" 'nil
 "C-k" 'company-select-previous
 "C-l" 'nil
 "C-l" 'company-select-next
 "C-;" 'nil
 "C-;" 'fb/company-complete-selection

 "C-J"   'yas-prev-field
 "C-:"   'yas-next-field-or-maybe-expand
 "C-M-:" 'yas-expand-from-trigger-key
 )

(general-define-key
 :keymaps '(evil-insert-state-map)
 ;;;; :states  '(normal visual) WILL not work with states
 "C-k" 'nil
 "M-k" 'evil-insert-digraph
 )

(general-define-key
 :keymaps '(
            company-active-map
            ;; company-search-map
            )
 ;; "C-j" 'nil
 ;; "C-j" 'company-complete-selection
 "C-k" 'nil
 "C-k" 'company-select-previous
 "C-l" 'nil
 "C-l" 'company-select-next
 "C-;" 'nil
 "C-;" 'fb/company-complete-selection
 )

(general-define-key
 :keymaps '(minibuffer-local-map)
 ;; :states  '(normal visual)
 "C-r"    'counsel-minibuffer-history
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
 :keymaps '(emoji-list-mode-map)
 :states  'motion
 "RET"    'nil
 "RET"    'emoji-list-select
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
 :keymaps '(ivy-minibuffer-map)
 "C-k" 'ivy-previous-line
 "C-l" 'ivy-next-line
 "C-;" 'ivy-alt-done
 "TAB" 'ivy-alt-done
 )

(general-define-key
 :keymaps '(ivy-switch-buffer-map)
 "C-k" 'ivy-previous-line
 "C-;" 'ivy-done
 "C-d" 'ivy-switch-buffer-kill
 )

(general-define-key
 :keymaps '(ivy-reverse-i-search-map)
 "C-k" 'ivy-previous-line
 "C-d" 'ivy-reverse-i-search-kill
 )

(general-define-key
 :keymaps '(
            ivy-occur-grep-mode-map
            ivy-occur-mode-map
            )
 :states  '(normal visual)
 "j"     'nil
 "k"     'nil
 "l"     'nil
 ";"     'nil
 "j"     'evil-backward-char
 "k"     'ivy-occur-previous-line
 "l"     'ivy-occur-next-line
 ";"     'evil-forward-char
 )

(general-define-key
 :keymaps '(
            ivy-occur-grep-mode-map
            ivy-occur-mode-map
            )
 :states  '(visual)
 "k"     'nil
 "l"     'nil
 "k"     'evil-previous-visual-line
 "l"     'evil-next-visual-line
 )

;; (general-define-key
;;  :keymaps '(jsonnet-mode-map
;;             )
;;  "C-l" 'nil
;;  )
(fb/local-leader-key
  :keymaps 'jsonnet-mode-map
  :states  '(normal visual insert)

  ;; "b"      '(                                                   :which-key "table"            :ignore t)
  "="      '(jsonnet-reformat-buffer                            :which-key "format"           )
  )

(general-define-key
 "C-M-x" 'eval-last-sexp
 )

(general-define-key
 :keymaps '(lsp-command-map)
 ;; "i"  '(:ignore t :which-key "ivy/imenu") ;;; defined in fb/leader-key
 "ii"  'lsp-ivy-workspace-symbol
 "im"  'lsp-ui-imenu
 ;; "t"  '(:ignore t  :which-key "treemacs") ;;; defined in fb/leader-key
 "ts" 'lsp-treemacs-symbols
 )

(general-define-key
 :keymaps '(
           lsp-mode-map
           )
 "C-S-k" 'lsp-ui-doc-focus-frame
  )

(general-define-key
 :keymaps '(
           lsp-ui-doc-frame-mode-map
            )
 "C-L" 'lsp-ui-doc-unfocus-frame
  )

(general-define-key
 :keymaps '(
           lsp-treemacs-error-list-mode-map
            )
 "c" 'lsp-treemacs-cycle-severity
 "x" 'lsp-treemacs-quick-fix
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
 :keymaps 'magit-mode-map
 :states  '(normal visual)
 "gj" 'magit-section-up
 "gk" 'magit-section-backward-sibling
 "gl" 'magit-section-forward-sibling
 "g;" 'magit-section-forward
 "gvaj" 'magit-section-show-level-1-all
 "gvak" 'magit-section-show-level-2-all
 "gval" 'magit-section-show-level-3-all
 "gva;" 'magit-section-show-level-4-all
 "gvj" 'magit-section-show-level-1
 "gvk" 'magit-section-show-level-2
 "gvl" 'magit-section-show-level-3
 "gv;" 'magit-section-show-level-4
 )

(general-define-key
 :keymaps 'git-rebase-mode-map
 :states  '(normal visual)
 "L" 'git-rebase-label
 "l" 'evil-next-line
 )

(general-define-key
 :prefix "C-c"
 "L" 'org-store-link
 ;; "l" 'org-store-link
 "a" 'org-agenda
 "c" 'org-capture
 )

(general-define-key
 :keymaps '(
            go-mode-map
	        json-mode-map
	        yaml-mode-map
            )
 "TAB"   'origami-recursively-toggle-node
 )

(fb/local-leader-key
  :keymaps 'solidity-mode-map
  :states  '(normal visual insert)

  "g"      '(solidity-estimate-gas-at-point :which-key "estimate-gas" )
  )

(general-define-key
 "C-s" 'swiper
 )

(general-define-key
 :keymaps '(swiper-map)
 "C-l"   'nil
 )
(general-define-key
 :keymaps '(swiper-map)
 "C-l"   'ivy-next-line
 "C-S-L" 'swiper-recenter-top-bottom
 )

(fb/local-leader-key
  :keymaps 'TeX-mode-map
  :states  '(normal visual insert)

  "c"      '(TeX-clean :which-key "clean")
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

(general-define-key
 :keymaps '(undo-tree-visualizer-mode-map)
 :states  'motion
 ;; "j" 'undo-tree-visualize-switch-branch-left        ;;; working
 "k" 'undo-tree-visualize-undo
 "l" 'undo-tree-visualize-redo
 ;; ";" 'undo-tree-visualize-switch-branch-right       ;;; working
 )

(general-define-key
 :keymaps '(writeroom-mode-map)
 "s-?"  'nil
 "M-m"   '(writeroom-toggle-mode-line :which-key "toggle-modeline")
 "C-M-<" 'writeroom-decrease-width
 "C-M->" 'writeroom-increase-width
 ;; "C-M-=" 'writeroom-adjust-width
 "C-M-=" '(writeroom-adjust-width :which-key "wr-with-=")
 )

(general-define-key
  :keymaps '(
             xref--xref-buffer-mode-map
             xref--button-map
             )
;; :states  'normal-state
  "C-;" 'xref-goto-xref
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; keys-global-leader
;;;;
;;

(fb/leader-key
  "!"   '(shell-command                                 :which-key "shell-command"                    )

  "B"   '(                                              :which-key "bookmarks"                        :ignore t)
  "BJ"  '(counsel-bookmark                              :which-key "jump"                             )
  "BS"  '(bookmark-set                                  :which-key "set"                              )
  "BL"  '(bookmark-bmenu-list                           :which-key "list"                             )

  "b"   '(                                              :which-key "buffer"                           :ignore t)
  "br"  '(revert-buffer                                 :which-key "revert"                           )

  "c"   '(                                              :which-key "comment"                          :ignore t)
  "cc"  '(evilnc-comment-operator                       :which-key "cmnt-operator"                    )
  "ci"  '(evilnc-toggle-invert-comment-line-by-line     :which-key "toggle-invert-cmnt-line-by-line"  )
  "cl"  '(evilnc-comment-or-uncomment-lines             :which-key "cmmnt-or-uncmnt-lines"            )
  "cp"  '(evilnc-comment-or-uncomment-paragraphs        :which-key "cmmnt-or-uncmnt-paragraphs"       )
  "cr"  '(comment-or-uncomment-region                   :which-key "cmmnt-or-uncmnt-region"           )
  "ct"  '(evilnc-quick-comment-or-uncomment-to-the-line :which-key "quick-cmmnt-or-uncmnt-to-the-line")
  "cy"  '(evilnc-copy-and-comment-lines                 :which-key "cp-and-cmnt-lines"                )

  "C"   '(                                              :which-key "command-log cal"                  :ignore t)
  "CA"  '(cfw:open-org-calendar                         :which-key "org-cal"                          )
  "CC"  '(fb/open-calendar                              :which-key "combined-cal"                     )
  "CS"  '(calendar                                      :which-key "show-cal"                         )
  "CL"  '(command-log-mode                              :which-key "toggle-local"                     )
  "CB"  '(clm/open-command-log-buffer                   :which-key "show-clm-buffer"                  )
  "CG"  '(global-command-log-mode                       :which-key "toggle-global"                    )

  "d"   '(                                              :which-key "dap"                              :ignore t)

  "d"   '(                                              :which-key "debug"                            :ignore t)
  "d."  '(dap-hydra                                     :which-key "hydra"                            )
  "d'"  '(dap-ui-repl                                   :which-key "repl"                             )
  "da"  '(                                              :which-key "abandon"                          )
  "daa" '(dap-disconnect                                :which-key "disconnect"                       )
  "daA" '(dap-delete-all-sessions                       :which-key "delete-all-sessions"              )
  "db"  '(                                              :which-key "breakpoints"                      :ignore t)
  "dbb" '(dap-breakpoint-toggle                         :which-key "bp-toggle"                        )
  "dbc" '(dap-breakpoint-condition                      :which-key "bp-condition"                     )
  "dbl" '(dap-breakpoint-log-message                    :which-key "bp-log-message"                   )
  "dbh" '(dap-breakpoint-hit-condition                  :which-key "bp-hit-cond"                      )
  "dba" '(dap-breakpoint-add                            :which-key "bp-add"                           )
  "dbd" '(dap-breakpoint-delete                         :which-key "bp-delete"                        )
  "dbD" '(dap-breakpoint-delete-all                     :which-key "bp-delete-all"                    )
  "dd"  '(                                              :which-key "debugging"                        :ignore t)
  "ddd" '(dap-debug                                     :which-key "debug"                            )
  "dde" '(dap-debug-edit-template                       :which-key "edit-template"                    )
  "ddl" '(dap-debug-last                                :which-key "last"                             )
  "ddr" '(dap-debug-recent                              :which-key "recent"                           )
  "de"  '(                                              :which-key "eval"                             :ignore t)
  "dee" '(dap-eval                                      :which-key "eval"                             )
  "der" '(dap-eval-region                               :which-key "eval-region"                      )
  "det" '(dap-eval-thing-at-point                       :which-key "eval-thing-at-point"              )
  "det" '(dap-ui-expressions-add                        :which-key "ui-expressions-add"               )
  "dI"  '(                                              :which-key "inspect"                          :ignore t)
  "dIi" '(dap-ui-inspect                                :which-key "ui-inspect"                       )
  "dIr" '(dap-ui-inspect-region                         :which-key "ui-inspect-region"                )
  "dIt" '(dap-ui-inspect-thing-at-point                 :which-key "ui-inspect-thing-at-point"        )

  "dc"  '(dap-continue                                  :which-key "continue"                         )
  "di"  '(dap-step-in                                   :which-key "step-in"                          )
  "dls" '(dap-tm-loaded-sources                         :which-key "sources"                          )
  "dlv" '(dap-ui-set-variable-value                     :which-key "set-variable-value"               )
  "do"  '(dap-step-out                                  :which-key "step-out"                         )
  "dr"  '(dap-restart-frame                             :which-key "restart-frame"                    )
  "ds"  '(dap-next                                      :which-key "next"                             )
  "dv"  '(dap-ui-inspect-thing-at-point                 :which-key "ui-inspect-thing-at-point"        )
  "dS"  '(                                              :which-key "switch"                           :ignore t)
  "dSs" '(dap-switch-session                            :which-key "switch-session"                   )
  "dSt" '(dap-switch-thread                             :which-key "switch-thread"                    )
  "dSf" '(dap-switch-frame                              :which-key "switch-frame"                     )
  "dT"  '(                                              :which-key "toggles"                          :ignore t)
  "dTm" '(spacemacs/toggle-dap-mouse                    :which-key "mouse"                            )
  "dw"  '(                                              :which-key "windows"                          :ignore t)
  "dwo" '(dap-go-to-output-buffer                       :which-key "go-to-output-buffer"              )
  "dwl" '(dap-ui-locals                                 :which-key "ui-locals"                        )
  "dws" '(dap-ui-sessions                               :which-key "ui-sessions"                      )
  "dwb" '(dap-ui-breakpoints                            :which-key "ui-breakpoints"                   )

  ;; "D"   '(                                              :which-key "delete"                           :ignore t)

  "e"   '(                                              :which-key "error"                            :ignore t)
  "e?"  '(flycheck-describe-checker                     :which-key "describe-checker"                 )
  "eH"  '(display-local-help                            :which-key "local-help"                       )
  "el"  '(spacemacs/goto-flycheck-error-list            :which-key "goto-list"                        )
  "eM"  '(flycheck-compile                              :which-key "compile"                          )
  "eS"  '(flycheck-set-checker-executable               :which-key "set-checker-executable"           )
  "eV"  '(flycheck-version                              :which-key "version"                          )
  "eb"  '(flycheck-buffer                               :which-key "buffer"                           )
  "ec"  '(flycheck-clear                                :which-key "clear"                            )
  "ee"  '(flycheck-explain-error-at-point               :which-key "explain-at-point"                 )
  "ei"  '(flycheck-manual                               :which-key "manual"                           )
  "et"  '(fb/toggle-flycheck-error-buffer               :which-key "toggle-list"                      )
  "en"  '(flycheck-next-error                           :which-key "next"                             )
  "ep"  '(flycheck-previous-error                       :which-key "previous"                         )
  "es"  '(flycheck-select-checker                       :which-key "select-checker"                   )
  "ev"  '(flycheck-verify-setup                         :which-key "verify-setup"                     )
  "ex"  '(flycheck-disable-checker                      :which-key "disable"                          )
  "ey"  '(flycheck-copy-errors-as-kill                  :which-key "copy-errors"                      )

  "E"   '(:keymap envrc-command-map :package envrc-mode :which-key "envrc"                            )

  "f"   '(                                              :which-key "fast/file"                        :ignore t)
  "fy"  '(fb/yank-buffer-filename                       :which-key "yank-name"                        )
  "ff"  '(counsel-find-file                             :which-key "find"                             )
  "fs"  '(save-buffer                                   :which-key "save-buffer"                      )
  "fS"  '(save-some-buffers                             :which-key "save-some-buffer"                 )

  "g"   '(                                              :which-key "git"                              :ignore t)
  "gb"  '(spacemacs/git-blame-transient-state/body      :which-key "blameTransient"                   )
  "gc"  '(magit-clone                                   :which-key "clone"                            )
  "gfl" '(magit-log-buffer-file                         :which-key "logs"                             )
  "gfd" '(magit-diff                                    :which-key "diff"                             )
  "gi"  '(magit-init                                    :which-key "init"                             )
  "gL"  '(magit-list-repositories                       :which-key "repolist"                         )
  "gm"  '(magit-dispatch                                :which-key "dispatch"                         )
  "gs"  '(magit-status                                  :which-key "status"                           )
  "gS"  '(magit-stage-file                              :which-key "stage"                            )
  "gU"  '(magit-unstage-file                            :which-key "unstage"                          )

  "G"   '(                                              :which-key "go"                               :ignore t)
  "GB"  '(browse-url                                    :which-key "browser"                          )
  "GF"  '(browse-url-of-file                            :which-key "browser"                          )

  "i"   '(                                              :which-key "imenu"                            :ignore t)
  "ii"  '(imenu-list                                    :which-key "imenulist"                        )

  "j"   '(dired-jump                                    :which-key "dired"                            )

  "J"   '(                                              :which-key "emoji"                            :ignore t)
  "JD"  '(emoji-describe                                :which-key "describe"                         )
  "JI"  '(emoji-insert                                  :which-key "insert"                           )
  "JL"  '(emoji-list                                    :which-key "list"                             )
  "JR"  '(emoji-recent                                  :which-key "recent"                           )
  "JS"  '(emoji-search                                  :which-key "search"                           )

  "L"   '(                                              :which-key "lsp"                              :ignore t)
  "LD"  '(xref-find-definitions                         :which-key "find-def"                         )
  "LR"  '(xref-find-references                          :which-key "find-ref"                         )
  "LN"  '(lsp-ui-find-next-reference                    :which-key "next-ref"                         )
  "LP"  '(lsp-ui-find-prev-reference                    :which-key "prev-ref"                         )
  "LS"  '(counsel-imenu                                 :which-key "counsel0imenu"                    )
  "LE"  '(lsp-ui-flycheck-list                          :which-key "list"                             )
  "LS"  '(lsp-ui-sideline-mode                          :which-key "sideline"                         )
  "LX"  '(lsp-execute-code-action                       :which-key "action"                           )
  "LL"  '(lsp                                           :which-key "start-lsp"                        )

  "l"   '(:keymap lsp-command-map :package lsp-mode     :which-key "lsp"                              )
  "l="  '(                                              :which-key "formatting"                       :ignore t)
  "lF"  '(                                              :which-key "folders"                          :ignore t)
  "lG"  '(                                              :which-key "peeks"                            :ignore t)
  "lT"  '(                                              :which-key "toggles"                          :ignore t)
  "la"  '(                                              :which-key "actions"                          :ignore t)
  "lg"  '(                                              :which-key "goto"                             :ignore t)
  "lgG" '(xref-find-definitions-other-window            :which-key "definition-other-window"          )
  "lh"  '(                                              :which-key "help"                             :ignore t)
  "lr"  '(                                              :which-key "refactoring"                      :ignore t)
  "lw"  '(                                              :which-key "workspace"                        :ignore t)
  ;; "li"  '(                                              :which-key "ivy/imenu"                        :ignore t)
  ;; "lt"  '(                                              :which-key "treemacs"                         :ignore t)
  ;; "ltc" '(lsp-treemacs-call-hierarchy                   :which-key "call-hierarchy"                   )
  ;; "lte" '(lsp-treemacs-errors-list                      :which-key "errors"                           )
  ;; "lti" '(lsp-treemacs-implementations                  :which-key "implementations"                  )
  ;; "ltr" '(lsp-treemacs-references                       :which-key "references"                       )
  ;; "ltt" '(lsp-treemacs-type-hierarchy                   :which-key "type-hierarchy"                   )
  ;; "ltx" '(lsp-treemacs-quick-fix                        :which-key "quickfix"                         )
  ;; ;; "lts" '(lsp-treemacs-symbols                          :which-key "symbols"                          ) ;; already implemented in lsp-mode-map

  ;; "l="  '(                                              :which-key "formatting"                       :ignore t)
  ;; "la"  '(                                              :which-key "code actions"                     :ignore t)
  ;; "lF"  '(                                              :which-key "folders"                          :ignore t)
  ;; "lG"  '(                                              :which-key "peek"                             :ignore t)
  ;; "lg"  '(                                              :which-key "goto"                             :ignore t)
  ;; "lh"  '(                                              :which-key "help"                             :ignore t)
  ;; "lr"  '(                                              :which-key "refactor"                         :ignore t)
  ;; "ls"  '(                                              :which-key "sessions"                         :ignore t)
  ;; "lT"  '(                                              :which-key "toggle"                           :ignore t)
  ;; "lx"  '(lsp-execute-code-action                       :which-key "action"                           )

  "n"   '(                                              :which-key "numbers"                          :ignore t)
  "n+"  '(fb/inc-at-pt                                  :which-key "+"                                )
  "n="  '(fb/inc-at-pt                                  :which-key "+"                                )
  "n-"  '(fb/dec-at-pt                                  :which-key "-"                                )
  "n_"  '(fb/dec-at-pt                                  :which-key "-"                                )

  "o"   '(                                              :which-key "org"                              :ignore t)
  "oa"  '(org-agenda                                    :which-key "agenda"                           )
  "od"  '(evil-ex-show-digraphs                         :which-key "digraphs"                         )
  "oc"  '(org-capture                                   :which-key "capture"                          )
  "ol"  '(org-store-link                                :which-key "store-link"                       )
  "ok"  '(org-open-at-point-global                      :which-key "follow-link"                      )

  "oi"  '(                                                                      :which-key "go2file"               :ignore t)
  "oiu" '((lambda()(interactive)(find-file "~/NOTES/AKTUELLES.org"           )) :which-key "AKTUELLES"             )
  "oi1" '((lambda()(interactive)(find-file "~/NOTES/〇/1  UNSORTIERTES.org"   )) :which-key "UNSORTIERTES"          )
  "oi2" '((lambda()(interactive)(find-file "~/NOTES/〇/2  IDEEN.org"          )) :which-key "IDEEN"                 )
  "oi3" '((lambda()(interactive)(find-file "~/NOTES/〇/3  FRAGEN.org"         )) :which-key "FRAGEN"                )
  "oi4" '((lambda()(interactive)(find-file "~/NOTES/〇/4  RECHERCHE.org"      )) :which-key "RECHERCHE"             )
  "oi5" '((lambda()(interactive)(find-file "~/NOTES/〇/5  BIBLIO~.org"        )) :which-key "BIBLIO~"               )
  "oi6" '((lambda()(interactive)(find-file "~/NOTES/〇/6  I.org"              )) :which-key "INFORMATION"           )
  "oi7" '((lambda()(interactive)(find-file "~/NOTES/〇/7  ToDO.org"           )) :which-key "TODO"                  )
  "oia" '((lambda()(interactive)(find-file "~/NOTES/〇/7a ANSCHAFFUNGEN.org"  )) :which-key "ANSCHAFFUNGEN"         )
  "oi8" '((lambda()(interactive)(find-file "~/NOTES/〇/8  INSTALLATIONEN.org" )) :which-key "INSTALLATIONEN"        )
  "oi9" '((lambda()(interactive)(find-file "~/NOTES/〇/9  ROUTINEN.org"       )) :which-key "ROUTINEN"              )
  "oi0" '((lambda()(interactive)(find-file "~/NOTES/〇/10 ERKENNTNISSE.org"   )) :which-key "ERKENNTNISSE"          )
  "oie" '((lambda()(interactive)(find-file "~/NOTES/〇/11 ERLEDIGTES.org"     )) :which-key "ERLEDIGTES"            )

  "p"   '(projectile-command-map                        :which-key "projectile"                       )

  "P"   '(                                              :which-key "project"                          :ignore t)
  "PD"  '(                                              :which-key "project-direnv"                   :ignore t)
  "PDA" '(direnv-allow                                  :which-key "project-direnv-allow"             )
  "PDF" '(direnv-update-environment                     :which-key "project-direnv-file"              )
  "PDD" '(direnv-update-directory-environment           :which-key "project-direnv-dir"               )

  "r"   '(                                              :which-key "re-~"                             :ignore t)
  "rc"  '(fb/literate-recompile                         :which-key "recompile-emacs.d"                )
  "rd"   '(                                             :which-key "reloadDirLocals"                  :ignore t)
  "rdb" '(fb/reload-dir-locals-current-buffer           :which-key "reloadDirLocalsCurrentBuffer"     )
  "rda" '(fb/reload-dir-locals-all-directory-buffer     :which-key "reloadDirLocalsDirBuffer"         )
  "rf"  '(                                              :which-key "reformat"                         :ignore t)
  "rfh" '(fb/break-here                                 :which-key "break-here"                       )
  "rfc" '(fb/break-sub-sentence                         :which-key "break-sub"                        )
  "rfs" '(fb/break-sentence                             :which-key "break-sentence"                   )

  "rr"  '(redraw-display                                :which-key "redraw-display"                   )
  "rl"  '(fb/reload-config                              :which-key "reload init.el"                   )

  "s"   '(                                              :which-key "move"                             :ignore t)
  "sb"  '(beginning-of-defun                            :which-key "func-bg"                          )
  "se"  '(end-of-defun                                  :which-key "func-be"                          )

  "S"   '(                                              :which-key "SMERGE"                           :ignore t)
  "SD"  '(                                              :which-key "DIFF"                             :ignore t)
  "SDA" '(smerge-diff-upper-lower                       :which-key "diff-upper-lower"                 )
  "SDB" '(smerge-diff-base-upper                        :which-key "diff-base-upper"                  )
  "SDE" '(smerge-ediff                                  :which-key "ediff"                            )
  "SDL" '(smerge-diff-base-lower                        :which-key "diff-base-lower"                  )
  "SK"  '(                                              :which-key "KEEP"                             :ignore t)
  "SKA" '(smerge-keep-all                               :which-key "keep-all"                         )
  "SKB" '(smerge-keep-base                              :which-key "keep-base"                        )
  "SKC" '(smerge-keep-current                           :which-key "keep-current"                     )
  "SKL" '(smerge-keep-lower                             :which-key "keep-lower"                       )
  "SKU" '(smerge-keep-upper                             :which-key "keep-upper"                       )
  "SN"  '(                                              :which-key "NAVIGATION"                       :ignore t)
  "SNN" '(smerge-next                                   :which-key "next"                             )
  "SNP" '(smerge-prev                                   :which-key "prev"                             )
  "SNV" '(smerge-vc-next-conflict                       :which-key "vc-next-conflict"                 )
  "SNR" '(smerge-refine                                 :which-key "refine"                           )
  "SR"  '(                                              :which-key "RESOLVE"                          :ignore t)
  "SRR" '(smerge-resolve                                :which-key "resolve"                          )
  "SRA" '(smerge-resolve-all                            :which-key "resolve-all"                      )
  "SM"  '(                                              :which-key "MENUE"                            :ignore t)
  "SMC" '(smerge-context-menu                           :which-key "context-menu"                     )
  "SMP" '(smerge-popup-context-menu                     :which-key "popup-context-menu"               )
  "SMM" '(smerge-mode-menu                              :which-key "mode-menu"                        )
  "SO"  '(                                              :which-key "OTHER"                            :ignore t)
  "SOK" '(smerge-kill-current                           :which-key "kill-current"                     )
  "SOU" '(smerge-makeup-conflict                        :which-key "makeup-conflict"                  )
  "SOS" '(smerge-start-session                          :which-key "start-session"                    )
  "SOC" '(smerge-combine-with-next                      :which-key "combine-with-next"                )
  "SOW" '(smerge-swap                                   :which-key "swap"                             )
  "SOA" '(smerge-auto-combine                           :which-key "auto-combine"                     )
  "SOM" '(smerge-mode                                   :which-key "mode"                             )

  "t"   '(                                              :which-key "toggles"                          :ignore t)
  "td"  '(                                              :which-key "todo"                             :ignore t)
  "tdi" '(hl-todo-insert                                :which-key "insert"                           )
  "tdn" '(hl-todo-next                                  :which-key "next"                             )
  "tdo" '(hl-todo-occur                                 :which-key "occur"                            )
  "tdp" '(hl-todo-previous                              :which-key "prev"                             )
  "ti"  '(imenu-list-smart-toggle                       :which-key "imenu"                            )
  "tl"  '(toggle-truncate-lines                         :which-key "truncate-lines"                   )
  "tm"  '(treemacs                                      :which-key "treemacs"                         )
  "tn"  '(                                              :which-key "line-numbers"                     :ignore t)
  "tna" '(spacemacs/toggle-absolute-line-numbers        :which-key "line-absolute"                    )
  "tnr" '(spacemacs/toggle-relative-line-numbers        :which-key "line-relative"                    )
  "tnv" '(spacemacs/toggle-visual-line-numbers          :which-key "line-visual"                      )
  "tt"  '(counsel-load-theme                            :which-key "choose theme"                     )
  "tw"  '(whitespace-mode                               :which-key "whitespace"                       )
  "T"   '(                                              :which-key "toggles"                          :ignore t)
  "TW"  '(fb/toggle-which-key-sort-order                :which-key "whickKey-sort-order"              )

  "u"   '(undo-tree-visualize                           :which-key "undotree"                         )

  "w"   '(                                              :which-key "window"                           :ignore t)
  "wa"  '(aw-show-dispatch-help                         :which-key "ace-window"                       )
  "wb"  '(balance-windows                               :which-key "balance"                          )
  "wd"  '(ace-delete-window                             :which-key "ace-delete"                       )
  "we"  '(:keymap evil-window-map :package evil         :which-key "evil-window"                      )
  "wf"  '(aw-flip-window                                :which-key "flip"                             )
  "wg"  '(hydra-window-frame/body                       :which-key "frame"                            )
  "wh"  '(fb/aw-split-window-horz                       :which-key "split |"                          )
  "wi"  '(winner-mode                                   :which-key "winner-mode"                      )
  "wl"  '(hydra-window-size/body                        :which-key "resize"                           )
  "wm"  '(delete-other-windows                          :which-key "maximize"                         )
  "wo"  '(hydra-window-scroll/body                      :which-key "scroll"                           )
  "wp"  '(ace-swap-window                               :which-key "ace-swap"                         )
  "wr"  '(fb/winner-redo                                :which-key "winner-redo"                      )
  "ws"  '(ace-select-window                             :which-key "ace-select"                       )
  "wu"  '(fb/winner-undo                                :which-key "winner-undo"                      )
;;;; TODO harmonize with =SPW w e v=
;;;; cf. RESULT vs ACTION
  "wv"  '(fb/aw-split-window-vert                       :which-key "split -"                          )
  "ww"  '(writeroom-mode                                :which-key "writeroom-toggle"                 )
  "wx"  '(ace-delete-other-windows                      :which-key "ace-delete-other"                 )

  "W"   '(:keymap wrap-region-mode-map :package wrap-region :which-key "wrap-region"                  )

  "xa"  '(                                              :which-key "align"                            :ignore t)
  "xa%" '(spacemacs/align-repeat-percent                :which-key "repeat-percent"                   )
  "xa&" '(spacemacs/align-repeat-ampersand              :which-key "repeat-ampersand"                 )
  "xa(" '(spacemacs/align-repeat-left-paren             :which-key "repeat-left-paren"                )
  "xa)" '(spacemacs/align-repeat-right-paren            :which-key "repeat-right-paren"               )
  "xa{" '(spacemacs/align-repeat-left-curly-brace       :which-key "repeat-left-curly-brace"          )
  "xa}" '(spacemacs/align-repeat-right-curly-brace      :which-key "repeat-right-curly-brace"         )
  "xa[" '(spacemacs/align-repeat-left-square-brace      :which-key "repeat-left-square-brace"         )
  "xa]" '(spacemacs/align-repeat-right-square-brace     :which-key "repeat-right-square-brace"        )
  "xa," '(spacemacs/align-repeat-comma                  :which-key "repeat-comma"                     )
  "xa." '(spacemacs/align-repeat-decimal                :which-key "repeat-decimal"                   )
  "xa:" '(spacemacs/align-repeat-colon                  :which-key "repeat-colon"                     )
  "xa;" '(spacemacs/align-repeat-semicolon              :which-key "repeat-semicolon"                 )
  "xa=" '(spacemacs/align-repeat-equal                  :which-key "repeat-equal"                     )
  "xa\\"'(spacemacs/align-repeat-backslash              :which-key "repeat-backslash"                 )
  "xaa" '(align                                         :which-key "align"                            )
  "xac" '(align-current                                 :which-key "align-current"                    )
  "xam" '(spacemacs/align-repeat-math-oper              :which-key "align-repeat-math-oper"           )
  "xar" '(spacemacs/align-repeat                        :which-key "align-repeat"                     )
  "xa|" '(spacemacs/align-repeat-bar                    :which-key "align-repeat-bar"                 )
  "xc"  '(count-region                                  :which-key "count-region"                     )
  "xd"  '(                                              :which-key "delete"                           )
  "xdl" '(delete-blank-lines                            :which-key "delete-blank-lines"               )
  "xdw" '(delete-trailing-whitespace                    :which-key "delete-trailing-whitespace"       )

  "xi"  '(                                              :which-key "inflection"                       :ignore t)
  "xic" '(string-inflection-lower-camelcase             :which-key "camel"                            )
  "xiC" '(string-inflection-camelcase                   :which-key "camel-lower"                      )
  "xid" '(fb/downcase-word                              :which-key "down"                             )
  "xiD" '(fb/upcase-word                                :which-key "up"                               )
  "xii" '(fb/string-inflection-all-cycle                :which-key "transient"                        )
  "xi." '(fb/string-inflection-all-cycle                :which-key "transient"                        )
  "xi-" '(string-inflection-kebab-case                  :which-key "kebab"                            )
  "xik" '(string-inflection-kebab-case                  :which-key "kebab"                            )
  "xil" '(downcase-region                               :which-key "downcase-region"                  )
  "xi_" '(string-inflection-underscore                  :which-key "snake"                            )
  "xis" '(string-inflection-underscore                  :which-key "snake"                            )
  "xit" '(fb/titlecase-word                             :which-key "title"                            )
  "xiu" '(string-inflection-capital-underscore          :which-key "snake-upper"                      )
  "xiU" '(string-inflection-upcase                      :which-key "upper"                            )

  "xj"  '(                                              :which-key "justification"                    :ignore t)
  "xjc" '(set-justification-center                      :which-key "justification-center"             )
  "xjf" '(set-justification-full                        :which-key "justification-full"               )
  "xjl" '(set-justification-left                        :which-key "justification-left"               )
  "xjn" '(set-justification-none                        :which-key "justification-none"               )
  "xjr" '(set-justification-right                       :which-key "justification-right"              )
  "xl"  '(                                              :which-key "sort-lines"                       )
  "xlc" '(spacemacs/sort-lines-by-column                :which-key "sort-lines-by-column"             )
  "xlC" '(spacemacs/sort-lines-by-column-reverse        :which-key "sort-lines-by-column-reverse"     )
  "xls" '(spacemacs/sort-lines                          :which-key "sort-lines"                       )
  "xlS" '(spacemacs/sort-lines-reverse                  :which-key "sort-lines-reverse"               )
  "xlu" '(spacemacs/uniquify-lines                      :which-key "uniquify-lines"                   )
  "xt"  '(                                              :which-key "transpose"                        )
  "xtc" '(transpose-chars                               :which-key "transpose-chars"                  )
  "xte" '(transpose-sexps                               :which-key "transpose-sexps"                  )
  "xtl" '(transpose-lines                               :which-key "transpose-lines"                  )
  "xtp" '(transpose-paragraphs                          :which-key "transpose-paragraphs"             )
  "xts" '(transpose-sentences                           :which-key "transpose-sentences"              )
  "xtw" '(transpose-words                               :which-key "transpose-words"                  )
  "xU"  '(upcase-region                                 :which-key "upcase-region"                    )
  "xu"  '(downcase-region                               :which-key "downcase-region"                  )

  "X"   '(                                              :which-key "insert"                           :ignore t)
  "XI*" '(insert-char                                   :which-key "insert unicode"                   )

  "y"   '(                                              :which-key "yasnippets"                       :ignore t)
  "yy"  '(yas-insert-snippet                            :which-key "insert"                           )
  "yr"  '(yas-reload-all                                :which-key "reload-all"                       )
  "yv"  '(yas-visit-snippet-file                        :which-key "visit"                            )

  "z"   '(                                              :which-key "fold"                             :ignore t)
  "zc"  '(fb/close-fold                                 :which-key "close"                            )
  "zo"  '(fb/open-fold                                  :which-key "open"                             )

  ";"   '(counsel-switch-buffer                         :which-key "switch-buffer"                    )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; dart-keybindings
;;;;
;;

(fb/local-leader-key
  :keymaps 'dart-mode-map
  :states  '(normal visual insert)


  "d"      '(lsp-dart-dap-open-devtools                         :which-key "devtools"               )

  "h"      '(                                                   :which-key "hot"                    :ignore t)
  "hs"     '(lsp-dart-flutter-hot-restart                       :which-key "restart"                )
  "hl"     '(lsp-dart-flutter-hot-reload                        :which-key "reload"                 )

  "o"      '(                                                   :which-key "outline"                :ignore t)
  "od"     '(lsp-dart-show-outline                              :which-key "dart"                   )
  "of"     '(lsp-dart-show-flutter-outline                      :which-key "flutter"                )

  "p"      '(                                                   :which-key "pub"                    :ignore t)
  "pg"     '(lsp-dart-pub-get                                   :which-key "get"                    )
  "pu"     '(lsp-dart-pub-upgrade                               :which-key "upgrade"                )
  "pg"     '(lsp-dart-pub-outdated                              :which-key "outdated"               )

  "r"      '(lsp-dart-run                                       :which-key "run"                    )

  "t"      '(                                                   :which-key "test"                   :ignore t)
  "td"     '(                                                   :which-key "debug"                  :ignore t)
  "tdl"    '(lsp-dart-debug-last-test                           :which-key "last"                   )
  "tdp"    '(lsp-dart-debug-test-at-point                       :which-key "point"                  )
  "tr"     '(                                                   :which-key "run"                    :ignore t)
  "tra"    '(lsp-dart-run-all-tests                             :which-key "all"                    )
  "trl"    '(lsp-dart-run-last-test                             :which-key "last"                   )
  "trp"    '(lsp-dart-run-test-at-point                         :which-key "point"                  )
  "trf"    '(lsp-dart-run-test-file                             :which-key "file"                   )

  "tt"     '(lsp-dart-test-show-tree                            :which-key "tree"                   )
  "tv"     '(lsp-dart-visit-last-test                           :which-key "visit"                  )



  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; go-keybindings
;;;;
;;

(fb/local-leader-key
  :keymaps 'go-mode-map
  :states  '(normal visual insert)

  "a"      '(go-import-add                                      :which-key "import-add"       )


  "d"      '(                                                   :which-key "godef"            :ignore t)
  "dd"     '(godef-describe                                     :which-key "run-main"         )
  "dj"     '(godef-jump                                         :which-key "run-main"         )
  "do"     '(godef-jump-other-window                            :which-key "run-main"         )

  "g"      '(                                                   :which-key "goto"             :ignore t)
  "ga"     '(go-goto-arguments                                  :which-key "run-main"         )
  "gd"     '(go-goto-docstring                                  :which-key "run-main"         )
  "gf"     '(go-goto-function                                   :which-key "run-main"         )
  "gi"     '(go-goto-imports                                    :which-key "run-main"         )
  "gm"     '(go-goto-method-receiver                            :which-key "run-main"         )
  "gn"     '(go-goto-function-name                              :which-key "run-main"         )
  "gr"     '(go-goto-return-values                              :which-key "run-main"         )

  "i"      '(prog-indent-sexp                                   :which-key "indent"           )

  "t"      '(                                                   :which-key "test"             :ignore t)
  "tg"     '(                                                   :which-key "generate"         :ignore t)
  "tgg"    '(go-gen-test-dwim                                   :which-key "dwim"             )
  "tgf"    '(go-gen-test-exported                               :which-key "exported"         )
  "tgF"    '(go-gen-test-all                                    :which-key "all"              )
  "tP"     '(spacemacs/go-run-package-tests-nested              :which-key "nested"           )
  "tp"     '(spacemacs/go-run-package-tests                     :which-key "tests"            )
  "ts"     '(spacemacs/go-run-test-current-suite                :which-key "suite"            )
  "tc"     '(spacemacs/go-run-test-current-function             :which-key "function"         )
  "tt"     '(                                                   :which-key "go-test"          :ignore t)
  "ttbb"   '(go-test-current-benchmark                          :which-key "bench"            )
  "ttbf"   '(go-test-current-file-benchmarks                    :which-key "bench-file"       )
  "ttbp"   '(go-test-current-project-benchmarks                 :which-key "bench-project"    )
  "ttc"    '(go-test-current-coverage                           :which-key "coverage"         )
  "tta"    '(go-test-current-test-cache                         :which-key "cache"            )
  "ttf"    '(go-test-current-file                               :which-key "file"             )
  "ttp"    '(go-test-current-project                            :which-key "project"          )
  "ttt"    '(go-test-current-test                               :which-key "test"             )

  "T"      '(                                                   :which-key "toggle"           :ignore t)
  "TB"     '(spacemacs/toggle-go-test-benchmark                 :which-key "test-benchmark"   )
  "TC"     '(spacemacs/toggle-go-test-coverage                  :which-key "test-coverage"    )
  "TT"     '(spacemacs/toggle-go-test-testify-for-testing       :which-key "use-testify"      )
  "TV"     '(spacemacs/toggle-go-test-verbose                   :which-key "test-verbose"     )


  "x"      '(                                                   :which-key "execute"          :ignore t)
  "xx"     '(spacemacs/go-run-main                              :which-key "run-main"         )

  "_"      '(fb/golangDashUnused                                :which-key "dash-unused"      )

  ;; "tt"     '((lambda () (interactive)(org-todo 'todo))          :which-key "todo"             )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; json-keybindings
;;;;
;;

(fb/local-leader-key
  :keymaps 'json-mode-map
  :states  '(normal visual insert)


  "b"     '(json-mode-beautify                                 :which-key "beautify"          )

  "n"      '(                                                   :which-key "number/null"      :ignore t)
  "nk"     '(json-decrement-number-at-point                     :which-key "dec"              )
  "nl"     '(json-increment-number-at-point                     :which-key "inc"              )
  "nn"     '(json-nullify-sexp                                  :which-key "nullify"          )

  "p"      '(                                                   :which-key "pretty"           :ignore t)
  "ppp"    '(json-pretty-print                                  :which-key "region"           )
  "ppo"    '(json-pretty-print-ordered                          :which-key "region-ordered"   )
  "pbb"    '(json-pretty-print-buffer                           :which-key "buffer"           )
  "pbo"    '(json-pretty-print-buffer-ordered                   :which-key "buffer-ordered"   )

  "P"      '(                                                   :which-key "path"             :ignore t)
  "PS"     '(json-mode-show-path                                :which-key "show"             )
  "PP"     '(json-mode-kill-path                                :which-key "kill"             )

  "r"      '(json-reformat-region                               :which-key "reformat-region"  )

  "t"      '(json-toggle-boolean                                :which-key "toggle-bool"      )
  )

(fb/local-leader-key
  :keymaps 'inferior-python-mode-map
  :states  '(normal visual insert)

  "c"      '(comint-clear-buffer                                :which-key "clear"            )

  "pn"     '(comint-next-prompt                                 :which-key "next"             )
  "pp"     '(comint-previous-prompt                             :which-key "previous"         )

  ;; ""      '(backward-kill-word                                 :which-key ""        )
  ;; ""      '(comint-accumulate                                  :which-key ""        )
  ;; ""      '(comint-bol-or-process-mark                         :which-key ""        )
  ;; ""      '(comint-copy-old-input                              :which-key ""        )
  ;; ""      '(comint-delchar-or-maybe-eof                        :which-key ""        )
  ;; ""      '(comint-delete-output                               :which-key ""        )
  ;; ""      '(comint-dynamic-list-input-ring                     :which-key ""        )
  ;; ""      '(comint-get-next-from-history                       :which-key ""        )
  ;; ""      '(comint-history-isearch-backward-regexp             :which-key ""        )
  ;; ""      '(comint-insert-input                                :which-key ""        )
  ;; ""      '(comint-insert-previous-argument                    :which-key ""        )
  ;; ""      '(comint-interrupt-subjob                            :which-key ""        )
  ;; ""      '(comint-kill-input                                  :which-key ""        )
  ;; ""      '(comint-next-input                                  :which-key ""        )
  ;; ""      '(comint-next-matching-input-from-input              :which-key ""        )
  ;; ""      '(comint-previous-input                              :which-key ""        )
  ;; ""      '(comint-previous-matching-input-from-input          :which-key ""        )
  ;; ""      '(comint-quit-subjob                                 :which-key ""        )
  ;; ""      '(comint-send-eof                                    :which-key ""        )
  ;; ""      '(comint-send-input                                  :which-key ""        )
  ;; ""      '(comint-show-maximum-output                         :which-key ""        )
  ;; ""      '(comint-show-output                                 :which-key ""        )
  ;; ""      '(comint-stop-subjob                                 :which-key ""        )
  ;; ""      '(comint-write-output                                :which-key ""        )
  ;; ""      '(delete-forward-char                                :which-key ""        )
  ;; ""      '(evil-scroll-down                                   :which-key ""        )
  ;; ""      '(python-shell-completion-complete-or-indent         :which-key ""        )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; mermaid-keybindings
;;;;
;;

(fb/local-leader-key
  :keymaps 'mermaid-mode-map
  :states  '(normal visual insert)

  "c"      '(                                                   :which-key "compile"          :ignore t)
  "cc"     '(mermaid-compile                                    :which-key "compile"          )
  "cf"     '(mermaid-compile-file                               :which-key "file"             )
  "cb"     '(mermaid-compile-buffer                             :which-key "buffer"           )
  "cr"     '(mermaid-compile-region                             :which-key "region"           )
  "b"      '(mermaid-open-browser                               :which-key "browser"          )
  "d"      '(mermaid-open-doc                                   :which-key "doc"              )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; nix-keybindings
;;;;
;;

(fb/local-leader-key
  :keymaps 'nix-mode-map
  :states  '(normal visual insert)

  ;; "g"      '(                                                   :which-key "goto"             :ignore t)
  "="      '(nix-mode-format                                    :which-key "format"           )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; orgmode-keybindings
;;;;
;;

(evil-set-initial-state 'org-agenda-mode 'motion)

(general-define-key
 :keymaps 'org-agenda-mode-map
 ;; :states  'motion
 :states  '(normal visual motion)

 "C-S-j" 'org-agenda-todo-previousset
 "C-:"   'org-agenda-todo-nextset

 "C-M-j" 'org-agenda-date-earlier-minutes
 "C-j"   'org-agenda-date-earlier-hours
 "C-k"   'org-agenda-do-date-earlier
 "C-l"   'org-agenda-do-date-later
 "C-;"   'org-agenda-date-later-hours
 "C-M-;" 'org-agenda-date-later-minutes

 "M-k"   'org-agenda-drag-line-backward
 "M-l"   'org-agenda-drag-line-forward

 "C-M-K" 'org-agenda-priority-up
 "C-M-L" 'org-agenda-priority-down

 "gl"    'org-agenda-next-item
 "gk"    'org-agenda-previous-item

 "k"     'org-agenda-previous-line
 "l"     'org-agenda-next-line

 "a"     'org-agenda-append-agenda

 "cc"    'org-agenda-clock-cancel
 "cg"    'org-agenda-clock-goto
 "cr"    'org-agenda-clockreport-mode
 "ci"    'org-agenda-clock-in

 "co"    'org-agenda-clock-out
 "cI"    'org-agenda-show-clocking-issues

 "ct"    'org-agenda-set-tags

 "dd"    'org-agenda-kill
 "da"    'org-agenda-toggle-archive-tag

 "e"     'org-agenda-set-effort

 "fa"    'org-agenda-filter-remove-all
 "fc"    'org-agenda-filter-by-category
 "fh"    'org-agenda-filter-by-top-headline
 "fe"    'org-agenda-filter-by-effort
 "ft"    'org-agenda-filter-by-tag
 "fr"    'org-agenda-filter-by-regexp
 "fs"    'org-agenda-limit-interactively

 "gc"    'org-agenda-goto-calendar
 "gd"    'org-agenda-goto-date
 "gh"    'org-agenda-holidays
 "gm"    'org-agenda-phases-of-moon
 "gn"    'org-agenda-add-note
 "gr"    'org-agenda-redo-all
 "gs"    'org-agenda-sunrise-sunset
 "gt"    'org-agenda-show-tags

 "gC"    'org-agenda-convert-date
 "gD"    'org-agenda-view-mode-dispatch

 "gH"    'evil-window-top
 "gM"    'evil-window-middle
 "gL"    'evil-window-bottom

 "i"     'org-agenda-diary-entry

 "m"     'org-agenda-bulk-toggle
 "MT"    'org-agenda-bulk-toggle-all
 "MA"    'org-agenda-bulk-mark-all
 "MR"    'org-agenda-bulk-mark-regexp
 "MU"    'org-agenda-bulk-unmark-all
 "x"     'org-agenda-bulk-action

 "oc"    'org-agenda-capture
 "p"     'org-agenda-date-prompt

 "q"     'org-agenda-quit
 "Q"     'org-agenda-exit

 "r"     'org-agenda-redo
 "t"     'org-agenda-todo
 "u"     'org-agenda-undo

 "va"    'org-agenda-view-mode-dispatch
 "vr"    'org-agenda-reset-view
 "vd"    'org-agenda-day-view
 "vf"    'org-agenda-fortnight-view
 "vw"    'org-agenda-week-view
 "vm"    'org-agenda-month-view
 "vy"    'org-agenda-year-view

 "."     'org-agenda-goto-today

 "[["    'org-agenda-earlier
 "]]"    'org-agenda-later

 "+"     'org-agenda-manipulate-query-add
 "-"     'org-agenda-manipulate-query-subtract

 "A"     'org-agenda-archives-mode
 "F"     'org-agenda-follow-mode
 "G"     'org-agenda-clock-goto
 ;; "H"     '
 ;; "J"     '
 "D"     'org-agenda-toggle-diary
 "I"     'org-agenda-clock-in
 "L"     'org-agenda-log-mode
 "O"     'org-agenda-clock-out
 "P"     'org-agenda-show-the-flagging-note
 "R"     'org-agenda-clockreport-mode
 "T"     'org-timer-set-timer
 "Z"     'org-agenda-dim-blocked-tasks

 "TAB"        'org-agenda-goto
 "RET"        'org-agenda-switch-to
 "<backtab>"  'org-agenda-show
 "<S-return>" 'org-agenda-show
 )

(general-define-key
 :keymaps 'org-mode-map
 "C-'" 'nil
 )

(general-define-key
 :keymaps 'org-mode-map
"M-<return>"   'fb/org-meta-return
"M-S-<return>" 'org-insert-todo-subheading
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

(general-define-key
 :keymaps '(org-mode-map)
 :states  '(normal visual)
 "gj"     'outline-up-heading
 "gl"     'org-forward-heading-same-level
 )

(general-define-key
 :keymaps  'org-mode-map
 "C-M-S-j" 'org-shiftleft
 "C-M-S-k" 'org-shiftup
 "C-M-S-l" 'org-shiftdown
 "C-M-:"   'org-shiftright
 )

(general-define-key
 :keymaps 'org-mode-map
 :states  '(normal motion)
 "C-J"    'org-shiftcontrolleft
 "C-K"    'org-shiftcontrolup
 "C-L"    'org-shiftcontroldown
 "C-:"    'org-shiftcontrolright
 )

(general-define-key
 :keymaps 'org-mode-map
 "C-M-j"  'org-shiftmetaleft
 "C-M-k"  'org-metaup
 "C-M-l"  'org-metadown
 "C-M-;"  'org-shiftmetaright
 )

(general-define-key
 :keymaps 'org-mode-map
 "M-j"    'org-metaleft
 "M-k"    'org-shiftmetaup
 "M-l"    'org-shiftmetadown
 "M-;"    'org-metaright
 )
(defun fb*org-mode-meta-bindings ()
  (general-define-key
   :keymaps 'outline-mode-map
   :states  'normal
   "M-j"    'nil
   "M-k"    'nil
   "M-l"    'nil
   "M-;"    'nil
   ))

(defun fb*org-mode-keybindings-h ()
  (fb*org-mode-meta-bindings)
)

(add-hook 'org-mode-hook 'fb*org-mode-keybindings-h)

(fb/local-leader-key
  :keymaps 'org-mode-map
  :states  '(normal visual insert)

  "a"      '(org-agenda                                         :which-key "agenda"           )

  "b"      '(                                                   :which-key "table"            :ignore t)
  "bb"     '(org-table-blank-field                              :which-key "blank"            )
  "bc"     '(                                                   :which-key "create"           :ignore t)
  "bcc"    '(org-table-create                                   :which-key "create"           )
  "bce"    '(org-table-create-with-table.el                     :which-key "create-el"        )
  "bcv"    '(org-table-create-or-convert-from-region            :which-key "convert"          )
  "bC"     '(org-table-toggle-coordinate-overlays               :which-key "coordinates"      )
  "bh"     '(org-table-hline-and-move                           :which-key "headline"         )
  "bi"     '(                                                   :which-key "insert"           :ignore t)
  "bic"    '(org-table-insert-column                            :which-key "col"              )
  "bir"    '(org-table-insert-row                               :which-key "row"              )
  "bI"     '(org-table-field-info                               :which-key "info"             )
  "bm"     '(org-table-sum                                      :which-key "sum"              )
  "br"     '(                                                   :which-key "recalc"           :ignore t)
  "brb"    '(org-table-recalculate-buffer-tables                :which-key "buffer"           )
  "brr"    '(org-table-recalculate                             :which-key "table"             )
  "bR"     '(org-table-paste-rectangle                          :which-key "rectangle"        )
  "bs"     '(org-table-sort-lines                               :which-key "sort"             )
  "bS"     '(org-table-show-reference                           :which-key "show-ref"         )

  "B"      '(                                                   :which-key "babel"            :ignore t)
  "BS"     '(org-babel-pop-to-session                           :which-key "session"          )
  "BE"     '(org-babel-execute-buffer                           :which-key "exec-buffer"      )

  "c"      '(org-comment-dwim                                   :which-key "comment"          )

  "C"      '(                                                   :which-key "clock"            :ignore t)
  "CC"     '(org-clock-cancel                                   :which-key "cancel"           )
  "CG"     '(org-clock-goto                                     :which-key "goto"             )
  "CI"     '(org-clock-in                                       :which-key "in"               )
  "CL"     '(org-clock-in-last                                  :which-key "last"             )
  "CO"     '(org-clock-out                                      :which-key "out"              )
  "CP"     '(org-pomodoro                                       :which-key "pomodoro"         )

  "d"      '(                                                   :which-key "id-dependecy"     :ignore t)
  "dic"    '(org-id-copy                                        :which-key "id-copy"          )
  "dif"    '(org-find-entry-with-id                             :which-key "id-find-entry"    )
  "dig"    '(org-id-goto                                        :which-key "id-goto"          )
  "dic"    '(org-id-get-create                                  :which-key "id-create"        )
  "dil"    '(org-id-store-link                                  :which-key "id-store-link"    )

  "dp"     '(                                                   :which-key "predecessor"      :ignore t)
  "dpg"    '(fb/goto-predecessor                                :which-key "pre-goto"         )
  "dpi"    '(fb/insert-predecessor-relation                     :which-key "pre-insert"       )
  "dpr"    '(fb/remove-predecessor-relation                     :which-key "pre-remove"       )
  "dps"    '(fb/store-as-predecessor                            :which-key "pre-store-as"     )
  "ds"     '(                                                   :which-key "successor"        :ignore t)
  "dsg"    '(fb/goto-successor                                  :which-key "suc-goto"         )
  "dsi"    '(fb/insert-successor-relation                       :which-key "suc-insert"       )
  "dsr"    '(fb/remove-successor-relation                       :which-key "suc-remove"       )
  "dss"    '(fb/store-as-successor                              :which-key "suc-store-as"     )

  "D"      '(                                                   :which-key "drill"            :ignore t)
  "DA"     '(org-drill-again                                    :which-key "again"            )
  "DC"     '(org-drill-cram                                     :which-key "cram"             )
  "DD"     '(org-drill                                          :which-key "drill"            )
  "DI"     '(org-drill-relearn-item                             :which-key "relear-item"      )
  "DL"     '(org-drill-leitner                                  :which-key "leitner"          )
  "DR"     '(org-drill-resume                                   :which-key "resume"           )
  "DT"     '(org-drill-tree                                     :which-key "tree"             )
  "DM "    '(                                                   :which-key "meta"             :ignore t)
  "DMM"    '(org-drill-merge-buffers                            :which-key "merge-buffer"     )
  "DMS"    '(org-drill-strip-all-data                           :which-key "strip-data"       )

  "l"      '(                                                   :which-key "link"             )
  "li"     '(org-insert-last-stored-link                        :which-key "insert last link" )
  "ll"     '(org-insert-link                                    :which-key "insert link"      )
  "ls"     '(org-store-link                                     :which-key "store link"       )
  "lt"     '(org-toggle-link-display                            :which-key "toggle display"   )

  "L"      '(lsp-org                                            :which-key "lsp-org"          )

  "n"      '(org-add-note                                       :which-key "node"             )

  "o"      '(org-open-at-point                                  :which-key "C-c C-o"          )
  "O"      '(                                                   :which-key "toggle"           :ignore t)
  "OI"     '(org-toggle-inline-images                           :which-key "images"           )

  "p"      '(org-set-property                                   :which-key "property"         )
  "S"      '(org-insert-structure-template 'elisp               :which-key "struc-temp"       )

  "s"      '(                                                   :which-key "subtree"          :ignore t)
  "sn"     '(org-narrow-to-subtree                              :which-key "narrow"           )
  "so"     '(org-sort                                           :which-key "sort"             )
  "sw"     '(widen                                              :which-key "widen"            )

  "r"      '(fb/org-refile-hydra-grouped/body                   :which-key "refile"           )

  "R"      '(                                                   :which-key "reveal"           :ignore t)
  "RF"     '(fb/org-re-reveal/fragments                         :which-key "fragments"        )
  "RC"     '((lambda()(interactive)(fb/org-re-reveal/extra-attribut "class=''")) :which-key "class" )
  "RH"     '(fb/org-re-reveal/add-html                          :which-key "html"             )
  "RE"     '(fb/org-re-reveal/insert-end-of-talk                :which-key "endOfTalk"        )
  "RB"     '(fb/org-re-reveal/insert-line-break                 :which-key "lineBreak"        )
  "RS"     '(fb/org-re-reveal/insert-slide-split                :which-key "split"            )

  "t"      '(org-todo                                           :which-key "todo"             )

  "T"      '(                                                   :which-key "time"             :ignore t)
  "TC"     '(                                                   :which-key "check"            :ignore t)
  "TCA"    '(org-check-after-date                               :which-key "check-after"      )
  "TCB"    '(org-check-before-date                              :which-key "check-before"     )
  "TCC"    '(org-goto-calendar                                  :which-key "calendar"         )
  "TCD"    '(org-check-deadlines                                :which-key "check-deadline"   )
  "TD"     '(org-time-stamp                                     :which-key "date"             )
  "TE"     '(org-evaluate-time-range                            :which-key "evaluate"         )
  "TF"     '(org-date-from-calendar                             :which-key "date from cal"    )
  "TV"     '((lambda()(interactive)(org-evaluate-time-range 0)) :which-key "evaluate+ins"     )
  "TI"     '(org-time-stamp-inactive                            :which-key "inact"            )
  "TO"     '((lambda()(interactive)(org-time-stamp-inactive 0)) :which-key "inact+time"       )
  "TL"     '(org-deadline                                       :which-key "deadline"         )
  "TS"     '(org-schedule                                       :which-key "schedule"         )
  "TT"     '((lambda()(interactive)(org-time-stamp 0))          :which-key "date+time"        )

  "x"      '(                                                   :which-key "text"             :ignore t)
  "xb"     '((lambda () (interactive)(org-emphasize ?\*))       :which-key "bold"             )
  "xc"     '((lambda () (interactive)(org-emphasize ?\~))       :which-key "code"             )
  "xi"     '((lambda () (interactive)(org-emphasize ?\/))       :which-key "italic"           )
  "xr"     '((lambda () (interactive)(org-emphasize ?\ ))       :which-key "clear"            )
  "xR"     '((lambda () (interactive)(org-emphasize ?\s))       :which-key "clear"            )
  "xs"     '((lambda () (interactive)(org-emphasize ?\+))       :which-key "strike-through"   )
  "xu"     '((lambda () (interactive)(org-emphasize ?\_))       :which-key "underline"        )
  "xv"     '((lambda () (interactive)(org-emphasize ?\=))       :which-key "verbatim"         )
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; python-keybindings
;;;;
;;

(fb/local-leader-key
  :keymaps 'python-mode-map
  :states  '(normal visual insert)

  ;;;; prefix-r is not as ergonomical as c
  "c"      '(                                                   :which-key "run"              :ignore t)
  "co"     '(run-python                                         :which-key "open"             )
  "cb"     '(python-shell-send-buffer                           :which-key "buffer"           )
  "cd"     '(python-shell-send-defun                            :which-key "defun"            )
  "cf"     '(python-shell-send-file                             :which-key "file"             )
  "cm"     '(python-shell-send-statement                        :which-key "statement"        )
  "cs"     '(python-shell-switch-to-shell                       :which-key "switch"           )
  "cr"     '(python-shell-send-region                           :which-key "region"           )
  "ct"     '(python-shell-send-string                           :which-key "string"           )

  "i"      '(                                                   :which-key "import/indent"    :ignore t)
  "ii"     '(python-skeleton-import                             :which-key "import"           )
  "il"     '(python-indent-shift-left                           :which-key "shift-left"       )
  "ir"     '(python-indent-shift-right                          :which-key "shift-right"      )
  "id"     '(python-indent-dedent-line                          :which-key "in/de~line"       )
  "ib"     '(python-indent-dedent-line-backspace                :which-key "backspace"        )

  "n"      '(                                                   :which-key "nav"              :ignore t)
  "nl"     '(python-nav-backward-up-list                        :which-key "nav-list"         )
  "nb"     '(python-nav-backward-block                          :which-key "backward-block"   )
  "nf"     '(python-nav-forward-block                           :which-key "forward-block"    )

  "s"      '(                                                   :which-key "sceleton"         :ignore t)
  "sc"     '(python-skeleton-class                              :which-key "class"            )
  "sd"     '(python-skeleton-def                                :which-key "def"              )
  "sf"     '(python-skeleton-for                                :which-key "for"              )
  "si"     '(python-skeleton-if                                 :which-key "if"               )
  "sm"     '(python-skeleton-import                             :which-key "import"           )
  "st"     '(python-skeleton-try                                :which-key "try"              )
  "sw"     '(python-skeleton-while                              :which-key "while"            )

  "t"      '(python-pytest-dispatch                             :which-key "test"             )

  ;; ""      '(completion-at-point                            :which-key ""                 )
  ;; ""      '(imenu                                          :which-key ""                 )
  ;; ""      '(prog-indent-sexp                               :which-key ""                 )
  ;; ""      '(python-check                                   :which-key ""                 )
  ;; ""      '(python-describe-at-point                       :which-key ""                 )
  ;; ""      '(python-eldoc-at-point                          :which-key ""                 )
  ;; ""      '(python-mark-defun                              :which-key ""                 )
  )

(fb/local-leader-key
  :keymaps 'inferior-python-mode-map
  :states  '(normal visual insert)

  "c"      '(comint-clear-buffer                                :which-key "clear"            )

  "pn"     '(comint-next-prompt                                 :which-key "next"             )
  "pp"     '(comint-previous-prompt                             :which-key "previous"         )

  ;; ""      '(backward-kill-word                                 :which-key ""        )
  ;; ""      '(comint-accumulate                                  :which-key ""        )
  ;; ""      '(comint-bol-or-process-mark                         :which-key ""        )
  ;; ""      '(comint-copy-old-input                              :which-key ""        )
  ;; ""      '(comint-delchar-or-maybe-eof                        :which-key ""        )
  ;; ""      '(comint-delete-output                               :which-key ""        )
  ;; ""      '(comint-dynamic-list-input-ring                     :which-key ""        )
  ;; ""      '(comint-get-next-from-history                       :which-key ""        )
  ;; ""      '(comint-history-isearch-backward-regexp             :which-key ""        )
  ;; ""      '(comint-insert-input                                :which-key ""        )
  ;; ""      '(comint-insert-previous-argument                    :which-key ""        )
  ;; ""      '(comint-interrupt-subjob                            :which-key ""        )
  ;; ""      '(comint-kill-input                                  :which-key ""        )
  ;; ""      '(comint-next-input                                  :which-key ""        )
  ;; ""      '(comint-next-matching-input-from-input              :which-key ""        )
  ;; ""      '(comint-previous-input                              :which-key ""        )
  ;; ""      '(comint-previous-matching-input-from-input          :which-key ""        )
  ;; ""      '(comint-quit-subjob                                 :which-key ""        )
  ;; ""      '(comint-send-eof                                    :which-key ""        )
  ;; ""      '(comint-send-input                                  :which-key ""        )
  ;; ""      '(comint-show-maximum-output                         :which-key ""        )
  ;; ""      '(comint-show-output                                 :which-key ""        )
  ;; ""      '(comint-stop-subjob                                 :which-key ""        )
  ;; ""      '(comint-write-output                                :which-key ""        )
  ;; ""      '(delete-forward-char                                :which-key ""        )
  ;; ""      '(evil-scroll-down                                   :which-key ""        )
  ;; ""      '(python-shell-completion-complete-or-indent         :which-key ""        )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; web-keybindings
;;;;
;;

(fb/local-leader-key
  :keymaps 'web-mode-map
  :states  '(normal visual insert)


  "a"      '(                                                   :which-key "attribute"              :ignore t)
  "ab"     '(web-mode-attribute-beginning                       :which-key "beginning"              )
  "ae"     '(web-mode-attribute-end                             :which-key "end"                    )
  "ai"     '(web-mode-attribute-insert                          :which-key "insert"                 )
  "ak"     '(web-mode-attribute-kill                            :which-key "kill"                   )
  "an"     '(web-mode-attribute-next                            :which-key "next"                   )
  "ap"     '(web-mode-attribute-previous                        :which-key "previous"               )
  "as"     '(web-mode-attribute-select                          :which-key "select"                 )
  "at"     '(web-mode-attribute-transpose                       :which-key "transpose"              )

  "b"      '(                                                   :which-key "block"                  :ignore t)
  "bb"     '(web-mode-block-beginning                           :which-key "beginning"              )
  "bc"     '(web-mode-block-close                               :which-key "close"                  )
  "be"     '(web-mode-block-end                                 :which-key "end"                    )
  "bk"     '(web-mode-block-kill                                :which-key "kill"                   )
  "bn"     '(web-mode-block-next                                :which-key "next"                   )
  "bp"     '(web-mode-block-previous                            :which-key "previous"               )
  "bs"     '(web-mode-block-select                              :which-key "select"                 )

  "c"      '(web-mode-comment-or-uncomment                      :which-key "{un}comment"            )

  "d"      '(                                                   :which-key "dom"                    :ignore t)
  "da"     '(web-mode-dom-apostrophes-replace                   :which-key "apostrophes-replace"    )
  "dd"     '(web-mode-dom-errors-show                           :which-key "errors-show"            )
  "de"     '(web-mode-dom-entities-replace                      :which-key "entities-replace"       )
  "dn"     '(web-mode-dom-normalize                             :which-key "normalize"              )
  "dq"     '(web-mode-dom-quotes-replace                        :which-key "quotes-replace"         )
  "dt"     '(web-mode-dom-traverse                              :which-key "traverse"               )
  "dx"     '(web-mode-dom-xpath                                 :which-key "xpath"                  )

  "e"      '(                                                   :which-key "element"                :ignore t)
  "e+"     '(web-mode-element-extract                           :which-key "extract"                )
  "e-"     '(web-mode-element-contract                          :which-key "contract"               )
  "e/"     '(web-mode-element-close                             :which-key "close"                  )
  "eI"     '(web-mode-element-insert-at-point                   :which-key "insert-at-point"        )
  "ea"     '(web-mode-element-content-select                    :which-key "content-select"         )
  "eb"     '(web-mode-element-beginning                         :which-key "beginning"              )
  "ec"     '(web-mode-element-clone                             :which-key "clone"                  )
  "ed"     '(web-mode-element-child                             :which-key "child"                  )
  "ee"     '(web-mode-element-end                               :which-key "end"                    )
  "ef"     '(web-mode-element-children-fold-or-unfold           :which-key "children-fold-or-unfold")
  "ei"     '(web-mode-element-insert                            :which-key "insert"                 )
  "ek"     '(web-mode-element-kill                              :which-key "kill"                   )
  "em"     '(web-mode-element-mute-blanks                       :which-key "mute-blanks"            )
  "en"     '(web-mode-element-next                              :which-key "next"                   )
  "ep"     '(web-mode-element-previous                          :which-key "previous"               )
  "er"     '(web-mode-element-rename                            :which-key "rename"                 )
  "es"     '(web-mode-element-select                            :which-key "select"                 )
  "et"     '(web-mode-element-transpose                         :which-key "transpose"              )
  "eu"     '(web-mode-element-parent                            :which-key "parent"                 )
  "ev"     '(web-mode-element-vanish                            :which-key "vanish"                 )
  "ew"     '(web-mode-element-wrap                              :which-key "wrap"                   )

  "f"      '(web-mode-fold-or-unfold                            :which-key "fold-or-unfold"         )

  "h"      '(web-mode-buffer-fontify                            :which-key "buffer-fontify"         )

  "i"      '(prog-indent-sexp                                   :which-key "prog-indent-sexp"       )

  "j"      '(web-mode-jshint                                    :which-key "jshint"                 )

  "l"      '(web-mode-file-link                                 :which-key "file-link"              )

  "n"      '(web-mode-navigate                                  :which-key "navigate"               )

  "p"      '(sgml-pretty-print                                  :which-key "pretty"                 )

  "r"      '(web-mode-reload                                    :which-key "reload"                 )

  "s"      '(web-mode-snippet-insert                            :which-key "snippet-insert"         )

  "t"      '(                                                   :which-key "tag"                    :ignore t)
  "ta"     '(web-mode-tag-attributes-sort                       :which-key "attributes-sort"        )
  "tb"     '(web-mode-tag-beginning                             :which-key "beginning"              )
  "te"     '(web-mode-tag-end                                   :which-key "end"                    )
  "tm"     '(web-mode-tag-match                                 :which-key "match"                  )
  "tn"     '(web-mode-tag-next                                  :which-key "next"                   )
  "tp"     '(web-mode-tag-previous                              :which-key "previous"               )
  "ts"     '(web-mode-tag-select                                :which-key "select"                 )

  "w"      '(web-mode-whitespaces-show                          :which-key "whitespaces-show"       )

  "RET"    '(web-mode-mark-and-expand                           :which-key "mark-expand"            )
  "TAB"    '(web-mode-buffer-indent                             :which-key "indent"                 )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; all non-lisp-code after this message shouldn't be there
;;;; find the org-babel-block in the sources and add a :tangle no