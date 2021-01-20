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

(defun fb*literate-recompile-h ()
  "Recompile literate config to `user-emacs-directory'"
  (display-message-or-buffer "recompiling emacs config")
  (fb*literate-tangle-h)
  )

(defun fb*reload-config ()
  "reload ~/.emacs.d/init.el"
  (load-file "~/.emacs.d/init.el"))

;; (setq debug-on-error t)
;; (setq debug-ignored-error t)
;;;; nixos-packages

(defun fb*getPathToConfigFile (filename)
    "Returns concatenation of \"HOME\" , \".emacs.d/\" and the passed \"filename\"."
    (expand-file-name filename (expand-file-name ".emacs.d" (getenv "HOME")))

)

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

;;(fb*loadConfigFile "evil/evil.el")

(fb*loadConfigFile "global/0-global.el")

(fb*loadConfigFile "languages/0-languages.el")
(fb*loadConfigFile "modeline/doom-modeline.el")
(fb*loadConfigFile "orgmode/0-orgmode.el")
(fb*loadConfigFile "outline/0-outline.el")
(fb*loadConfigFile "project/0-project.el")
(fb*loadConfigFile "tex/auctex.el")
(fb*loadConfigFile "keys/0-keys.el")

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
