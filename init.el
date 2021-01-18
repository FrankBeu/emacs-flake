;;; emacs-init

;; + `doom/abc` A public, interactive command, designed to be used via `M-x` or a
  ;; keybinding.
;; + `doom:abc` A public evil operator, motion or command.
;; + `doom|abc` A public, non-interactive function meant to be used as a hook.
;; + `doom*abc` Functions designed to be used as advice for other functions.
;; + `abc!` A public Doom "autodef" function or macro. An autodef should always
  ;; be defined, even if its containing module is disabled (i.e. they will not throw a
  ;; void-function error). The purpose of this is to avoid peppering module configs
  ;; with conditionals or `after!` blocks before using their APIs. They should
  ;; noop if their module is disabled, and should be zero-cost in the case their
  ;; module is disabled.

  ;; Autodefs usually serve to configure Doom or a module. [and are usually syntactic sugar]
;; + Functions prefixed with `+abc...` belong to a module, e.g.
  ;; `+emacs-lisp|init-hook` is a hook function in the `lang/emacs-lisp` module.
;; + `=abc` An interactive command that invokes an app module.

(require 'package)  ;; package-loading

(setq package-archives nil)  ;; optional. makes unpure packages archives unavailable

(setq package-enable-at-startup nil)
;; (package-initialize 'no-activate)
(package-initialize)
(eval-when-compile
  (require 'use-package))

;;;; debugging
;; (setq debug-on-error t)
;; (setq debug-ignored-error t)
;;;; nixos-packages


;;;; functions: loadConfig

(defun fb/getPathToConfigFile (filename)
  "Returns concatenation of \"HOME\" , \".emacs.d/\" and the passed \"filename\"."
  (expand-file-name filename (expand-file-name ".emacs.d" (getenv "HOME")))
  )

(defun fb/loadConfigFile (configFileName)
  "Load the config-file associated with the passed configFileName if it exists."
  (let ((pathToConfigFile (fb/getPathToConfigFile configFileName)))
    (if (file-readable-p pathToConfigFile) (load pathToConfigFile) (message "WARNING: CONFIG-FILE NOT FOUND: %s" pathToConfigFile))
    ))


;;;; load configs

(fb/loadConfigFile "completion/0-completion.el")
(fb/loadConfigFile "evil/evil.el")
(fb/loadConfigFile "global/0-global.el")
(fb/loadConfigFile "keys/0-keys.el")
(fb/loadConfigFile "languages/0-languages.el")
(fb/loadConfigFile "modeline/doom-modeline.el")
(fb/loadConfigFile "orgmode/0-orgmode.el")
(fb/loadConfigFile "outline/0-outline.el")
(fb/loadConfigFile "project/0-project.el")
(fb/loadConfigFile "tex/auctex.el")
(fb/loadConfigFile "themes/themes.el")



(fb/loadConfigFile "keys/0-keys.el")
