;;; emacs-init


;;;; nixos-packages

(require 'package)  ;; package-loading

(setq package-archives nil)  ;; optional. makes unpure packages archives unavailable

(setq package-enable-at-startup nil)
;; (package-initialize 'no-activate)
(package-initialize)
(eval-when-compile
  (require 'use-package))


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

;; (fb/loadConfigFile "auctex/auctex.el")
(fb/loadConfigFile "completion/0-completion.el")
(fb/loadConfigFile "elisp/elisp.el")
(fb/loadConfigFile "global/0-global.el")
(fb/loadConfigFile "keys/0-keys.el")
(fb/loadConfigFile "modeline/doom-modeline.el")
(fb/loadConfigFile "orgmode/0-orgmode.el")
(fb/loadConfigFile "outline/0-outline.el")
(fb/loadConfigFile "project/0-project.el")
(fb/loadConfigFile "themes/themes.el")
