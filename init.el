;;; emacs-init

;;;; functions, global

(defun fb/getPathToConfigFile (filename)
       "Returns concatenation of \"HOME\" , \".emacs.d/\" and the passed \"filename\"."
       (expand-file-name filename (expand-file-name ".emacs.d" (getenv "HOME")))
       )

(defun fb/loadConfigFile (configFileName)
       "Load the config-file associated with the passed configFileName if it exists."
       (let ((pathToConfigFile (fb/getPathToConfigFile configFileName)))
         (if (file-readable-p pathToConfigFile) (load pathToConfigFile) (message "WARNING: CONFIG-FILE NOT FOUND: %s" pathToConfigFile))
         )
       )


;;;; load configs

(fb/loadConfigFile "misc/misc.el")
(fb/loadConfigFile "elisp/elisp.el")
(fb/loadConfigFile "orgmode/0-orgmode.el")
(fb/loadConfigFile "outline/0-outline.el")
