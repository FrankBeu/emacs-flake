;;; hydra

(use-package hydra)


(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  ("k" text-scale-increase "in")
  ("l" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(fb/leader-key-SPC
  "ts" '(hydra-text-scale/body :which-key "scale text"))
