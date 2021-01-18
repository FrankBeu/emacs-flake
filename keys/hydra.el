;;; hydra

(use-package hydra)


;;;; scale-text

(defhydra hydra-text-scale (:timeout 5)
  "scale text"
  ("k" text-scale-increase "in")
  ("l" text-scale-decrease "out")
  ("q" nil "quit" :exit t))

(fb/leader-key
  "ts" '(hydra-text-scale/body :which-key "scale text"))


;;;; evil-numbers

(defhydra hydra-evil-numbers (:timeout 5)
  "evil-numbers"
  ("k" evil-numbers/inc-at-pt "+")
  ("l" evil-numbers/dec-at-pt "-")
  ("q" nil "quit" :exit t))

(fb/leader-key
  "n." '(hydra-evil-numbers/body :which-key "transient"))
