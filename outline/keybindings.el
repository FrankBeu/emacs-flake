;;; outline-keybindings

;;;; outline-minor-mode key map
(define-prefix-command 'cm-map nil "Outline-")

;;;; HIDE
(define-key cm-map "\M-q" 'outline-hide-sublevels)     ;;; Hide everything but the top-level headings
(define-key cm-map "\M-t" 'outline-hide-body)          ;;; Hide everything but headings (all body lines)
(define-key cm-map "\M-o" 'outline-hide-other)         ;;; Hide other branches
(define-key cm-map "\M-c" 'outline-hide-entry)         ;;; Hide this entry's body
(define-key cm-map "\M-l" 'outline-hide-leaves)        ;;; Hide body lines in this entry and sub-entries
(define-key cm-map "\M-d" 'outline-hide-subtree)       ;;; Hide everything in this entry and sub-entries

;;;; SHOW
(define-key cm-map "\M-a" 'outline-show-all)           ;;; Show (expand) everything
(define-key cm-map "\M-e" 'outline-show-entry) ;;; Show this heading's body
(define-key cm-map "\M-i" 'outline-show-children)      ;;; Show this heading's immediate child sub-headings
(define-key cm-map "\M-k" 'outline-show-branches)      ;;; Show all sub-headings under this heading
(define-key cm-map "\M-s" 'outline-show-subtree)       ;;; Show (expand) everything in this heading & below

;;;; MOVE
(define-key cm-map "\M-u" 'outline-up-heading)                ;;; Up
(define-key cm-map "\M-n" 'outline-next-visible-heading)      ;;; Next
(define-key cm-map "\M-p" 'outline-previous-visible-heading)  ;;; Previous
(define-key cm-map "\M-f" 'outline-forward-same-level)        ;;; Forward - same level
(define-key cm-map "\M-b" 'outline-backward-same-level)       ;;; Backward - same level
(global-set-key "\M-o" cm-map)


;;; TODO TODOS
;;;; TODO make keybindings pairs
