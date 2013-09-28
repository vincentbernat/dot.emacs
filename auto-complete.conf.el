(add-to-list 'ac-dictionary-directories (vbe:run-directory "ac-dict"))
(require 'auto-complete-config)
(ac-config-default)

;; Don't use up/down arrow (use M-n, M-p only) to browse list
(define-key ac-completing-map [down] nil)
(define-key ac-completing-map [up] nil)
