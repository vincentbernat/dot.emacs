(add-to-list 'ac-dictionary-directories (vbe:run-directory "ac-dict"))
(setq ac-comphist-file (expand-file-name "run/ac-comphist.dat"
                                         user-emacs-directory))
(require 'auto-complete-config)
(ac-config-default)

;; Don't use up/down arrow (use M-n, M-p only) to browse list
(define-key ac-completing-map [down] nil)
(define-key ac-completing-map [up] nil)
