(add-to-list 'ac-dictionary-directories (vbe:run-directory "ac-dict"))
(setq ac-comphist-file (expand-file-name "run/ac-comphist.dat"
                                         user-emacs-directory))
(require 'auto-complete-config)
(require 'auto-complete-clang-async)
(ac-config-default)
(defun vbe:clang-auto-complete-mode-hook ()
  (unless (not (tramp-tramp-file-p (buffer-file-name)))
    (setq ac-sources '(ac-source-clang-async))
    (ac-clang-launch-completion-process)))
(add-hook 'c-mode-common-hook 'vbe:clang-auto-complete-mode-hook)

;; Don't use up/down arrow (use M-n, M-p only) to browse list
(define-key ac-completing-map [down] nil)
(define-key ac-completing-map [up] nil)
