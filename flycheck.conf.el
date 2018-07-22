;;; Code:

(setq
 ;; Use a dot file to avoid being detected by some watchers
 flycheck-temp-prefix ".flycheck"
 ;; Do not hijack next-error
 flycheck-standard-error-navigation nil
 ;; Do not display anything in modeline (see spaceline)
 flycheck-mode-line nil
 ;; Don't enable flycheck on some modes
 flycheck-global-modes '(not erc-mode))

;; Go
(setq
 ;; Speedup compilation by saving intermediate files.
 flycheck-go-build-install-deps t)

;; Disable emacs-lisp-checkdoc
(add-to-list 'flycheck-disabled-checkers 'emacs-lisp-checkdoc)
