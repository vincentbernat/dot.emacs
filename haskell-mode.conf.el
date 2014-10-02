;;; Code:
;; haskell-mode configuration

(add-hook 'haskell-mode-hook
          'haskell-indentation-mode)
(add-hook 'haskell-mode-hook
          'interactive-haskell-mode)
(setq haskell-program-name "cabal repl")

;;; haskell-mode.conf.el ends here
