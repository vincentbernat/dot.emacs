(setq
 ;; Use a dot file to avoid being detected by some watchers
 flycheck-temp-prefix ".flycheck"
 ;; Don't enable flycheck on some modes
 flycheck-global-modes '(not erc-mode))
