;;; Code:

(setq
 ;; Use a dot file to avoid being detected by some watchers
 flycheck-temp-prefix ".flycheck"
 ;; Do not hijack next-error
 flycheck-standard-error-navigation nil
 ;; Don't enable flycheck on some modes
 flycheck-global-modes '(not erc-mode))

;; Go
(setq
 ;; Speedup compilation by saving intermediate files.
 flycheck-go-build-install-deps t)

;;;
