;;; Code:

;; Enable el-doc
(add-hook 'cider-mode-hook #'cider-turn-on-eldoc-mode)

;; Enable paredit on REPL
(add-hook 'cider-repl-mode-hook #'paredit-mode)


(setq
 ;; Change the spinner type to stay at constant-width
 cider-eval-spinner-type 'vertical-breathing
 ;; Don't font-lock too much
 cider-font-lock-dynamically '(macro function deprecated))

;;; cider-mode.conf.el ends here
