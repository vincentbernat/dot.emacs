;;; Code:

;; Enable el-doc
(add-hook 'cider-mode-hook #'cider-turn-on-eldoc-mode)

;; Enable paredit on REPL
(add-hook 'cider-repl-mode-hook #'paredit-mode)

;;; cider-mode.conf.el ends here
