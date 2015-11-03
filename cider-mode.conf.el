;;; Code:

;; Enable el-doc
(add-hook 'cider-mode-hook #'cider-turn-on-eldoc-mode)

;; Enable paredit on REPL
(add-hook 'cider-repl-mode-hook #'paredit-mode)

;; Keybindings for clj-refactor
(cljr-add-keybindings-with-prefix "\C-cr")

;;; cider-mode.conf.el ends here
