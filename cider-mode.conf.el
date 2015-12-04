;;; Code:

;; Enable el-doc
(add-hook 'cider-mode-hook #'cider-turn-on-eldoc-mode)

;; Enable paredit on REPL
(add-hook 'cider-repl-mode-hook #'paredit-mode)

;; Change the spinner type to stay at constant-width
(setq cider-eval-spinner-type 'vertical-breathing)

;;; cider-mode.conf.el ends here
