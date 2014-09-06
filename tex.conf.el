;;; Code:
;; AUCTeX configuration

(setq latex-run-command "xelatex")
(add-to-list 'tex-compile-commands
             '("xdg-open %r.pdf &" "%r.pdf"))

;;; tex.conf.el ends here
