;;; Code:

(setq elpy-modules (remove 'elpy-module-flymake
                           (remove 'elpy-module-highlight-indentation
                                   (remove 'elpy-module-yasnippet elpy-modules))))

;;; elpy.conf.el ends here
