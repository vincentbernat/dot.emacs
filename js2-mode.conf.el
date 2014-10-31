;;; Code:

;; Use lambda instead of anonymous function
(prettify-symbols-mode 1)
(push '("function" . ?ƒ) prettify-symbols-alist)
(push '("return" . ?←) prettify-symbols-alist)

(setq-default js2-basic-offset 2)
(setq js2-skip-preprocessor-directives t)

;; Let flycheck does this
(setq-default js2-show-parse-errors nil)
(setq-default js2-strict-missing-semi-warning nil)

;;; js2-mode.conf.el ends here
