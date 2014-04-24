;; Use lambda instead of anonymous function
(font-lock-add-keywords
 'js2-mode `(("\\(function\\) *\\*?("
               (0 (progn (compose-region (match-beginning 1)
                           (match-end 1) "\u0192")
                    nil)))))

;; Use right arrow for returns
(font-lock-add-keywords
 'js2-mode `(("function *([^)]*) *{ *\\(return\\) "
               (0 (progn (compose-region (match-beginning 1)
                           (match-end 1) "\u2190")
                    nil)))))

(setq-default js2-basic-offset 2)
(setq js2-skip-preprocessor-directives t)

;; Let flycheck does this
(setq-default js2-show-parse-errors nil)
(setq-default js2-strict-missing-semi-warning nil)

