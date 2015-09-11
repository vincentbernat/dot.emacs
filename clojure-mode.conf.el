;;; Code:

;; For some reason, `clojure-test-maybe-enable' is autoloaded and add
;; a silly hook to clojure-mode triggering an error. Remove this hook.
(remove-hook 'clojure-mode-hook 'clojure-test-maybe-enable)

;; Use cider-mode as well
(add-hook 'clojure-mode-hook 'cider-mode)

;; Some indentation preferences
;;; Midje-related:
(put-clojure-indent 'fact 1)
(put-clojure-indent 'facts 1)

;;; clojure-mode.conf.el ends here
