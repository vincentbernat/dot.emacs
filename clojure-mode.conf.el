;; For some reason, `clojure-test-maybe-enable' is autoloaded and add
;; a silly hook to clojure-mode triggering an error. Remove this hook.
(remove-hook 'clojure-mode-hook 'clojure-test-maybe-enable)
