;; When two buffers share the same name, use the directory instead of
;; appending things like <1>
(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t) ; rename after killing
(setq uniquify-ignore-buffers-re "^\\*")

;; Use ido
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Remember last position in opened files
(require 'saveplace)
(setq-default save-place t)

(provide 'vbe/buffers)
