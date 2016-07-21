;;; Code:

(require 'ansi-color)
(defun vbe:colorize-compilation-buffer ()
  (when (eq major-mode 'compilation-mode)
    (ansi-color-apply-on-region compilation-filter-start (point-max))))
(add-hook 'compilation-filter-hook 'vbe:colorize-compilation-buffer)

(setq compilation-ask-about-save nil)
(global-set-key (kbd "<f9>") 'recompile)

;;;
