;;; Code:

(defun vbe:js-mode-hook ()
  (setq js-indent-level 2))
(add-hook 'js-mode-hook 'vbe:js-mode-hook)

;;; js.conf.el ends here
