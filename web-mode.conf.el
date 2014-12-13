;;; Code:

(setq web-mode-engines-alist
      '(("django" . "\\.j2\\'")))

;; For JS, use a default indentation of 2
(defun vbe:web-mode-hook ()
  ;; TODO: do that only for JS?
  (setq web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook 'vbe:web-mode-hook)

;;; web-mode.conf.el ends here
