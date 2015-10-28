;;; Code:

(diminish 'paredit-mode "⦅⦆")

(define-key paredit-mode-map (kbd "M-k") 'paredit-copy-as-kill)

(with-eval-after-load "eldoc"
  (eldoc-add-command
   'paredit-backward-delete
   'paredit-close-round))

;;; paredit.conf.el ends here
