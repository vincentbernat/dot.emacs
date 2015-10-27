;;; Code:

(diminish 'paredit-mode "⦅⦆")

(with-eval-after-load "eldoc"
  (eldoc-add-command
   'paredit-backward-delete
   'paredit-close-round))

;;; paredit.conf.el ends here
