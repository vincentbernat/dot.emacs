;;; Code:

(diminish 'paredit-mode "⦅⦆")

(define-key paredit-mode-map (kbd "M-k") 'paredit-copy-as-kill)

(with-eval-after-load "eldoc"
  (eldoc-add-command
   'paredit-backward-delete
   'paredit-close-round))

(defadvice he-substitute-string (after vbe:he-paredit-fix activate)
  "Remove extra paren when expanding line in paredit"
  ;; Is ")" enough?
  (if (and paredit-mode (equal (substring str -1) ")"))
      (progn (backward-delete-char 1) (forward-char))))

;;; paredit.conf.el ends here
