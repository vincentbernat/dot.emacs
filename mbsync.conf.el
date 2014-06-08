;; Integration with gnus (we assume gnus is loaded)
(add-hook 'mbsync-exit-hook 'gnus-group-get-new-news)

;; From group, trigger with f
(define-key gnus-group-mode-map (kbd "f") 'mbsync)
