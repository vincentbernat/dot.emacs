;; Add a "latest commits" section
(magit-add-section-hook 'magit-status-sections-hook
                        'magit-insert-recent-commits
                        'magit-insert-unpushed-commits t)
