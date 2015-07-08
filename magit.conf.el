;; Add a "latest commits" section
(magit-add-section-hook 'magit-status-sections-hook
                        'magit-insert-recent-commits
                        'magit-insert-unpushed-commits t)

(remove-hook 'git-commit-finish-query-functions
             'git-commit-check-style-conventions)
