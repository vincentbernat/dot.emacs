;; Add a "latest commits" section
(magit-add-section-hook 'magit-status-sections-hook
                        'magit-insert-recent-commits
                        nil t)
(magit-add-section-hook 'magit-status-sections-hook
                        'magit-insert-unpushed-to-upstream
                        'magit-insert-unpushed-to-upstream-or-recent t)
(remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)

(remove-hook 'git-commit-finish-query-functions
             'git-commit-check-style-conventions)

;; Remove unneeded prompts
(add-to-list 'magit-no-confirm 'stage-all-changes)

(setq magit-hide-campaign-header t)
