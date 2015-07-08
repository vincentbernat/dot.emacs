(defun vbe:magit-insert-recent-commits ()
  (magit-git-insert-section (recent "Recent commits:")
      (apply-partially 'magit-wash-log 'unique)
    "log" "--format=format:%h %s" "-n" "10"))

;; Add a "latest commits" section
(magit-add-section-hook 'magit-status-sections-hook
                        'vbe:magit-insert-recent-commits
                        'magit-insert-unpushed-commits t)
