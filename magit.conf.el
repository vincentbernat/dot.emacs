(setq magit-diff-refine-hunk nil)

;; full screen magit-status
;; Stolen from: http://whattheemacsd.com/setup-magit.el-01.html
(defadvice magit-status (around vbe:magit-fullscreen activate)
  (window-configuration-to-register :vbe/magit-fullscreen)
  ad-do-it
  (delete-other-windows))
(defun vbe:magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :vbe/magit-fullscreen))
(define-key magit-status-mode-map (kbd "q") 'vbe:magit-quit-session)

(defun vbe:magit-insert-recent-commits ()
  (magit-git-insert-section (recent "Recent commits:")
      (apply-partially 'magit-wash-log 'unique)
    "log" "--format=format:%h %s" "-n" "10"))

;; Add a "latest commits" section
(magit-add-section-hook 'magit-status-sections-hook
                        'vbe:magit-insert-recent-commits
                        'magit-insert-unpushed-commits t)
