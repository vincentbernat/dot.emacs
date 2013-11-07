;; Where is located the code?
(setq magit-repo-dirs (list (expand-file-name "~/code"))
      magit-repo-dirs-depth 2)

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
