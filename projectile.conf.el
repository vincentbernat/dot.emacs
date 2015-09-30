(let ((dir (vbe:run-directory "projectile")))
  (setq projectile-known-projects-file (expand-file-name "bookmarks.eld" dir)
        projectile-cache-file (expand-file-name "projectile.cache" dir)))

;; Detect SVN projects
(add-to-list 'projectile-project-root-files ".svn")
(add-to-list 'projectile-globally-ignored-directories ".svn")

;; Use ivy
(setq projectile-completion-system 'ivy)

(diminish 'projectile-mode)
