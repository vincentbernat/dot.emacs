(let ((dir (vbe:run-directory "projectile")))
  (setq projectile-known-projects-file (expand-file-name "bookmarks.eld" dir)
        projectile-cache-file (expand-file-name "projectile.cache" dir)))
