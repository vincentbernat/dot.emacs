;;; Code:

(let ((dir (vbe:run-directory "projectile")))
  (setq projectile-known-projects-file (expand-file-name "bookmarks.eld" dir)
        projectile-cache-file (expand-file-name "projectile.cache" dir)))

(setq projectile-completion-system 'ivy)

(diminish 'projectile-mode)

(defun vbe:set-compilation-save-buffer-predicate ()
  (setq-local compilation-save-buffers-predicate
              '(lambda ()
                 (string-prefix-p
                  projectile-current-root
                  (file-truename (buffer-file-name))))))
(add-hook 'projectile-mode-hook 'vbe:set-compilation-save-buffer-predicate nil t)

;;;
