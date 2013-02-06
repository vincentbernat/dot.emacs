(defun vbe:revert-buffer-keep-history (&rest -)
  "Revert buffer but keep undo history"
  (interactive)

  (clear-visited-file-modtime)
  (let ((inhibit-read-only t))
    (widen)
    (delete-region (point-min) (point-max))
    (insert-file-contents (buffer-file-name))
    (not-modified)
    (set-visited-file-modtime)))

(setq revert-buffer-function 'vbe:revert-buffer-keep-history)
(global-auto-revert-mode 1)   ; Auto revert (when no pending changes)
