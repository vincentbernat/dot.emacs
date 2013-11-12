(defun vbe:revert-buffer-keep-history (&rest -)
  "Revert buffer but keep undo history"
  (interactive)

  (clear-visited-file-modtime)
  (widen)
  (let ((inhibit-read-only t)
        (current-point (point)))
    (delete-region (point-min) (point-max))
    (insert-file-contents (buffer-file-name))
    (not-modified)
    (goto-char (current-position))
    (set-visited-file-modtime)))

(setq revert-buffer-function 'vbe:revert-buffer-keep-history)
(global-auto-revert-mode 1)   ; Auto revert (when no pending changes)

;; Backups and auto saves
(let ((tmp (vbe:run-directory "saves")))
  (setq make-backup-files nil     ; Don't make backups, not used in ages
        auto-save-list-file-prefix tmp  ; Autosave in a dedicated directory
        auto-save-file-name-transforms
        `((".*" ,tmp t))))

