;;; Code:

;; Try to emulate auto revert with undo history
(when (version< emacs-version "24.4")
  (defun vbe:revert-buffer-keep-history (&rest -)
    "Revert buffer but keep undo history"
    (clear-visited-file-modtime)
    (widen)
    (let ((inhibit-read-only t)
          (current-point (point)))
      (erase-buffer)
      (insert-file-contents (buffer-file-name))
      (not-modified)
      (goto-char current-point)
      (set-visited-file-modtime)))

  (setq revert-buffer-function 'vbe:revert-buffer-keep-history))

(global-auto-revert-mode 1)   ; Auto revert (when no pending changes)

;; Backups and auto saves
(let ((tmp (concat (vbe:run-directory "saves") "/saves-")))
  (setq make-backup-files nil     ; Don't make backups, not used in ages
        auto-save-list-file-prefix tmp  ; Autosave in a dedicated directory
        auto-save-file-name-transforms
        `((".*" ,tmp t))))

