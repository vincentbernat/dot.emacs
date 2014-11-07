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

;; Let emacs open files with line and column number in it
;; See: http://stackoverflow.com/questions/3139970/open-a-file-at-line-with-filenameline-syntax
(defadvice find-file (around find-file-line-number
                             (path &optional wildcards)
                             activate)
  "Turn files like file.js:14:10 into file.js and going to line 14, col 10."
  (save-match-data
    (let* ((match (string-match "^\\(.*?\\):\\([0-9]+\\):?\\([0-9]*\\):?$" path))
           (line-no (and match
                         (match-string 2 path)
                         (string-to-number (match-string 2 path))))
           (col-no (and match
                        (match-string 3 path)
                        (string-to-number (match-string 3 path))))
           (path (if match (match-string 1 path) path)))
      ad-do-it
      (when line-no
        ;; goto-line is for interactive use
        (goto-char (point-min))
        (forward-line (1- line-no))
        (when (> col-no 0)
          (forward-char (1- col-no)))))))

;;; files.conf.el ends here
