;;; Code:

(global-auto-revert-mode 1)   ; Auto revert (when no pending changes)
(setq create-lockfiles nil)     ; Don't create lock files

;; Backups and auto saves
(let ((tmp (concat (vbe:run-directory "saves") "/saves-")))
  (setq make-backup-files nil     ; Don't make backups, not used in ages
        auto-save-list-file-prefix tmp  ; Autosave in a dedicated directory
        auto-save-file-name-transforms
        `((".*" ,tmp t))))

;; Let emacs open files with line and column number in it
;; See: http://stackoverflow.com/questions/3139970/open-a-file-at-line-with-filenameline-syntax
(defadvice find-file (around vbe:find-file-line-number
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

(defadvice server-visit-files (before vbe:parse-numbers-in-lines
                                      (files proc &optional nowait)
                                      activate)
  "Looks for filenames like file:line or file:line:position and
reparses name in such manner that position in file"
  (save-match-data
    (ad-set-arg 0
                (mapcar (lambda (fn)
                          (let ((path (car fn)))
                            (if (string-match "^\\(.*?\\):\\([0-9]+\\):?\\([0-9]*\\):?$" path)
                              (cons
                               (match-string 1 path)
                               (cons (string-to-number (match-string 2 path))
                                     (string-to-number (or (match-string 3 path) "")))
                               )
                              fn))) files))))

(defun vbe:sudo-save ()
  "Save a file as root."
  (interactive)
  (if (not buffer-file-name)
      (write-file (concat "/sudo:root@localhost:" (ido-read-file-name "File:")))
    (write-file (concat "/sudo:root@localhost:" buffer-file-name))))

;;; files.conf.el ends here
