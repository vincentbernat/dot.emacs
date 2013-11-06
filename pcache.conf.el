;; Delete old bogus pcache directory if empty
(let ((dirs (list pcache-directory (expand-file-name ".." pcache-directory))))
  (while dirs
    (let ((d (pop dirs)))
      (when (and (file-accessible-directory-p d)
                 (eq 2 (length (directory-files d))))
        (delete-directory d)))))
(setq pcache-directory (vbe:run-directory "pcache"))
