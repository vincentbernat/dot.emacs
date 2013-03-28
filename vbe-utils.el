;; The main way to load a file is to put functionalities depending on
;; some other file into a file `somelibrary.conf.el` which will be
;; loaded when `somelibrary` is loaded. The system is a bit smart and
;; if the library has hiphens in its name, it will also search into
;; subdirectories. This feature is inspired from Julien Danjou's emacs
;; configuration.
(defun vbe:after-load (file)
  "Execute appropriate hook after loading FILE.
The hooks are looked in FILE.conf.el in user emacs directory or
in a subdirectory if we can find the appropriate file by
substituting hyphens for slashes."
  ;; We got an absolute filename. Let's find the basename.
  (let* ((filename (file-name-nondirectory file))
	 (name (substring filename 0
			  (string-match "\\.elc?\\>" filename)))
	 (components (split-string name "-"))
	 (directory user-emacs-directory))

    ;; Do we have files for this?
    (while components
      (let ((target (expand-file-name
		     (format "%s.conf.el"
			     (mapconcat 'identity components "-")) directory)))
	(when (file-readable-p target)
	  (load target))
	(setq directory (expand-file-name (car components) directory)
	      components (cdr components))))))

;; Load current features
(mapc '(lambda (f) (vbe:after-load (symbol-name f))) features)
;; Load future features
(add-hook 'after-load-functions 'vbe:after-load)

(defun vbe:run-directory (name)
  "Return a directory for runtime files. Create it if it does not exist."
  (let ((dir (expand-file-name (format "run/%s" name)
			       user-emacs-directory)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

(defun vbe:at (where)
  "Return `t' if the current profile is WHERE."
  (string= (cond ((string-match (concat (regexp-quote ".p.fti.net") "$")
			   (system-name))
		  "orange")
                 ((string-match (concat (regexp-quote ".corp.dailymotion.com") "$")
                                (system-name))
                  "dailymotion")
		 (t "unknown"))
	   (cond ((symbolp where) (symbol-name where))
		 ((stringp where) where)
		 (t ""))))
