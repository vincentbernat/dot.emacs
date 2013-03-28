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

;; http://stackoverflow.com/questions/6532898/is-there-a-apply-function-to-region-lines-in-emacs
(defun vbe:apply-function-to-region-lines (fn)
  (interactive "aFunction to apply to lines in region: ")
  (save-excursion
    (goto-char (region-end))
    (let ((end-marker (copy-marker (point-marker)))
          next-line-marker)
      (goto-char (region-beginning))
      (if (not (bolp))
          (forward-line 1))
      (setq next-line-marker (point-marker))
      (while (< next-line-marker end-marker)
        (let ((start nil)
              (end nil))
          (goto-char next-line-marker)
          (save-excursion
            (setq start (point))
            (forward-line 1)
            (set-marker next-line-marker (point))
            (setq end (point)))
          (save-excursion
            (let ((mark-active nil))
              (narrow-to-region start end)
              (funcall fn)
              (widen)))))
      (set-marker end-marker nil)
      (set-marker next-line-marker nil))))
