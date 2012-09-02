;; Check we are using Emacs 24
(when (/= emacs-major-version 24)
  (error "Only Emacs 24 is supported. You seem to use Emacs %d"
	 emacs-major-version))

;; There are two ways to load some functionalities. The first one is
;; to use `(vbe:require)` which is like `(require)` but does not need
;; to alter `load-path`.
(defun vbe:require (feature)
  "Load FEATURE if not loaded (with added prefix).
The appropriate prefix is added to the provided feature but the
name is searched without prefix. For example, if FEATURE is
\"el-get\", the loaded feature will be \"vbe:el-get\" and it will
be searched in \"el-get.el\" in the user Emacs directory."
  (let* ((filename (expand-file-name (symbol-name feature)
				     user-emacs-directory))
	 (prefix "vbe")
	 (fullfeature (intern (format "%s:%s" prefix feature))))
    (unless (featurep fullfeature)
      (load filename)
      (unless (featurep fullfeature)
	(error "[vbe:] Required feature `%s' was not found."
	       fullfeature)))))

;; The second way is to put functionalities depending on some other
;; file into a file `somelibrary.conf.el` which will be loaded when
;; `somelibrary` is loaded. The system is a bit smart and if the
;; library has hiphens in its name, it will also search into
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
(add-hook 'after-load-functions 'vbe:after-load)

(defun vbe:run-directory (name)
  "Return a directory for runtime files. Create it if it does not exist."
  (let ((dir (expand-file-name (format "run/%s" name)
			       user-emacs-directory)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

;; Initialize el-get
(setq el-get-dir (expand-file-name "vendor" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "el-get" el-get-dir))
(require 'el-get)

(vbe:require 'appearance)		; appearance/display related stuff
(vbe:require 'behaviour)		; behavioral stuff
(vbe:require 'custom)			; custom variables
(vbe:require 'gnus)			; Ma Gnus

(unless (string= (user-login-name) "root")
  (require 'server)
  (when (or (not server-process)
            (not (eq (process-status server-process)
                     'listen)))
    (unless (server-running-p server-name)
      (server-start))))
