;; Check we are using Emacs 24
(when (/= emacs-major-version 24)
  (error "Only Emacs 24 is supported. You seem to use Emacs %d"
	 emacs-major-version))

(defun vbe/require (feature)
  "Load FEATURE if not loaded (with added prefix).
The appropriate prefix is added to the provided feature but the
name is searched without prefix. For example, if FEATURE is
\"el-get\", the loaded feature will be \"vbe/el-get\" and it will
be searched in \"el-get.el\" in the user Emacs directory."
  (let* ((filename (expand-file-name (symbol-name feature)
				     user-emacs-directory))
	 (prefix "vbe")
	 (fullfeature (intern (format "%s/%s" prefix feature))))
    (unless (featurep fullfeature)
      (load filename)
      (unless (featurep fullfeature)
	(error "[vbe/] Required feature `%s' was not found."
	       fullfeature)))))

(defun vbe/run-directory (name)
  "Return a directory for runtime files. Create it if it does not exist."
  (let ((dir (expand-file-name (format "run/%s" name)
			       user-emacs-directory)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

(vbe/require 'el-get)			; el-get initialization
(vbe/require 'appearance)		; appearance/display related stuff
(vbe/require 'behaviour)		; behavioral stuff
(vbe/require 'bindings)			; keyboard bindings
(vbe/require 'buffers)			; buffer stuff
(vbe/require 'server)			; server mode
(vbe/require 'custom)			; custom variables
(vbe/require 'programming)		; programming mode
(vbe/require 'magit)			; it's magit!
(vbe/require 'debian)			; Debian related stuff
(vbe/require 'gnus)			; gnus

(vbe/sync-packages)
