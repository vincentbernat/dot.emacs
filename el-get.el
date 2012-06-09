; el-get configuration

; Initialize el-get
(add-to-list 'load-path (expand-file-name "el-get/el-get" user-emacs-directory))
(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

; Package registration
(setq vbe/packages ())
(defun vbe/add-package (package)
  "Add PACKAGE to the list of package to install.
PACKAGE is a property list. It should contain :name and may
contain :init (executed when the package is initialized."
  (add-to-list 'vbe/packages package))
(defun vbe/init-package (package)
  "Init the package PACKAGE."
  (dolist (p vbe/packages)
    (when (string= (plist-get p :name) package)
      (let ((init (plist-get p :init)))
	(when init
	  (message "[vbe/] calling init hook for package %s"
		   package)
	  (eval init))))))
(add-hook 'el-get-post-init-hooks 'vbe/init-package)

; Package installation
(defun vbe/sync-packages ()
  "Install missing packages using el-get."
  (interactive)
  (el-get 'sync (mapcar '(lambda (p) (plist-get p :name)) vbe/packages)))

(provide 'vbe/el-get)
