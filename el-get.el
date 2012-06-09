; el-get configuration

; Initialize el-get
(add-to-list 'load-path (expand-file-name "el-get/el-get" user-emacs-directory))
(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (end-of-buffer)
     (eval-print-last-sexp))))

(setq vbe/packages ())
(defun vbe/add-package (package &optional init)
  "Add PACKAGE to the list of package to install.
Execute INIT after loading the package."
  (add-to-list 'vbe/packages package)
  (when init
    (eval-after-load package init)))

(defun vbe/sync-packages ()
  "Install missing packages using el-get."
  (interactive)
  (el-get 'sync vbe/packages))

(add-hook 'after-init-hook 'vbe/sync-packages)

(provide 'vbe/el-get)
