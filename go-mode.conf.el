;;; Code:

(require 'dash)

(defun vbe:go-mode-setup-gopath ()
  "Setup GOPATH to the root of the project + build."
  (let* ((initial-gopath (split-string (getenv "GOPATH") ":"))
         (root (if (projectile-project-p)
                   (projectile-project-root)
                 default-directory))
         (altroot (locate-dominating-file default-directory "Makefile"))
         (additionals (list root
                            (format "%sbuild" root)
                            (and altroot (expand-file-name altroot))
                            (and altroot (format "%sbuild" (expand-file-name altroot)))))
         (new-gopath (append (-filter 'file-accessible-directory-p (-non-nil additionals))
                             initial-gopath))
         (gopath (mapconcat 'identity new-gopath ":")))
    (make-local-variable 'process-environment)
    (setq process-environment (cons (format "GOPATH=%s" gopath) process-environment))))

(add-hook 'go-mode-hook 'vbe:go-mode-setup-gopath)

;;; go-mode.conf.el ends here