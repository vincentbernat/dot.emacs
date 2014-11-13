;;; Code:

(require 'dash)

(defun vbe:go-mode-setup-gopath ()
  "Setup GOPATH to the root of the project + build."
  (let* ((initial-gopath (split-string (getenv "GOPATH") ":"))
         (root (projectile-project-root))
         (additionals (list root (format "%sbuild" (projectile-project-root))))
         (new-gopath (append (--filter (file-accessible-directory-p it) additionals)
                             initial-gopath))
         (gopath (mapconcat 'identity new-gopath ":")))
    (make-local-variable 'process-environment)
    (setq process-environment (cons (format "GOPATH=%s" gopath) process-environment))))

(add-hook 'go-mode-hook 'vbe:go-mode-setup-gopath)

;;; go-mode.conf.el ends here
