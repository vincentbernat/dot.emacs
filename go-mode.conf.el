;;; Code:

(require 'dash)
(require 'cl)

(defun vbe:go-mode-setup-gopath ()
  "Setup GOPATH from guessed possibilities."
  (let ((gopath (or (go-guess-gopath (current-buffer))
                    (go-original-gopath))))
    (make-local-variable 'process-environment)
    (setq process-environment (cons (format "GOPATH=%s" gopath) process-environment))))

;; I am allergic to the GOPATH concept. I use kludge to work around them.
(defun vbe:custom-gopath ()
  "Guess gopath if we have a GOPATH along with a `vendor'
directory. In this case, GOPATH is both of them. This usually
requires to have a symlink in vendor with `src' pointing back to
`vendor'."
  (cl-letf ((add (gopath)
                 (let* ((d (locate-dominating-file buffer-file-name gopath))
                        (src (concat d (file-name-as-directory "vendor"))))
                   (if (and d
                            (file-exists-p src))
                       (list (concat d (file-name-as-directory gopath)) src)))))
    (or (add ".gopath") (add ".gopath~"))))
(add-to-list 'go-guess-gopath-functions 'vbe:custom-gopath)

(add-hook 'go-mode-hook 'vbe:go-mode-setup-gopath)
(add-hook 'before-save-hook 'gofmt-before-save)

;;; go-mode.conf.el ends here
