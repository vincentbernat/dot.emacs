;;; Code:

(require 'dash)

(defun vbe:go-mode-setup-gopath ()
  "Setup GOPATH from guessed possibilities."
  (let ((gopath (or (go-guess-gopath (current-buffer))
                    (go-original-gopath))))
    (make-local-variable 'process-environment)
    (setq process-environment (cons (format "GOPATH=%s" gopath) process-environment))))

(add-hook 'go-mode-hook 'vbe:go-mode-setup-gopath)
(add-hook 'before-save-hook 'gofmt-before-save)

;;; go-mode.conf.el ends here
