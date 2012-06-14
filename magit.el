(vbe/add-package '(:name "magit" :init 'vbe/magit-init)) ; git with magit

(defun vbe/magit-init ()
  "Initialize magit."
  ;; Global binding for magit
  (global-set-key (kbd "C-x g") 'magit-status)
  ;; Where is located the code?
  (setq magit-repo-dirs (list (expand-file-name "~/code"))
	magit-repo-dirs-depth 2))


(provide 'vbe/magit)
