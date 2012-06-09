(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "<delete>") 'delete-char)
(global-set-key (kbd "M-/") 'hippie-expand)

;; Save current point in a stack and allow navigation
(vbe/add-package (list :name "point-stack"
		       :init '(progn
				 (global-set-key (kbd "s-.") 'point-stack-push)
				 (global-set-key (kbd "s-,") 'point-stack-pop)
				 (global-set-key (kbd "s-/") 'point-stack-forward-stack-pop))))

;; autopair (more sophisticated than Emacs 24 electric pair mode)
(vbe/add-package (list :name "autopair"
		       :init '(progn
				(autopair-global-mode)
				(setq autopair-autowrap t))))

(provide 'vbe/bindings)
