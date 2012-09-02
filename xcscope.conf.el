;; Plug cscope into C-like major modes
(add-hook 'c-mode-common-hook
	  '(lambda()
	     (cscope-minor-mode)))
