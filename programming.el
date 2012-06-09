; cscope stuff
(vbe/add-package (list :name "xcscope"
		       :init '(add-hook 'c-mode-common-hook
					'(lambda()
					   (cscope-minor-mode)))))

; Various packages
(vbe/add-package '(:name "dtrt-indent")) ; autodetect indentation
(vbe/add-package '(:name "coffee-mode")) ; coffee
(vbe/add-package '(:name "auctex"))	 ; latex
(vbe/add-package '(:name
		   "markdown-mode"))	 ; markdown

(provide 'vbe/programming)
