; cscope stuff
(vbe/add-package "xcscope"
		 '(add-hook 'c-mode-common-hook
			    '(lambda()
			       (require 'xcscope)
			       (cscope-minor-mode))))

; Various packages
(vbe/add-package "dtrt-indent")		; autodetect indentation
(vbe/add-package "coffee-mode")		; coffee
(vbe/add-package "auctex")		; latex
(vbe/add-package "markdown-mode")	; markdown

(provide 'vbe/programming)
