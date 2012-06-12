;; cscope stuff
(vbe/add-package (list :name "xcscope"
		       :init '(add-hook 'c-mode-common-hook
					'(lambda()
					   (cscope-minor-mode)))))

;; Various packages
(vbe/add-package '(:name "dtrt-indent")) ; autodetect indentation
(vbe/add-package (list :name "coffee-mode"
		       :init '(setq coffee-tab-width 2))) ; coffee
(vbe/add-package '(:name "auctex"))	 ; latex
(vbe/add-package '(:name
		   "markdown-mode"))	 ; markdown

;; SNMP
(add-to-list 'auto-mode-alist '("-MIB$" . snmpv2-mode))

;; C stuff
(setq c-default-style "linux"           ; default style is "linux"
      c-basic-offset 4                  ; with a 4 spaces indent
      indent-tabs-mode nil)             ; don't use tabs

(provide 'vbe/programming)
