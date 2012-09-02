;; cscope stuff
(vbe:add-package (list :name "xcscope"
		       :init '(add-hook 'c-mode-common-hook
					'(lambda()
					   (cscope-minor-mode)))))

;; Various packages
(vbe:add-package '(:name "dtrt-indent")) ; autodetect indentation
(vbe:add-package (list :name "coffee-mode"
		       :init '(setq coffee-tab-width 2))) ; coffee
(vbe:add-package '(:name "auctex"))	 ; latex
(vbe:add-package '(:name "php-mode"))
(vbe:add-package '(:name "lua-mode"))
(vbe:add-package '(:name
		   "markdown-mode"))	 ; markdown

;; SNMP
(add-to-list 'auto-mode-alist '("-MIB$" . snmpv2-mode))

;; C stuff
(setq c-default-style "linux"           ; default style is "linux"
      c-basic-offset 4                  ; with a 4 spaces indent
      x-stretch-cursor t                ; stretch cursor to the width of the char (for example tab)
      indent-tabs-mode nil)             ; don't use tabs
(add-hook 'c-mode-common-hook '(lambda nil (setq show-trailing-whitespace t)))
(which-function-mode 1)			; display current function in modeline

;; More styles. To debug, use C-c C-s.
;;  - `+` means `c-basic-offset` times 1.
;;  - `*` means `c-basic-offset` times 0.5.
(c-add-style
 "openbsd"                     ; Should be compatible with FreeBSD too
 '("bsd"
   (c-basic-offset . 8)
   (c-tab-width . 8)
   (fill-column . 80)
   (indent-tabs-mode . t)
   (c-offsets-alist . ((defun-block-intro     . +)
                       (statement-block-intro . +)
                       (statement-case-intro  . +)
                       (statement-cont        . *)
                       (substatement-open     . *)
                       (substatement          . +)
                       (arglist-cont-nonempty . *)
                       (inclass               . +)
		       (inextern-lang         . 0)
                       (knr-argdecl-intro     . +)))))

(provide 'vbe:programming)
