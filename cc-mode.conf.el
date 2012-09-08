;; C stuff
(setq c-default-style "linux"           ; default style is "linux
      c-basic-offset 4)			; with a 4 spaces indent
(add-hook 'c-mode-common-hook
	  '(lambda nil (setq show-trailing-whitespace t)))

(which-function-mode 1)			; display current function in modeline
(require 'xcscope)                      ; cscope bindings
(require 'dtrt-indent)                  ; autodetect indentation

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
