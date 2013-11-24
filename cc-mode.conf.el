;; C stuff
(setq c-default-style "linux"           ; default style is "linux
      c-basic-offset 4)			; with a 4 spaces indent

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

(defun vbe:cc-mode-hook ()
  (unless (and
           (fboundp 'tramp-tramp-file-p)
           (tramp-tramp-file-p (buffer-file-name)))
    (ggtags-mode 1)))
(add-hook 'c-mode-common-hook 'vbe:cc-mode-hook)
