(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "<delete>") 'delete-char)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-c C-SPC") 'ace-jump-mode)
(global-unset-key "\C-x\C-c")           ; Too easy to hit by
                                        ; accident. Never used. Use
                                        ; kill-emacs directly
(when window-system
  (global-unset-key "\C-z"))
