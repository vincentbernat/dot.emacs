(setq mouse-yank-at-point t		; Yank where the point currently is
      mouse-1-click-follows-link nil	; Don't follow links with left click
      make-backup-files nil)		; Don't make backups, not used in ages

;; Always use y/n prompt
(fset 'yes-or-no-p 'y-or-n-p)

(provide 'vbe/behaviour)
