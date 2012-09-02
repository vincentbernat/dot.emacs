(setq mouse-yank-at-point t		; Yank where the point currently is
      mouse-1-click-follows-link nil	; Don't follow links with left click
      make-backup-files nil)		; Don't make backups, not used in ages

(fset 'yes-or-no-p 'y-or-n-p) ; Always use y/n prompt
(setq use-dialog-box nil)     ; No dialog box
(global-auto-revert-mode 1)   ; Auto revert (when no pending changes)

;; Various runtime directories
(setq url-cache-directory (vbe:run-directory "url")
      auto-save-list-file-prefix (format "%s/saves-"
					 (vbe:run-directory "auto-save")))

;; Enable some disabled commands
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;; Bindings
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "<delete>") 'delete-char)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x g") 'magit-status)

;; Automode
(add-to-list 'auto-mode-alist '("-MIB$" . snmpv2-mode))

;; Other stuff we need
(require 'point-stack)
(require 'uniquify)
(require 'ido)
(require 'saveplace)
(setq-default save-place t)


(provide 'vbe:behaviour)
