;; Check we are using Emacs 24
(when (/= emacs-major-version 24)
  (error "Only Emacs 24 is supported. You seem to use Emacs %d"
	 emacs-major-version))

(defun vbe/require (feature)
  "Load FEATURE if not loaded (with added prefix).
The appropriate prefix is added to the provided feature but the
name is searched without prefix. For example, if FEATURE is
\"el-get\", the loaded feature will be \"vbe/el-get\" and it will
be searched in \"el-get.el\" in the user Emacs directory."
  (let* ((prefix "vbe")
	 (filename (expand-file-name (concat prefix "-" (symbol-name feature))
				     user-emacs-directory))
	 (fullfeature (intern (format "%s/%s" prefix feature))))
    (unless (featurep fullfeature)
      (load filename)
      (unless (featurep fullfeature)
	(error "[vbe/] Required feature `%s' was not found."
	       fullfeature)))))

(vbe/require 'utils)

;; Various directories
(setq auto-save-list-file-prefix (format "%s/saves-"
					 (vbe:run-directory "auto-save"))
      custom-file (expand-file-name "custom.el" user-emacs-directory))

;; Initialize el-get
(setq el-get-dir (expand-file-name "vendor" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "el-get" el-get-dir))
(require 'el-get)

;; Appearance
(menu-bar-mode -1)			; No menu
(tool-bar-mode -1)			; No toolbar
(scroll-bar-mode -1)			; No scrollbar
(blink-cursor-mode -1)			; No blinking cursor
(show-paren-mode t)			; Display matching parenthesis ; C-M-n and C-M-p
(setq inhibit-splash-screen t)		; No splash screen
(line-number-mode 1)			; show line number
(column-number-mode 1)			; show column number
(global-hl-line-mode 1)			; highlight current line
(mouse-avoidance-mode 'jump)		; move the mouse away
(set-default 'indicate-buffer-boundaries '((up . nil) (down . nil) (t . left)))
(require 'naquadah-theme)

;; Behaviour
(setq mouse-yank-at-point t		; Yank where the point currently is
      x-select-enable-primary t         ; Yank use the primary selection if available
      mouse-1-click-follows-link nil	; Don't follow links with left click
      make-backup-files nil)		; Don't make backups, not used in ages
(fset 'yes-or-no-p 'y-or-n-p) ; Always use y/n prompt
(setq use-dialog-box nil)     ; No dialog box
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(setq-default indent-tabs-mode nil)   ; don't use tabs
(setq next-screen-context-lines 5     ; Keep more lines when scrolling
      x-stretch-cursor t)    ; stretch cursor to the width of the char
(projectile-global-mode 1)
(require 'auto-complete)

;; Bindings
(vbe/require 'bindings)

;; Automode
(add-to-list 'auto-mode-alist '("-MIB$" . snmpv2-mode))
(add-to-list 'auto-mode-alist '("\\.less$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))

;; Programming
(defun vbe:customize-programming-language-mode ()
  (whitespace-mode 1)
  (fci-mode 0)
  (hs-minor-mode 1)
  (highlight-parentheses-mode 1))
(add-hook 'prog-mode-hook ; This is the mode perl, makefile,
                          ; lisp-mode, scheme-mode, emacs-lisp-mode,
                          ; sh-mode, java-mode, c-mode, c++-mode,
                          ; python-mode inherits from.
          'vbe:customize-programming-language-mode)

;; Other stuff we need
(require 'point-stack)
(require 'uniquify)
(require 'ido)
(require 'saveplace)
(require 'multiple-cursors)

;; Server
(unless (string= (user-login-name) "root")
  (require 'server)
  (when (or (not server-process)
            (not (eq (process-status server-process)
                     'listen)))
    (unless (server-running-p server-name)
      (server-start))))
