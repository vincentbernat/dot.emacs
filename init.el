;;; Code:
;; Check we are using Emacs 24
(when (/= emacs-major-version 24)
  (error "Only Emacs 24 is supported. You seem to use Emacs %d"
	 emacs-major-version))

;; Ensure we don't load outdated bytecodes. This needs to be done early.
(add-to-list 'load-path (expand-file-name "vendor/packed" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "vendor/auto-compile" user-emacs-directory))
(setq load-prefer-newer t)
(require 'auto-compile)
(auto-compile-on-load-mode 1)

;; I don't like to use require to load my own configuration files
;; because it is not possible to limit its use to a given directory. I
;; don't want to mess with the load path and I don't want to load
;; anything else than configuration file. Hence this custom function.
(defun vbe:require (feature)
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

(vbe:require 'utils)

;; Various directories
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

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
(global-pretty-mode t)                  ; pretty minor mode globally enabled
(electric-indent-mode 1)                ; auto-indent (disabled for some modes below)
(setq make-pointer-invisible t)		; hide the mouse while typing
(setq font-lock-maximum-decoration 2)   ; faster font-lock-mode
(set-default 'indicate-buffer-boundaries '((up . nil) (down . nil) (t . left)))
(require 'naquadah-theme)

;; Behaviour
(setq mouse-yank-at-point t		; Yank where the point currently is
      x-select-enable-primary t         ; Yank use the primary selection if available
      x-select-enable-clipboard t       ; Yank use the clipboard if available
      save-interprogram-paste-before-kill t ; Put clipboard/selection into kill ring
      x-selection-timeout 10                ; Workaround. See https://debbugs.gnu.org/16737
      mouse-1-click-follows-link nil)	; Don't follow links with left click
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
(vbe:require 'bindings)

;; Automode
(add-to-list 'auto-mode-alist '("-MIB$" . snmpv2-mode))
(add-to-list 'auto-mode-alist '("\\.less$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("README.md$" . gfm-mode))
(add-to-list 'auto-mode-alist '("/Rakefile" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.html?$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))
(add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))

;; Programming
(defun vbe:customize-programming-language-mode ()
  (whitespace-mode 1)
  (highlight-parentheses-mode 1))
(add-hook 'prog-mode-hook ; This is the mode perl, makefile,
                          ; lisp-mode, scheme-mode, emacs-lisp-mode,
                          ; sh-mode, java-mode, c-mode, c++-mode,
                          ; python-mode inherits from.
  'vbe:customize-programming-language-mode)

;; Disable electric indent mode on some mode
(defun vbe:no-electric-indent-mode ()
  (set (make-local-variable 'electric-indent-mode) nil))
(let ((modes '(markdown-mode rst-mode yaml-mode
               ledger-mode message-mode
               fundamental-mode)))
  (dolist (mode modes)
    (add-hook
      (intern (concat (symbol-name mode) "-hook"))
      'vbe:no-electric-indent-mode)))

;; magit warning
(setq magit-last-seen-setup-instructions "1.4.0")

;; Other stuff we need
(require 'server)
(require 'point-stack)
(require 'uniquify)
(require 'ido)
(require 'saveplace)
(require 'expand-region)
(require 'multiple-cursors)
(require 'midnight)               ; clean up buffers from time to time
(global-flycheck-mode 1)
(autoload 'vbe:edit-region-in-another-buffer
  (expand-file-name "vbe-edit-region"
                    user-emacs-directory)
  "Edit region in another buffer" t nil)
