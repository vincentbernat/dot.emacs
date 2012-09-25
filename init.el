;; Check we are using Emacs 24
(when (/= emacs-major-version 24)
  (error "Only Emacs 24 is supported. You seem to use Emacs %d"
	 emacs-major-version))

;; The main way to load a file is to put functionalities depending on
;; some other file into a file `somelibrary.conf.el` which will be
;; loaded when `somelibrary` is loaded. The system is a bit smart and
;; if the library has hiphens in its name, it will also search into
;; subdirectories. This feature is inspired from Julien Danjou's emacs
;; configuration.
(defun vbe:after-load (file)
  "Execute appropriate hook after loading FILE.
The hooks are looked in FILE.conf.el in user emacs directory or
in a subdirectory if we can find the appropriate file by
substituting hyphens for slashes."
  ;; We got an absolute filename. Let's find the basename.
  (let* ((filename (file-name-nondirectory file))
	 (name (substring filename 0
			  (string-match "\\.elc?\\>" filename)))
	 (components (split-string name "-"))
	 (directory user-emacs-directory))

    ;; Do we have files for this?
    (while components
      (let ((target (expand-file-name
		     (format "%s.conf.el"
			     (mapconcat 'identity components "-")) directory)))
	(when (file-readable-p target)
	  (load target))
	(setq directory (expand-file-name (car components) directory)
	      components (cdr components))))))

;; Load current features
(mapc '(lambda (f) (vbe:after-load (symbol-name f))) features)
;; Load future features
(add-hook 'after-load-functions 'vbe:after-load)

(defun vbe:run-directory (name)
  "Return a directory for runtime files. Create it if it does not exist."
  (let ((dir (expand-file-name (format "run/%s" name)
			       user-emacs-directory)))
    (unless (file-directory-p dir)
      (make-directory dir t))
    dir))

(defun vbe:at (where)
  "Return `t' if the current profile is WHERE."
  (string= (cond ((string-match (concat (regexp-quote ".p.fti.net") "$")
			   (system-name))
		  "orange")
		 (t "unknown"))
	   (cond ((symbolp where) (symbol-name where))
		 ((stringp where) where)
		 (t ""))))

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
      mouse-1-click-follows-link nil	; Don't follow links with left click
      make-backup-files nil)		; Don't make backups, not used in ages
(fset 'yes-or-no-p 'y-or-n-p) ; Always use y/n prompt
(setq use-dialog-box nil)     ; No dialog box
(global-auto-revert-mode 1)   ; Auto revert (when no pending changes)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(setq-default indent-tabs-mode nil)   ; don't use tabs
(setq next-screen-context-lines 5     ; Keep more lines when scrolling
      x-stretch-cursor t)    ; stretch cursor to the width of the char

;; Bindings
(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "<delete>") 'delete-char)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x g") 'magit-status)

;; Automode
(add-to-list 'auto-mode-alist '("-MIB$" . snmpv2-mode))
(add-to-list 'auto-mode-alist '("\\.less$" . css-mode))

;; Programming
(defun vbe:customize-programming-language-mode ()
  (setq show-trailing-whitespace t)
  (highlight-parentheses-mode 1))
(add-hook 'prog-mode-hook ; This is the mode perl, makefile,
                          ; lisp-mode, scheme-mode, emacs-lisp-mode,
                          ; sh-mode, java-mode, c-mode, c++-mode,
                          ; python-mode inherits from.
          'vbe:customize-programming-language-mode)

;; Other stuff we need
(require 'autopair)
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
