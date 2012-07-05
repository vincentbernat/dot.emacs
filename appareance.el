;; Customize Emacs appareance

(menu-bar-mode -1)			; No menu
(tool-bar-mode -1)			; No toolbar
(scroll-bar-mode -1)			; No scrollbar
(blink-cursor-mode -1)			; No blinking cursor
(show-paren-mode t)			; Display matching parenthesis
(setq inhibit-splash-screen t)		; No splash screen

;; Font stuff
(defvar vbe/default-font "DejaVu Sans Mono-10")
(defun vbe/set-font (&optional frame)
  "Change default font for the given FRAME."
  (when frame
    (select-frame frame))
  (when window-system
    (set-face-attribute 'default nil :font vbe/default-font)))
(vbe/set-font)
(add-hook 'after-make-frame-functions 'vbe/set-font)

;; Use naquadah theme
(vbe/add-package (list :name "naquadah-theme"
		       :init '(vbe/custom-theme)))
(defun vbe/custom-theme ()
  "Custom theme."
  (add-to-list ' custom-theme-load-path (car (el-get-load-path "naquadah-theme")))
  (load-theme 'naquadah t)
  ; We could use `custom-theme-set-faces' but we prefer to use
  ; `set-face-attribute' since we can redefine only part of a face.
  (dolist (face `(mode-line minibuffer-prompt))
    (set-face-attribute face nil :font "DejaVu Sans-10"))
  (vbe/gnus/custom-theme)
  (eval-after-load "gnus"
    '(add-hook 'gnus-started-hook 'vbe/gnus/custom-theme)))

(defun vbe/gnus/custom-theme ()
  "Custom theme, for Gnus"
  (set-face-attribute 'gnus-summary-selected-face nil :underline nil)
  (dolist (face `(content from subject name))
    (set-face-attribute (intern (format "gnus-header-%s-face" face))
			nil :font "DejaVu Sans-10")))

(provide 'vbe/appareance)
