;; Customize Emacs appareance
(menu-bar-mode -1)			; No menu
(tool-bar-mode -1)			; No toolbar
(scroll-bar-mode -1)			; No scrollbar
(blink-cursor-mode -1)			; No blinking cursor
(show-paren-mode t)			; Display matching parenthesis ; C-M-n and C-M-p
(setq inhibit-splash-screen t)		; No splash screen
(line-number-mode 1)			; show line number
(column-number-mode 1)			; show column number
(global-hl-line-mode 1)			; highlight current line
(mouse-avoidance-mode 'animate)		; nove the mouse away

(require 'naquadah-theme)
(defun vbe:gnus/custom-theme ()
  "Custom theme, for Gnus"
  (set-face-attribute 'gnus-summary-selected-face nil :underline nil)
  (when (eq 'x (window-system))
    (dolist (face `(content from subject name))
      (set-face-attribute (intern (format "gnus-header-%s-face" face))
			  nil :font "DejaVu Sans-10"))))

(provide 'vbe:appearance)
