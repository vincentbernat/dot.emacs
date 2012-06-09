; Customize Emacs appareance

(tool-bar-mode -1)			; No toolbar
(scroll-bar-mode -1)			; No scrollbar
(blink-cursor-mode -1)			; No blinking cursor
(show-paren-mode t)			; Display matching parenthesis
(setq inhibit-splash-screen t)		; No splash screen

; Font stuff
(defvar vbe/default-font "DejaVu Sans Mono-10")
(defun vbe/set-font (&optional frame)
  "Change default font for the given FRAME."
  (when frame
    (select-frame frame))
  (when window-system
    (set-face-attribute 'default nil :font vbe/default-font)))
(vbe/set-font)
(add-hook 'after-make-frame-functions 'vbe/set-font)

(provide 'vbe/appareance)
