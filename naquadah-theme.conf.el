;; Customize the font

(defvar vbe:default-font "DejaVu Sans Mono-10"
  "Default font.")

(defun vbe:set-font (&optional frame)
  "Change default font for the given FRAME."
  (when frame
    (select-frame frame))
  (when window-system
    (set-face-attribute 'default nil :font vbe:default-font)
    (dolist (face `(mode-line mode-line-inactive minibuffer-prompt))
      (set-face-attribute face nil :font "DejaVu Sans-10"))))

(require 'powerline)

(vbe:set-font)
(add-hook 'after-make-frame-functions 'vbe:set-font)
