;; Customize the font

(defvar vbe:default-font "DejaVu Sans Mono-11"
  "Default font.")

(defun vbe:set-font (&optional frame)
  "Change default font for the given FRAME."
  (when frame
    (select-frame frame))
  (when window-system
    (set-face-attribute 'default nil :font vbe:default-font)
    (dolist (face `(mode-line mode-line-inactive minibuffer-prompt))
      (set-face-attribute face nil :font "DejaVu Sans-10"))
    (set-fontset-font
     t 'symbol (font-spec :family "Symbola") nil 'prepend)))

(vbe:set-font)
(add-hook 'after-make-frame-functions 'vbe:set-font)

;; Alter naquadah a bit
(naquadah-theme-set-faces
 'naquadah
 '(gnus-summary-selected (:background chameleon-3 :foreground black))
 '(gnus-summary-normal-ticked (:background scarlet-red-3))
 '(gnus-summary-normal-read (:foreground aluminium-3 :italic t))
 '(gnus-summary-high-unread (:inherit gnus-summary-normal-unread
                                      :background sky-blue-2))
 '(gnus-summary-high-read (:inherit gnus-summary-normal-read
                                    :background sky-blue-3))
 '(comint-highlight-prompt (:foreground orange-2 :weight bold)))
