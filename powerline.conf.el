;;; Powerline settings
;;; Code:

(require 'spaceline-config)
(setq powerline-default-separator 'zigzag
      powerline-display-buffer-size t
      powerline-display-hud nil
      spaceline-hud-p nil
      spaceline-buffer-encoding-abbrev-p nil)
(spaceline-emacs-theme)

;; Handle DPI change
(defun vbe/after-font-setting-change-default-font (&optional display-or-frame set-font)
  (setq powerline-height (round (* 1.3 (frame-char-height))))
  (pl/reset-cache))
(vbe/after-font-setting-change-default-font)
(advice-add 'font-setting-change-default-font
            :after #'vbe/after-font-setting-change-default-font)

;; Don't change the modeline on focus out
(remove-hook 'focus-out-hook 'powerline-unset-selected-window)

;;; powerline.conf.el ends here
