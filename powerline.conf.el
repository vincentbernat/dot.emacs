;;; Powerline settings
;;; Code:

(require 'spaceline-config)
(setq powerline-default-separator 'wave
      powerline-display-buffer-size t
      powerline-display-hud nil
      powerline-height (round (* 1.3 (frame-char-height)))
      spaceline-buffer-encoding-abbrev-p nil)
(spaceline-emacs-theme)

;; Handle DPI change
(defun vbe/after-font-setting-change-default-font (display-or-frame set-font)
    (pl/reset-cache))
(advice-add 'font-setting-change-default-font
            :after #'vbe/after-font-setting-change-default-font)

;;; powerline.conf.el ends here
