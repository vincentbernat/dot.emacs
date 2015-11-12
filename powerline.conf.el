;;; Powerline settings
;;; Code:

(powerline-default-theme)
(setq powerline-default-separator 'arrow)

;; Handle DPI change
(defun vbe/after-font-setting-change-default-font (display-or-frame set-font)
    (pl/reset-cache))
(advice-add 'font-setting-change-default-font
            :after #'vbe/after-font-setting-change-default-font)

;;; powerline.conf.el ends here
