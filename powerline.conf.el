;;; Powerline settings
;;; Code:

(powerline-default-theme)
(setq powerline-default-separator 'arrow)
(add-hook 'after-setting-font-hook #'pl/reset-cache)

;;; powerline.conf.el ends here
