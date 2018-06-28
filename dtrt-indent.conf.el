;;; Code:

;; Remove the dtrt indent stuff from modeline, no need for it.
(setq global-mode-string (remove 'dtrt-indent-mode-line-info global-mode-string))

;; Add more offset variables
(add-to-list 'dtrt-indent-hook-mapping-list '(groovy-mode c/c++/java groovy-indent-offset))

;;; dtrt-indent.conf.el ends here
